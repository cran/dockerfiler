
#' @importFrom memoise memoise
#' @noRd
pkg_sysreqs_mem <- memoise::memoise(
    pak::pkg_sysreqs
)


#' Create a Dockerfile from an `renv.lock` file
#'
#' @param lockfile Path to an `renv.lock` file to use as an input..
#' @param FROM Docker image to start FROM Default is FROM rocker/r-base
#' @param AS The AS of the Dockerfile. Default it `NULL`.
#' @param distro - deprecated - only debian/ubuntu based images are supported
#' @param sysreqs boolean. If `TRUE`, the Dockerfile will contain sysreq installation.
#' @param expand boolean. If `TRUE` each system requirement will have its own `RUN` line.
#' @param repos character. The URL(s) of the repositories to use for `options("repos")`.
#' @param extra_sysreqs character vector. Extra debian system requirements.
#'    Will be installed with apt-get install.
#' @param renv_version character. The renv version to use in the generated Dockerfile. By default, it is set to the version specified in the `renv.lock` file.
#'   If the `renv.lock` file does not specify a renv version,
#'   the version of renv bundled with dockerfiler,
#'   specifically `r dockerfiler::renv$initialize();toString(dockerfiler::renv$the$metadata$version)`, will be used.
#'   If you set it to `NULL`, the latest available version of renv will be used.
#' @param use_pak boolean. If `TRUE` use pak to deal with dependencies  during `renv::restore()`. FALSE by default
#' @param user Name of the user to specify in the Dockerfile with the USER instruction. Default is `NULL`, in which case the user from the FROM image is used.
#' @param dependencies What kinds of dependencies to install. Most commonly
#'   one of the following values:
#'   - `NA`: only required (hard) dependencies,
#'   - `TRUE`: required dependencies plus optional and development
#'     dependencies,
#'   - `FALSE`: do not install any dependencies. (You might end up with a
#'     non-working package, and/or the installation might fail.)
#' @param sysreqs_platform System requirements platform.`ubuntu` by default. If `NULL`, then the  current platform is used. Can be : "ubuntu-22.04" if needed to fit with the `FROM` Operating System. Only debian or ubuntu based images are supported
#' @importFrom utils getFromNamespace
#' @return A R6 object of class `Dockerfile`.
#' @details
#'
#' System requirements for packages are provided
#' through RStudio Package Manager via the pak
#' package. The install commands provided from pak
#' are added as `RUN` directives within the `Dockerfile`.
#'
#' The R version is taken from the `renv.lock` file.
#' Packages are installed using `renv::restore()` which ensures
#' that the proper package version and source is used when installed.
#'
#' @importFrom attempt map_try_catch
#' @importFrom glue glue
#' @importFrom pak pkg_sysreqs
#' @importFrom purrr keep_at pluck

#' @export
#' @examples
#' \dontrun{
#' dock <- dock_from_renv("renv.lock", distro = "xenial")
#' dock$write("Dockerfile")
#' }
dock_from_renv <- function(
  lockfile = "renv.lock",
  distro = NULL,
  FROM = "rocker/r-base",
  AS = NULL,
  sysreqs = TRUE,
  repos = c(CRAN = "https://cran.rstudio.com/"),
  expand = FALSE,
  extra_sysreqs = NULL,
  use_pak = FALSE,
  user = NULL,
  dependencies = NA,
  sysreqs_platform = "ubuntu",
  renv_version
) {
  try(dockerfiler::renv$initialize(),silent=TRUE)
  lock <- dockerfiler::renv$lockfile_read(file = lockfile) # using vendored renv
  # https://rstudio.github.io/renv/reference/vendor.html?q=vendor#null

  # start the dockerfile
  R_major_minor <- lock$R$Version
  dock <- Dockerfile$new(
    FROM = gen_base_image(
      r_version = R_major_minor,
      FROM = FROM
    ),
    AS = AS
  )
  if (!is.null(user)) {
    dock$USER(user)
  }
  # get renv version

  if (missing(renv_version)) {
    if (!is.null(lock$Packages$renv$Version)) {
      renv_version <- lock$Packages$renv$Version
    } else {
      renv_version <-  dockerfiler::renv$the$metadata$version
    }
  }

  message("renv version = ",
          ifelse(!is.null(renv_version),renv_version,"the must up to date in the repos")
          )


  distro_args <- list(sysreqs_platform = sysreqs_platform)

  install_cmd <- "apt-get install -y"
  update_cmd <-"apt-get update -y"
  clean_cmd <- "rm -rf /var/lib/apt/lists/*"

  pkgs <- names(lock$Packages)

  if (sysreqs) {

    # please wait during system requirement calculation
    cat_bullet(
      "Please wait while we compute system requirements...",
      bullet = "info",
      bullet_col = "green"
    )

    message(
      sprintf(
        "Fetching system dependencies for %s package(s) records.",
        length(pkgs)
      )
    )

    pkg_os <- lapply(
      pkgs,
      FUN = function(x) {
        c(
          list(pkg = x,
               dependencies = dependencies),
          distro_args
        )
      }
    )


    pkg_sysreqs <- unlist(attempt::map_try_catch(
      pkg_os,
      function(x) {
        keep_at(
          pluck(
            do.call(pkg_sysreqs_mem, x),
            "packages"
          ),
          "system_packages"
        )
      },
      .e = ~ character(0)
    ))





    pkg_installs <-
      lapply(
        X = unique(pkg_sysreqs),
        FUN = function(.x) {
          paste0(install_cmd, " ", .x)
        }
      )

    if (length(unlist(pkg_installs)) == 0) {
      cat_bullet(
        "No sysreqs required",
        bullet = "info",
        bullet_col = "green"
      )
    }

    cat_green_tick("Done") # TODO animated version ?
  } else {
    pkg_installs <- NULL
  }

  # extra_sysreqs




  if (length(extra_sysreqs) > 0) {
    extra <- paste(
      install_cmd,
      extra_sysreqs
    )
    pkg_installs <- unique(c(pkg_installs, extra))
  }





  # compact
  if (!expand) {
    # we compact sysreqs
    pkg_installs <- compact_sysreqs(
      pkg_installs,
      update_cmd = update_cmd,
      install_cmd = install_cmd,
      clean_cmd = clean_cmd
    )

  } else {
    dock$RUN(update_cmd)
  }

  do.call(dock$RUN, list(pkg_installs))

  if (expand) {
    dock$RUN(clean_cmd)
  }

  repos_as_character <- repos_as_character(repos)
  dock$RUN("mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/")

  dock$RUN(
    sprintf(
      "echo \"options(renv.config.pak.enabled = %s, repos = %s, download.file.method = 'libcurl', Ncpus = 4)\" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site",
      use_pak,
      repos_as_character
    )
  )


  if (!is.null(renv_version)){
    dock$RUN("R -e 'install.packages(\"remotes\")'")
      install_renv_string <- paste0(
        "R -e 'remotes::install_version(\"renv\", version = \"",
        renv_version,
        "\")'"
      )
      dock$RUN(install_renv_string)

  } else {
    dock$RUN("R -e 'install.packages(c(\"renv\",\"remotes\"))'")
  }

  dock$COPY(basename(lockfile), "renv.lock")
  dock$RUN("--mount=type=cache,id=renv-cache,target=/root/.cache/R/renv R -e 'renv::restore()'")
  dock
}

