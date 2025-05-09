
# A temporary directory
dir_build <- tempfile(pattern = "renv")
dir.create(dir_build)

# Create a lockfile
the_lockfile <- file.path(dir_build, "renv.lock")

custom_packages <- c(
  # attachment::att_from_description(),
  "cli",
  "glue", # "golem",
  "shiny",
  "stats",
  "utils",
  "testthat",
  "knitr"
)
try(dockerfiler::renv$initialize(),silent=TRUE)
if ( !testthat:::on_cran()){
dockerfiler::renv$snapshot(
  packages = custom_packages,
  lockfile = the_lockfile,
  prompt = FALSE
) } else {
    file.copy(from = system.file("renv.lock",package = "dockerfiler"),to = the_lockfile)
}

# Modify R version for tests
renv_file <- readLines(file.path(dir_build, "renv.lock"))
renv_file[grep("Version", renv_file)[1]] <- '    "Version": "4.1.2",'
writeLines(renv_file, file.path(dir_build, "renv.lock"))



# dock_from_renv ----
test_that("dock_from_renv works", {

  # testthat::skip_on_cran()
  # skip_if_not(interactive())
  # Create Dockerfile
  skip_if(is_rdevel, "skip on R-devel")

  testthat::with_mocked_bindings(code = {
    out <- dock_from_renv(
      lockfile = the_lockfile,
      FROM = "rocker/verse",
      renv_version = "0.0.0"
    )
  },
  compact_sysreqs = function(...) "fake sys reqs",
  repos_as_character = function(...) "fake repos"
  )

  expect_s3_class(
    out,
    "Dockerfile"
  )
  expect_s3_class(
    out,
    "R6"
  )

  # read Dockerfile
  out$write(
    file.path(
      dir_build,
      "Dockerfile"
    )
  )

  dock_created <- readLines(
    file.path(
      dir_build,
      "Dockerfile"
    )
  )

  dock_expected <- readLines(
    testthat::test_path("renv_Dockerfile")
  )

  expect_equal(dock_created, dock_expected)

  skip_if(is_rdevel, "Skip R-devel")
  #python3 is not a direct dependencies from custom_packages
  expect_false(  any(grepl("python3",out$Dockerfile)))

})
# rstudioapi::navigateToFile(file.path(dir_build, "Dockerfile"))
#
test_that("dock_from_renv works with full dependencies", {
  # testthat::skip_on_cran()
  # skip_if_not(interactive())
  # Create Dockerfile
skip_if(is_rdevel, "skip on R-devel")
  out <- dock_from_renv(
    dependencies = TRUE,
    lockfile = the_lockfile,
    FROM = "rocker/verse",
  )
  expect_s3_class(
    out,
    "Dockerfile"
  )
  expect_s3_class(
    out,
    "R6"
  )
  skip_if(is_rdevel, "Skip R-devel")
  #python3 is  a un-direct dependencies from custom_packages
  expect_true(  any(grepl("python3",out$Dockerfile)))
})
# rstudioapi::navigateToFile(file.path(dir_build, "Dockerfile"))



# repos_as_character ----
test_that("repos_as_character works", {
  out <- dockerfiler:::repos_as_character(
    repos = c(
      RSPM = paste0("https://packagemanager.rstudio.com/all/__linux__/focal/latest"),
      CRAN = "https://cran.rstudio.com/"
    )
  )
  expect_equal(
    out,
    "c(RSPM = 'https://packagemanager.rstudio.com/all/__linux__/focal/latest', CRAN = 'https://cran.rstudio.com/')"
  )
})

# gen_base_image ----
test_that("gen_base_image works", {
  out <- dockerfiler:::gen_base_image(
    r_version = "4.0",
    FROM = "rstudio/r-base"
  )
  expect_equal(out, "rstudio/r-base:4.0")

  out <- dockerfiler:::gen_base_image(
    r_version = "4.0",
    FROM = "rocker/verse"
  )
  expect_equal(out, "rocker/verse:4.0")
})





test_that("dock_from_renv works with specific renv", {

  skip_if(is_rdevel, "skip on R-devel")
  # testthat::skip_on_cran()
the_lockfile1.0.0 <- system.file("renv_with_1.0.0.lock",package = "dockerfiler")

for (lf in list(the_lockfile,the_lockfile1.0.0)){
for (renv_version in list(NULL,"banana","missing")){


  if (!is.null(renv_version) && renv_version == "missing") {
    out <- dock_from_renv(lockfile = lf,
                          FROM = "rocker/verse")
  } else{
    out <- dock_from_renv(
      lockfile = lf,
      FROM = "rocker/verse",
      renv_version = renv_version
    )

  }
socle_install_version <- "remotes::install_version\\(\"renv\", version = \""
  if (lf == the_lockfile &    is.null(renv_version)) {
    test_string <- 'install.packages\\(c\\(\"renv\",\"remotes\"))'
  } else if (lf == the_lockfile1.0.0 & is.null(renv_version)) {
    test_string <- 'install.packages\\(c\\(\"renv\",\"remotes\"))'
  } else if (lf == the_lockfile &  renv_version == "banana") {
    test_string <-  paste0(socle_install_version,"banana"  ,"\"\\)")
  } else if (lf == the_lockfile1.0.0 & renv_version == "banana") {
    test_string <- paste0(socle_install_version,"banana","\"\\)")
  } else if (lf == the_lockfile & renv_version == "missing") {
    test_string <-
      paste0(
        socle_install_version,dockerfiler::renv$the$metadata$version,"\"\\)"
      )
  } else if (lf == the_lockfile1.0.0 & renv_version == "missing") {
    test_string <-paste0(socle_install_version,"1.0.0","\"\\)")
  }

  expect_true( any(   grepl(test_string , out$Dockerfile)    ),
               info = paste(lf," & ",renv_version))


}}




})

unlink(dir_build, recursive = TRUE)

