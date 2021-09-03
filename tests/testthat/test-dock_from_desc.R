test_that("dock_from_desc works", {

  base_pkg_ <- c(
    "base",
    "boot",
    "class",
    "cluster",
    "codetools",
    "compiler",
    "datasets",
    "foreign",
    "graphics",
    "grDevices",
    "grid",
    "KernSmooth",
    "lattice",
    "MASS",
    "Matrix",
    "methods",
    "mgcv",
    "nlme",
    "nnet",
    "parallel",
    "rpart",
    "spatial",
    "splines",
    "stats",
    "stats4",
    "survival",
    "tcltk",
    "tools",
    "utils"
  )

  skip_on_cran()
  tmp <- tempdir()

  old_wd <- setwd(tmp)
  on.exit({
    setwd(old_wd)
  })

  usethis::create_package("flap")

  setwd("flap")
  usethis::use_package("attempt")
  usethis::use_package("glue")
  usethis::use_package("R6")
  usethis::use_package("fs")
  usethis::use_package("cli")
  usethis::use_package("remotes")
  usethis::use_package("desc")
  usethis::use_package("pkgbuild")
  usethis::use_package("jsonlite")

  my_dock <- dock_from_desc(
    "DESCRIPTION"
  )

  expect_s3_class(my_dock, "R6")
  expect_s3_class(my_dock, "Dockerfile")

  tpf <- tempfile()

  my_dock$write(tpf)

  tpf <- paste(
    readLines(tpf),
    collapse = " "
  )

  expect_true(
    grepl(
      "rocker/r-ver",
      tpf
    )
  )
  expect_true(
    grepl(
      "apt-get update && apt-get install",
      tpf
    )
  )
  expect_true(
    grepl(
      "mkdir /build_zone",
      tpf
    )
  )
  expect_true(
    grepl(
      "rm -rf /build_zone",
      tpf
    )
  )
  x <- desc::desc_get_deps()
  x <- x[x$type == "Imports" & !(x$package %in% base_pkg_), ]
  if (length(x) > 0){
    for (i in x$package) {
      cat(i)
      expect_true(
        grepl(
          i,
          tpf
        )
      )
    }
  }
  unlink(".dockerignore", TRUE, TRUE)
})


