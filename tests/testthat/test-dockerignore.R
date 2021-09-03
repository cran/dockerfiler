test_that("dockerignore works", {
  pth <- docker_ignore_add(path = ".")

  fls <- paste(readLines(pth), collapse = " ")

  expect_true(
    grepl(".RData", fls)
  )
  expect_true(
    grepl(".Rhistory", fls)
  )
  expect_true(
    grepl(".git", fls)
  )
  expect_true(
    grepl(".gitignore", fls)
  )
  expect_true(
    grepl("manifest.json", fls)
  )
  expect_true(
    grepl("rsconnect", fls)
  )
  expect_true(
    grepl("Rproj.user", fls)
  )
})
unlink(".dockerignore")
