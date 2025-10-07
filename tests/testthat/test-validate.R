test_that("validate() works", {
  dat <- read.csv2(
    pkg_file("testdata", "transactions.csv")
  )
  expect_no_error(validate(dat))
})

test_that("validate_cbi() works", {
  x <- readLines(
    pkg_file("testdata", "cbi.txt")
  )
  expect_no_error(validate_cbi(x))
})
