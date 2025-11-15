test_that("tidy works", {
  dat <- read(
    pkg_file("testdata", "transactions.csv")
  )
  expect_true(is.data.frame(tidy(dat)))
  expect_true(nrow(tidy(dat)) > 0)
  expect_false("x_1" %in% colnames(tidy(dat)))
})
