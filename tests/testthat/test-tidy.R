test_that("multiplication works", {
  dat <- read(
    pkg_file("testdata", "transactions.csv")
  )
  expect_true(is.data.frame(tidy(dat)))
  expect_true(nrow(tidy(dat)) > 0)
})
