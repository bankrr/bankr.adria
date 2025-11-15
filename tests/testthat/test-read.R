test_that("read works", {
  f <- pkg_file("testdata", "transactions.csv")
  expect_no_error(read(f))
  expect_true(is.data.frame(read(f)))
})
