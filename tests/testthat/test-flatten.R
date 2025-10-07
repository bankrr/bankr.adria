test_that("multiplication works", {
  x <- readLines(pkg_file("testdata/cbi.txt"))
  dat <- flatten_cbi(x)
  expect_s3_class(dat, "data.frame")
  expect_equal(nrow(dat), 3L)
})
