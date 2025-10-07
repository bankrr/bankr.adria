test_that("pad_calc works", {
  x <- paste0(character(10L), collapse = " ")
  y <- paste0(character(100L), collapse = " ")
  expect_equal(pad_calc(x, y), 12L)
})

test_that("pad_str works", {
  x <- pad_str(
    c("XYZ", "ABCD"),
    nchar_tot = 10L
  )
  expect_equal(x, "XYZ  ABCD")
})
