test_that("xtr_match extracts simple pattern", {
  text <- "The amount is 123.45"
  pattern <- "amount is ([0-9.]+)"
  result <- xtr_match(text, pattern)

  expect_equal(result[[1]][1], "amount is 123.45")
  expect_equal(result[[1]][2], "123.45")
})

test_that("xtr_match extracts named groups", {
  text <- "Date: 2023-12-25"
  pattern <- "(?<label>Date): (?<value>[0-9-]+)"
  result <- xtr_match(text, pattern)

  expect_equal(result[[1]]["label"], c(label = "Date"))
  expect_equal(result[[1]]["value"], c(value = "2023-12-25"))
})

test_that("xtr_match returns empty for no match", {
  text <- "No numbers here"
  pattern <- "[0-9]+"
  result <- xtr_match(text, pattern)

  expect_equal(result[[1]], character(0))
})

test_that("xtr_match works on vectors", {
  text <- c("Amount: 100", "Amount: 200", "No match")
  pattern <- "Amount: ([0-9]+)"
  result <- xtr_match(text, pattern)

  expect_equal(result[[1]][2], "100")
  expect_equal(result[[2]][2], "200")
  expect_equal(result[[3]], character(0))
})
