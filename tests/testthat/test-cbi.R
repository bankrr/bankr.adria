test_that("is_header_code works", {
  expect_true(is_header_code("RH08982AEN6D080825INBK5389029800100606"))
})

test_that("header_code works", {
  expect_equal(header_code("RH08982AEN6D080825INBK5389029800100606"), "RH")
})

test_that("bank_code works", {
  expect_equal(bank_code("RH08982AEN6D080825INBK5389029800100606"), "08982")
})

test_that("company_code works", {
  expect_equal(company_code("RH08982AEN6D080825INBK5389029800100606"), "AEN6D")
})

test_that("date works", {
  expect_equal(date("RH08982AEN6D080825INBK5389029800100606"), "080825")
})

test_that("account_number works", {
  expect_equal(
    account_number("RH08982AEN6D080825INBK5389029800100606"),
    "INBK5389029800100606"
  )
})
