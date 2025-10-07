test_that("is_header works", {
  x <- pad_str(c("RH08982AEN6D080825INBK5389029800100606", "08982"))
  expect_true(is_header(x))
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

test_that("is_closing works", {
  expect_true(is_closing(
    "640000001EUR080825C000000014642,37C000000014642,37"
  ))
})

test_that("is_footer works", {
  expect_true(is_footer(
    "EF08982AEN6D080825INBK5389029800100606      0000001                              0000321                         080825"
  ))
})

test_that("is_transaction works", {
  expect_true(is_transaction(
    "620000001001040625040625D000000000456,1026                                           DISPOSIZIONE BONIFICO - SCT Coordi"
  ))
})

test_that("is_credit works", {
  expect_true(is_credit(
    "620000001003110625110625C000000000211,2648"
  ))
})

test_that("is_debit works", {
  expect_true(is_debit(
    "620000001001040625040625D000000000456,1026"
  ))
})
