validate_line <- function(x) {
  if (nchar(x) == 120L) {
    return(TRUE)
  }
  FALSE
}

is_header_code <- function(x) {
  grepl(header_code_pattern(), x)
}

header_code <- function(x) {
  assert_header_code(x)
  substr(x, 1L, 2L)
}

bank_code <- function(x) {
  assert_header_code(x)
  substr(x, 3L, 7L)
}

company_code <- function(x) {
  assert_header_code(x)
  substr(x, 8L, 12L)
}

date <- function(x) {
  assert_header_code(x)
  substr(x, 13L, 18L)
}

account_number <- function(x) {
  assert_header_code(x)
  substr(x, 19, nchar(x))
}
