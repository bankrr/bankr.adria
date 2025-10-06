validate_line <- function(x) {
  if (nchar(x) == 120L) {
    return(TRUE)
  }
  FALSE
}

validate_header_code <- function(x) {
  stringr::str_detect(x, "^RH\\d{5}[A-Z0-9]{5}\\d{6}[A-Z0-9]{20,27}$")
}

header_code <- function(x) {
  stopifnot(is_scalar_character(x))
  if (!validate_header_code(x)) {
    stop("Input is not header code")
  }
  stringr::str_sub(x, 1L, 2L)
}

bank_code <- function(x) {
  stopifnot(is_scalar_character(x))
  if (!validate_header_code(x)) {
    stop("Input is not header code")
  }
  stringr::str_sub(x, 3L, 7L)
}

company_code <- function(x) {
  stopifnot(is_scalar_character(x))
  if (!validate_header_code(x)) {
    stop("Input is not header code")
  }
  stringr::str_sub(x, 8L, 12L)
}

date <- function(x) {
  stopifnot(is_scalar_character(x))
  if (!validate_header_code(x)) {
    stop("Input is not header code")
  }
  stringr::str_sub(x, 13L, 18L)
}

account_number <- function(x) {
  stopifnot(is_scalar_character(x))
  if (!validate_header_code(x)) {
    stop("Input is not header code")
  }
  stringr::str_sub(x, 19, nchar(x))
}
