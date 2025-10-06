assert_header_code <- function(x) {
  stopifnot(is_scalar_character(x))
  if (!validate_header_code(x)) {
    stop("Input is not header code")
  }
}
