assert_header_code <- function(x) {
  stopifnot(
    "Not a scalar character" = is_scalar_character(x),
    "Not header code" = !is_header(x)
  )
}
