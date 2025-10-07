validate_line <- function(x) {
  if (nchar(x) == 120L) {
    return(TRUE)
  }
  FALSE
}

# Header code ----

is_header_code <- function(x) {
  grepl(pattern_header(), x)
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

# Summary record ----

is_summary_record <- function(x) {
  grepl(pattern_summary(), x)
}

# Closing record ----

is_closing_record <- function(x) {
  grepl(pattern_closing_with_commas(), x)
}

# Footer record ----

is_footer_record <- function(x) {
  grepl(pattern_footer(), x)
}

# Starting record ----

is_starting_record <- function(x) {
  grepl(pattern_transaction(), x)
}

# Continuation record ----

is_continuation_record <- function(x) {
  grepl(pattern_continuation(), x)
}
