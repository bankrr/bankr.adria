validate_line <- function(x) {
  if (nchar(x) == 120L) {
    return(TRUE)
  }
  FALSE
}

# Header code ----

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

# Summary record ----

is_summary_record <- function(x) {
  grepl(summary_record_pattern(), x)
}

# Closing record ----

is_closing_record <- function(x) {
  grepl(closing_record_pattern_with_commas(), x)
}

# Footer record ----

is_footer_record <- function(x) {
  grepl(footer_record_pattern(), x)
}

# Starting record ----

is_starting_record <- function(x) {
  grepl(transaction_record_pattern(), x)
}

# Continuation record ----

is_continuation_record <- function(x) {
  grepl(continuation_record__pattern(), x)
}
