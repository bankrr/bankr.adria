validate_line <- function(x) {
  if (nchar(x) == 120L) {
    return(TRUE)
  }
  FALSE
}

# CBI file structure constants ----

CBI_HEADER_IDX <- 1L
CBI_SUMMARY_IDX <- 2L
CBI_FIRST_TRANSACTION_IDX <- 3L

# Header code ----

is_header <- function(x) {
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

is_summary <- function(x) {
  grepl(pattern_summary(), x)
}

# Closing record ----

is_closing <- function(x) {
  grepl(pattern_closing_with_commas(), x)
}

# Footer record ----

is_footer <- function(x) {
  grepl(pattern_footer(), x)
}

# Transaction record ----

is_transaction <- function(x) {
  grepl(pattern_transaction(), x)
}

is_debit <- function(x) {
  grepl(pattern_debit(), x)
}

is_credit <- function(x) {
  grepl(pattern_credit(), x)
}

# Continuation record ----

is_continuation <- function(x) {
  grepl(pattern_continuation(), x)
}

# Extract transaction fields ----

xtr_record_number <- function(x) {
  m <- regexec(pattern_transaction(capture = TRUE), x)
  matches <- regmatches(x, m)
  # Capture group [3] is transaction_number (3 digits)
  vapply(
    matches,
    function(m) if (length(m) > 2) m[3] else NA_character_,
    character(1),
    USE.NAMES = FALSE
  )
}

xtr_debit_credit <- function(x) {
  m <- regexec(pattern_transaction(capture = TRUE), x)
  matches <- regmatches(x, m)
  vapply(
    matches,
    function(m) if (length(m) > 5) m[6] else NA_character_,
    character(1),
    USE.NAMES = FALSE
  )
}

xtr_amount <- function(x) {
  m <- regexec(pattern_transaction(capture = TRUE), x)
  matches <- regmatches(x, m)
  vapply(
    matches,
    function(m) {
      if (length(m) > 8) paste0(m[7], m[8]) else NA_character_
    },
    character(1),
    USE.NAMES = FALSE
  )
}

xtr_transaction_date <- function(x) {
  m <- regexec(pattern_transaction(capture = TRUE), x)
  matches <- regmatches(x, m)
  vapply(
    matches,
    function(m) if (length(m) > 4) m[4] else NA_character_,
    character(1),
    USE.NAMES = FALSE
  )
}

xtr_value_date <- function(x) {
  m <- regexec(pattern_transaction(capture = TRUE), x)
  matches <- regmatches(x, m)
  vapply(
    matches,
    function(m) if (length(m) > 5) m[5] else NA_character_,
    character(1),
    USE.NAMES = FALSE
  )
}

xtr_description <- function(x) {
  m <- regexec(pattern_transaction(capture = TRUE), x)
  matches <- regmatches(x, m)
  # Capture group [9] is description (variable length text)
  vapply(
    matches,
    function(m) if (length(m) > 9) m[10] else NA_character_,
    character(1),
    USE.NAMES = FALSE
  )
}
