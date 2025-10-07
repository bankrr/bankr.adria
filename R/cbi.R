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
  pattern <- ifelse(
    is_transaction(x),
    pattern_transaction(capture = TRUE),
    pattern_continuation(capture = TRUE)
  )

  chr_ply(
    seq_along(x),
    function(i) {
      m <- regexec(pattern[[i]], x[[i]], perl = TRUE)
      matches <- regmatches(x[[i]], m)[[1]]
      if ("transaction_number" %in% names(matches)) {
        matches[["transaction_number"]]
      } else {
        NA_character_
      }
    }
  )
}

xtr_debit_credit <- function(x) {
  m <- regexec(pattern_transaction(capture = TRUE), x, perl = TRUE)
  matches <- regmatches(x, m)
  chr_ply(
    matches,
    function(m) {
      if ("debit_credit" %in% names(m)) m[["debit_credit"]] else NA_character_
    }
  )
}

xtr_amount <- function(x) {
  m <- regexec(pattern_transaction(capture = TRUE), x, perl = TRUE)
  matches <- regmatches(x, m)
  chr_ply(
    matches,
    function(m) {
      if ("amount_int" %in% names(m) && "amount_dec" %in% names(m)) {
        paste0(m[["amount_int"]], m[["amount_dec"]])
      } else {
        NA_character_
      }
    }
  )
}

xtr_transaction_date <- function(x) {
  m <- regexec(pattern_transaction(capture = TRUE), x, perl = TRUE)
  matches <- regmatches(x, m)
  chr_ply(
    matches,
    function(m) {
      if ("transaction_date" %in% names(m)) {
        m[["transaction_date"]]
      } else {
        NA_character_
      }
    }
  )
}

xtr_value_date <- function(x) {
  m <- regexec(pattern_transaction(capture = TRUE), x, perl = TRUE)
  matches <- regmatches(x, m)
  chr_ply(
    matches,
    function(m) {
      if ("value_date" %in% names(m)) m[["value_date"]] else NA_character_
    }
  )
}

xtr_description <- function(x) {
  pattern <- ifelse(
    is_transaction(x),
    pattern_transaction(capture = TRUE),
    pattern_continuation(capture = TRUE)
  )

  chr_ply(
    seq_along(x),
    function(i) {
      m <- regexec(pattern[[i]], x[[i]], perl = TRUE)
      matches <- regmatches(x[[i]], m)[[1]]
      if ("description" %in% names(matches)) {
        matches[["description"]]
      } else {
        NA_character_
      }
    }
  )
}
