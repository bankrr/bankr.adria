pattern_header <- function() {
  paste0(
    "^\\s?RH", # Record type
    "\\d{5}", # Bank code (5 digits)
    "[A-Z0-9]{5}", # Company code (5 alphanumeric)
    "\\d{6}", # Date DDMMYY (6 digits)
    "[A-Z0-9]{20,27}", # Account ID (20-27 alphanumeric)
    "\\s*", # Spaces (padding)
    "\\d{5}", # Progressive record number (5 digits)
    "$"
  )
}

pattern_closing <- function() {
  paste0(
    "^\\s?64", # Record type (closing balance)
    "\\d{7}", # Progressive number (7 digits)
    "[A-Z]{3}", # Currency code (3 letters, e.g., EUR)
    "\\d{6}", # Date DDMMYY (6 digits)
    "[CD]", # Debit/Credit flag (C or D)
    "\\d{13,15}", # Amount with implied decimals (13-15 digits)
    "[CD]", # Final balance flag (C or D)
    "\\d{13,15}", # Final balance amount (13-15 digits)
    "\\s*", # Optional spaces (padding)
    "$"
  )
}

# Alternative with more specific amount format (assuming 2 decimal places)
pattern_closing_detailed <- function() {
  paste0(
    "^\\s?64", # Record type
    "(\\d{7})", # Progressive number
    "([A-Z]{3})", # Currency code
    "(\\d{6})", # Date DDMMYY
    "([CD])", # Debit/Credit flag
    "(\\d{13,15})", # Amount (including implied decimals)
    "([CD])", # Final balance flag
    "(\\d{13,15})", # Final balance amount
    "\\s*", # Padding
    "$"
  )
}

# CBI uses implied decimals, not commas (?)
pattern_closing_with_commas <- function() {
  paste0(
    "^\\s?64", # Record type
    "\\d{7}", # Progressive number
    "[A-Z]{3}", # Currency code
    "\\d{6}", # Date
    "[CD]", # Flag
    "\\d{9,12},\\d{2}", # Amount with comma
    "[CD]", # Flag
    "\\d{9,12},\\d{2}", # Balance with comma
    "\\s*",
    "$"
  )
}

pattern_footer <- function() {
  paste0(
    "^\\s?EF", # Record type (End File)
    "\\d{5}", # Bank code (5 digits)
    "[A-Z0-9]{5}", # Company code (5 alphanumeric)
    "\\d{6}", # Date DDMMYY (6 digits)
    "[A-Z0-9]{20,27}", # Account identifier (20-27 alphanumeric)
    "\\s*", # Spaces (padding)
    "\\d{7}", # Total number of records (7 digits)
    "\\s*", # More padding
    "\\d{7}", # Total number of transactions (7 digits)
    "\\s*", # Padding
    "\\d{6}", # Date DDMMYY again (6 digits)
    "\\s*", # Optional trailing spaces
    "$"
  )
}

# Detailed version with capture groups
pattern_footer_detailed <- function() {
  paste0(
    "^\\s?EF", # Record type
    "(\\d{5})", # Bank code
    "([A-Z0-9]{5})", # Company code
    "(\\d{6})", # Date
    "([A-Z0-9]{20,27})", # Account ID
    "\\s*", # Padding
    "(\\d{7})", # Total records
    "\\s*", # Padding
    "(\\d{7})", # Total transactions
    "\\s*", # Padding
    "(\\d{6})", # Date (repeat)
    "\\s*",
    "$"
  )
}

pattern_summary <- function(capture = FALSE) {
  p <- pattern_components()

  if (capture) {
    paste0(
      "^\\s?",
      p$record_type_summary,
      "(?<progressive_number>",
      p$progressive_number,
      ")",
      "\\s+",
      "(?<sequential_number>",
      p$sequential_number,
      ")",
      "(?<transaction_counter>",
      p$transaction_counter,
      ")",
      "\\s+",
      "(?<account_type>",
      p$account_type,
      ")",
      "(?<cin>",
      p$cin,
      ")",
      "(?<bank_code>",
      p$bank_code,
      ")",
      "(?<branch_code>",
      p$branch_code,
      ")",
      "(?<account_number>",
      p$account_number,
      ")",
      "(?<currency>",
      p$currency_code,
      ")",
      "(?<date>",
      p$transaction_date,
      ")",
      "(?<debit_credit>",
      p$debit_credit_flag,
      ")",
      "(?<balance>",
      p$balance_amount,
      ")",
      "(?<country_code>",
      p$country_code,
      ")",
      "\\s*",
      "$"
    )
  } else {
    paste0(
      "^\\s?",
      p$record_type_summary,
      p$progressive_number,
      "\\s+",
      p$sequential_number,
      p$transaction_counter,
      "\\s+",
      p$account_type,
      p$cin,
      p$bank_code,
      p$branch_code,
      p$account_number,
      p$currency_code,
      p$transaction_date,
      p$debit_credit_flag,
      p$balance_amount,
      p$country_code,
      "\\s*",
      "$"
    )
  }
}

pattern_components <- function() {
  list(
    # Common components
    sequential_record = "\\d{7}", # Sequential record number (7 digits)
    transaction_number = "\\d{3}", # Transaction number (3 digits)

    # Transaction-specific components
    record_type_transaction = "62", # Record type (transaction)
    record_type_continuation = "63", # Record type (continuation)
    record_type_summary = "61", # Record type (opening balance)
    transaction_date = "\\d{6}", # Transaction date DDMMYY (6 digits)
    value_date = "\\d{6}", # Value date DDMMYY (6 digits)
    debit_credit_flag = "[CD]", # Debit/Credit flag (C or D)
    amount_implied = "\\d{9,15}", # Amount with implied decimals (9-15 digits)
    optional_decimals = ",?\\d{0,2}", # Optional comma and decimals
    transaction_code = "\\d{2}", # Transaction code (2 digits)
    description = ".*", # Description text (variable length)
    continuation_text = ".*", # Continuation text (variable length)

    # Summary-specific components
    progressive_number = "\\d{7}", # Progressive number (7 digits)
    sequential_number = "\\d{7}", # Sequential number (7 digits)
    transaction_counter = "\\d{3}", # Transaction counter (3 digits)
    account_type = "[A-Z0-9]{2}", # Account type code (2 chars)
    cin = "[A-Z]", # CIN - Carattere di controllo nazionale (1 letter)
    bank_code = "\\d{5}", # Bank code / ABI (5 digits)
    branch_code = "\\d{5}", # Branch code / CAB (5 digits)
    account_number = "\\d{12}", # Account number (12 digits)
    currency_code = "[A-Z]{3}", # Currency code (3 letters)
    balance_amount = "\\d{9,15},\\d{2}", # Balance amount with comma
    country_code = "[A-Z]{2}\\d{2}" # Country code + numbers
  )
}

pattern_transaction <- function(capture = FALSE) {
  p <- pattern_components()

  if (capture) {
    paste0(
      "^\\s?",
      p$record_type_transaction,
      "(?<sequential_record>",
      p$sequential_record,
      ")",
      "(?<transaction_number>",
      p$transaction_number,
      ")",
      "(?<transaction_date>",
      p$transaction_date,
      ")",
      "(?<value_date>",
      p$value_date,
      ")",
      "(?<debit_credit>",
      p$debit_credit_flag,
      ")",
      "(?<amount_int>",
      p$amount_implied,
      ")",
      "(?<amount_dec>",
      p$optional_decimals,
      ")",
      "(?<transaction_code>",
      p$transaction_code,
      ")",
      "(?<description>",
      p$description,
      ")",
      "$"
    )
  } else {
    paste0(
      "^\\s?",
      p$record_type_transaction,
      p$sequential_record,
      p$transaction_number,
      p$transaction_date,
      p$value_date,
      p$debit_credit_flag,
      p$amount_implied,
      p$optional_decimals,
      p$transaction_code,
      p$description,
      "$"
    )
  }
}

pattern_debit <- function() {
  paste0(
    "^\\s?62", # Transaction record
    "\\d{7}", # Sequential record number
    "\\d{3}", # Transaction number
    "\\d{6}", # Transaction date
    "\\d{6}", # Value date
    "D" # Debit flag
  )
}

pattern_credit <- function() {
  paste0(
    "^\\s?62", # Transaction record
    "\\d{7}", # Sequential record number
    "\\d{3}", # Transaction number
    "\\d{6}", # Transaction date
    "\\d{6}", # Value date
    "C" # Credit flag
  )
}

pattern_continuation <- function(capture = FALSE) {
  p <- pattern_components()

  if (capture) {
    paste0(
      "^\\s?",
      p$record_type_continuation,
      "(?<sequential_record>",
      p$sequential_record,
      ")", # Sequential record number
      "(?<transaction_number>",
      p$transaction_number,
      ")", # Transaction number - links to parent 62
      "(?<description>",
      p$continuation_text,
      ")", # Continuation text
      "$"
    )
  } else {
    paste0(
      "^\\s?",
      p$record_type_continuation,
      p$sequential_record,
      p$transaction_number,
      p$continuation_text,
      "$"
    )
  }
}
