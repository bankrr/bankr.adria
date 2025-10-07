header_code_pattern <- function() {
  paste0(
    "^RH", # Record type
    "\\d{5}", # Bank code (5 digits)
    "[A-Z0-9]{5}", # Company code (5 alphanumeric)
    "\\d{6}", # Date DDMMYY (6 digits)
    "[A-Z0-9]{20,27}", # Account ID (20-27 alphanumeric)
    "\\s*", # Spaces (padding)
    "\\d{5}", # Progressive record number (5 digits)
    "$"
  )
}

closing_record_pattern <- function() {
  paste0(
    "^64", # Record type (closing balance)
    "\\d{7}", # Progressive number (7 digits)
    "[A-Z]{3}", # Currency code (3 letters, e.g., EUR)
    "\\d{6}", # Date DDMMYY (6 digits)
    "[CD]", # Debit/Credit flag (C or D)
    "\\d{13,15}", # Amount with implied decimals (13-15 digits)
    "[CD]", # Final balance flag (C or D)
    "\\d{13,15}", # Final balance amount (13-15 digits)
    "\\s{0,}", # Optional spaces (padding)
    "$"
  )
}

# Alternative with more specific amount format (assuming 2 decimal places)
closing_record_pattern_detailed <- function() {
  paste0(
    "^64", # Record type
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
closing_record_pattern_with_commas <- function() {
  paste0(
    "^64", # Record type
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

footer_record_pattern <- function() {
  paste0(
    "^EF", # Record type (End File)
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
footer_record_pattern_detailed <- function() {
  paste0(
    "^EF", # Record type
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

summary_record_pattern <- function() {
  paste0(
    "^61", # Record type (opening balance)
    "\\d{7}", # Progressive number (7 digits)
    "\\s*", # Spaces
    "\\d{7}", # Sequential number (7 digits)
    "\\s*", # Spaces
    "[A-Z0-9]{3}", # Account type code (3 chars)
    "\\d{5}", # Bank code (5 digits)
    "\\d{5}", # Branch code (5 digits)
    "\\d{12}", # Account number (12 digits)
    "[A-Z]{3}", # Currency code (3 letters)
    "\\d{6}", # Date DDMMYY (6 digits)
    "[CD]", # Debit/Credit flag (C or D)
    "\\d{13,15}", # Balance amount (13-15 digits)
    "[A-Z]{2}", # Country code (2 letters)
    "\\s*", # Optional trailing spaces
    "$"
  )
}

transaction_record_pattern <- function() "^62"

continuation_record__pattern <- function() "^63\\d{7}"
