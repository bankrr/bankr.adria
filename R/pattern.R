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
    "\\s{0,}", # Optional spaces (padding)
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

pattern_summary <- function() {
  paste0(
    "^\\s?61", # Record type (opening balance)
    "\\d{7}", # Progressive number (7 digits)
    "\\s+", # Spaces (one or more)
    "\\d{7}", # Sequential number (7 digits, e.g., 0000093)
    "\\d{3}", # More digits (3 digits, e.g., 001)
    "\\s+", # Spaces (one or more)
    "[A-Z0-9]{3}", # Account type code (3 chars)
    "\\d{5}", # Bank code (5 digits)
    "\\d{5}", # Branch code (5 digits)
    "\\d{12}", # Account number (12 digits)
    "[A-Z]{3}", # Currency code (3 letters)
    "\\d{6}", # Date DDMMYY (6 digits)
    "[CD]", # Debit/Credit flag (C or D)
    "\\d{9,15},\\d{2}", # Balance amount with comma (9-15 digits, comma, 2 decimals)
    "[A-Z]{2}\\d{2}", # Country code (2 letters) + numbers (2 digits)
    "\\s*", # Optional trailing spaces
    "$"
  )
}

pattern_transaction <- function() {
  paste0(
    "^\\s?62", # Record type (transaction)
    "\\d{7}", # Sequential record number (7 digits)
    "\\d{3}", # Transaction number (3 digits)
    "\\d{6}", # Transaction date DDMMYY (6 digits)
    "\\d{6}", # Value date DDMMYY (6 digits)
    "[CD]", # Debit/Credit flag (C or D)
    "\\d{9,15}", # Amount with implied decimals (9-15 digits)
    ",?\\d{0,2}", # Optional comma and decimals
    "\\d{2}", # Transaction code (2 digits)
    ".*", # Description text (variable length)
    "$"
  )
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

pattern_continuation <- function() {
  paste0(
    "^\\s?63", # Record type (continuation)
    "\\d{7}", # Sequential record number (7 digits)
    "\\d{3}", # Transaction number (3 digits) - links to parent 62
    ".*", # Continuation text (variable length)
    "$"
  )
}
