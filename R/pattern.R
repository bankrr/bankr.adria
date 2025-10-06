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
