validate <- function(dat) {
  expected_cols <- list(
    DATA = "character",
    VALUTA = "character",
    DARE = "logical",
    AVERE = "character",
    X = "character",
    DIVISA = "character",
    DESCRIZIONE_OPERAZIONE = "character",
    CAUSALE_ABI = "integer",
    X_1 = "logical"
  )

  validate_is_dataframe(dat)
  validate_has_rows(dat)
  currency <- c("X", "DIVISA")
  if (!any(currency %in% colnames(dat))) {
    stop(
      "At least one column between 'X' and 'DIVISA' is expected.",
      call. = FALSE
    )
  }
  validate_required_columns(dat, setdiff(expected_cols, currency))
  validate_no_extra_columns(dat, expected_cols)
  validate_column_types(dat, expected_cols)

  if ("DATA" %in% colnames(dat)) {
    invalid_data <- !validate_date_format_dmy(dat[["DATA"]], allow_empty = TRUE)
    if (any(invalid_data)) {
      stop(
        "Column 'DATA' contains invalid date formats. Expected DD/MM/YYYY or empty string.",
        call. = FALSE
      )
    }
  }

  if ("VALUTA" %in% colnames(dat)) {
    invalid_valuta <- !validate_date_format_dmy(dat[["VALUTA"]], allow_empty = TRUE)
    if (any(invalid_valuta)) {
      stop(
        "Column 'VALUTA' contains invalid date formats. Expected DD/MM/YYYY or empty string.",
        call. = FALSE
      )
    }
  }

  # Validate Italian number format for DARE and AVERE columns
  # Pattern matches: optional digits with dots as thousand separators, comma for decimal, then 2 digits
  italian_number_pattern <- "^\\d{1,3}(\\.\\d{3})*,\\d{2}$"

  if ("DARE" %in% colnames(dat)) {
    non_na_dare <- !is.na(dat[["DARE"]])
    if (any(non_na_dare)) {
      invalid_dare <- !grepl(
        italian_number_pattern,
        dat[["DARE"]][non_na_dare],
        perl = TRUE
      )
      if (any(invalid_dare)) {
        stop(
          "Column 'DARE' contains invalid number formats. Expected Italian format (e.g., '21.060,97')."
        )
      }
    }
  }

  if ("AVERE" %in% colnames(dat)) {
    non_na_avere <- !is.na(dat[["AVERE"]])
    if (any(non_na_avere)) {
      invalid_avere <- !grepl(
        italian_number_pattern,
        dat[["AVERE"]][non_na_avere],
        perl = TRUE
      )
      if (any(invalid_avere)) {
        stop(
          "Column 'AVERE' contains invalid number formats. Expected Italian format (e.g., '21.060,97')."
        )
      }
    }
  }

  # Check that required columns don't contain NA values
  required_no_na_cols <- c("DATA", "AVERE", "DESCRIZIONE_OPERAZIONE")
  validate_no_na_columns(dat, required_no_na_cols)

  # Validate X column contains only "EUR"
  if ("X" %in% colnames(dat)) {
    if (anyNA(is.na(dat[["X"]]))) {
      stop("Column 'X' contains NA values.")
    }
    invalid_currency <- dat[["X"]] != "EUR"
    if (any(invalid_currency)) {
      other_values <- unique(dat[["X"]][invalid_currency])
      stop(
        "Column 'X' contains invalid currency values. Expected only 'EUR', but found: ",
        paste(other_values, collapse = ", ")
      )
    }
  }

  invisible(dat)
}
