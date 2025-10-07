validate <- function(dat) {
  expected_cols <- list(
    DATA = "character",
    VALUTA = "character",
    DARE = "logical",
    AVERE = "character",
    X = "character",
    DESCRIZIONE.OPERAZIONE = "character",
    CAUSALE.ABI = "integer",
    X.1 = "logical"
  )

  if (!is.data.frame(dat)) {
    stop("Input must be a data.frame")
  }

  if (!nrow(dat) > 0) {
    stop("Input have at least one row")
  }

  if (!all(names(expected_cols) %in% colnames(dat))) {
    missing_cols <- setdiff(names(expected_cols), colnames(dat))
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!all(colnames(dat) %in% names(expected_cols))) {
    extra_cols <- setdiff(colnames(dat), expected_cols)
    warning("Unexpected columns found: ", paste(extra_cols, collapse = ", "))
  }

  for (col in expected_cols) {
    if (col %in% colnames(dat)) {
      col_class <- class(dat[[col]])
      expected_class <- expected_cols[[col]]

      if (!any(col_class %in% expected_class)) {
        stop(
          "Column '",
          col,
          "' has incorrect type. Expected: ",
          paste(expected_class, collapse = " or "),
          ", Got: ",
          paste(col_class, collapse = ", ")
        )
      }
    }
  }

  # Validate date format for DATA and VALUTA columns
  date_pattern <- "^(\\d{2}/\\d{2}/\\d{4}|)$"

  if ("DATA" %in% colnames(dat)) {
    invalid_data <- !grepl(date_pattern, dat[["DATA"]], perl = TRUE)
    if (any(invalid_data)) {
      stop(
        "Column 'DATA' contains invalid date formats. Expected DD/MM/YYYY or empty string."
      )
    }
  }

  if ("VALUTA" %in% colnames(dat)) {
    invalid_valuta <- !grepl(date_pattern, dat[["VALUTA"]], perl = TRUE)
    if (any(invalid_valuta)) {
      stop(
        "Column 'VALUTA' contains invalid date formats. Expected DD/MM/YYYY or empty string."
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
  required_no_na_cols <- c("DATA", "AVERE", "DESCRIZIONE.OPERAZIONE")
  for (col in required_no_na_cols) {
    if (col %in% colnames(dat)) {
      if (anyNA(dat[[col]])) {
        stop("Column '", col, "' contains NA values.")
      }
    }
  }

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

  invisible(TRUE)
}

validate_cbi <- function(x) {
  stopifnot(
    "`x` is not a character vector" = is.character(x),
    "`x` should have at least one item" = length(x) > 0
  )

  valid_len <- vapply(
    x,
    validate_line,
    FUN.VALUE = logical(1),
    USE.NAMES = FALSE
  )

  invalid_idx <- which(!valid_len)
  if (length(invalid_idx)) {
    stop(
      sprintf("The following lines does have 120 characters: %s"),
      paste0(invalid_idx, collapse = ", ")
    )
  }

  if (!is_header(x[[1L]])) {
    stop("First row is not a header record")
  }

  if (!is_summary(x[[2L]])) {
    stop("Second row is not a summary record")
  }

  if (!is_closing(x[[length(x) - 1]])) {
    stop("Second-last row is not a closing record")
  }

  if (!is_footer(x[[length(x)]])) {
    stop("Last row is not a footer record")
  }

  invisible(x)
}
