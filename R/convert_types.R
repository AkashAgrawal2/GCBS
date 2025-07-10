#' Convert Column Types in a Data Frame
#'
#' Converts columns of a data frame to target types. Specify columns with a named map,
#' or provide a pattern to convert all columns of a given current type (e.g., all character columns to factor).
#' Supports "numeric", "integer", "character", "factor", "logical", and "date" (with format).
#' Warns if conversion introduces new \code{NA} values. Returns a tibble with conversion summary as an attribute.
#'
#' @param data A data frame or tibble.
#' @param type_map A named list or named character vector; names are columns, values are types (or a list with elements 'type' and optionally 'format' for dates).
#'   Example: \code{list(age = "numeric", bday = list(type = "date", format = "\%d/\%m/\%Y"))}
#'   Default: NULL (no per-column conversion).
#' @param pattern_map Convert all columns currently of a given type to a new type. Example: \code{list(character = "factor")}
#' @param default_date_format Default date format string if not specified for a "date" type column (default \code{"\%Y-\%m-\%d"}).
#' @param quiet Logical. If TRUE, minimizes output and warnings.
#' @return A tibble; conversion summary is attached as the "type_conversion_summary" attribute.
#' @examples
#' df <- data.frame(
#'   age = c("20", "21", "22"),
#'   gender = c("M", "F", "F"),
#'   enrolled = c("TRUE", "FALSE", NA),
#'   when = c("2022-01-01", "2022-02-15", "NA")
#' )
#' # 1. Per-column
#' convert_types(df, type_map = list(age = "numeric", enrolled = "logical", when = list(type = "date")))
#' # 2. By pattern: all character to factor
#' convert_types(df, pattern_map = list(character = "factor"))
#'
#' @importFrom tibble as_tibble
#' @export
convert_types <- function(
    data,
    type_map = NULL,
    pattern_map = NULL,
    default_date_format = "%Y-%m-%d",
    quiet = FALSE
) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("The 'tibble' package is required for this function.")
  }
  data <- tibble::as_tibble(data)
  old_data <- data

  # Helper for type conversion
  convert_column <- function(x, type, format = default_date_format) {
    if (type == "numeric")      return(as.numeric(x))
    else if (type == "integer")   return(as.integer(x))
    else if (type == "character") return(as.character(x))
    else if (type == "factor")    return(as.factor(x))
    else if (type == "logical")   return(as.logical(x))
    else if (type == "date")      return(as.Date(x, format = format))
    else stop("Unsupported type: ", type)
  }

  # First, apply pattern_map for global rules
  conversion_plan <- list()
  if (!is.null(pattern_map)) {
    for (cur_type in names(pattern_map)) {
      tgt_type <- pattern_map[[cur_type]]
      cur_type_cols <- names(which(sapply(data, function(x) class(x)[1] == cur_type)))
      for (col in cur_type_cols) {
        conversion_plan[[col]] <- tgt_type
      }
    }
  }

  # Next, override with type_map if supplied (has priority)
  if (!is.null(type_map)) {
    # Accept character vector or list
    if (is.character(type_map)) type_map <- as.list(type_map)
    for (col in names(type_map)) {
      conversion_plan[[col]] <- type_map[[col]]
    }
  }

  conversion_summary <- list()

  for (col in names(conversion_plan)) {
    if (!(col %in% names(data))) {
      warning(paste0("Column '", col, "' not found; skipping."))
      next
    }
    # Get old values/classes for reporting
    old_class <- class(data[[col]])[1]
    old_col   <- data[[col]]
    new_na_idx <- NULL

    # Determine target type and any format
    mapping <- conversion_plan[[col]]
    if (is.list(mapping)) {
      tgt_type <- mapping$type
      dt_fmt <- if (!is.null(mapping$format)) mapping$format else default_date_format
    } else {
      tgt_type <- mapping
      dt_fmt <- default_date_format
    }
    # Actually convert
    converted_col <- tryCatch(
      convert_column(data[[col]], tgt_type, dt_fmt),
      warning = function(w) {
        if (!quiet) warning(sprintf("Conversion warning in '%s': %s", col, conditionMessage(w)))
        suppressWarnings(convert_column(data[[col]], tgt_type, dt_fmt))
      },
      error = function(e) {
        warning(sprintf("Could not convert column '%s' to type '%s'. Left unchanged.", col, tgt_type))
        data[[col]] # leave as is
      }
    )

    # Check for new NAs introduced
    pre_nas <- which(is.na(old_col))
    post_nas <- which(is.na(converted_col))
    new_na_idx <- setdiff(post_nas, pre_nas)

    # Store result and update data
    data[[col]] <- converted_col

    conversion_summary[[col]] <- data.frame(
      column = col,
      from = old_class,
      to = class(converted_col)[1],
      newly_introduced_nas = if (length(new_na_idx) > 0) paste(new_na_idx, collapse = ",") else "",
      stringsAsFactors = FALSE
    )
    if (!quiet && old_class != class(converted_col)[1]) {
      message(sprintf("Converted '%s' from %s to %s.", col, old_class, class(converted_col)[1]))
      if (length(new_na_idx) > 0) {
        warning(sprintf("Conversion in '%s' created %d new NA(s) at row(s): %s.",
                        col, length(new_na_idx), paste(new_na_idx, collapse = ", ")))
      }
    }
  }

  attr(data, "type_conversion_summary") <- do.call(rbind, conversion_summary)
  return(data)
}
