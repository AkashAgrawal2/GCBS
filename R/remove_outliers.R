#' Remove Outliers from a Data Frame
#'
#' Removes rows flagged as outliers by the logical columns produced by \code{detect_outliers()}.
#'
#' @param data A data frame or tibble with one or more \code{*_outlier} logical columns.
#' @param outlier_cols Character vector of columns to use for outlier removal. Default: all columns ending in \code{"_outlier"}.
#' @param how Should rows be removed if they are outliers in \code{"any"} of the specified columns (default), or \code{"all"} of them?
#' @param quiet Logical. If TRUE, suppresses informational messages.
#' @return Tibble without the rows considered outliers, with a summary attached as the "outlier_removal_summary" attribute.
#' @examples
#' df <- tibble::tibble(
#'   height = c(160, 165, 170, 171, 168, 250),
#'   weight = c(60, 65, 62, 1000, 64, 61)
#' )
#' flagged <- detect_outliers(df)
#' cleaned <- remove_outliers(flagged)
#'
#' @importFrom tibble as_tibble
#' @export
remove_outliers <- function(
    data,
    outlier_cols = NULL,
    how = c("any", "all"),
    quiet = FALSE
) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("The 'tibble' package is required for this function.")
  }
  data <- tibble::as_tibble(data)
  how <- match.arg(how)

  # Guess outlier columns if not provided
  if (is.null(outlier_cols)) {
    outlier_cols <- names(data)[grepl("_outlier$", names(data))]
    if (length(outlier_cols) == 0) stop("No outlier columns found (columns ending in '_outlier').")
  }
  # Check columns exist and are logical
  for (col in outlier_cols) {
    if (!(col %in% names(data)))
      stop(sprintf("Outlier column '%s' not found in data.", col))
    if (!is.logical(data[[col]]))
      stop(sprintf("Column '%s' is not logical--did you run detect_outliers() first?", col))
  }

  row_is_outlier <- if (how == "any") {
    apply(data[outlier_cols], 1, function(x) any(isTRUE(x)))
  } else {
    apply(data[outlier_cols], 1, function(x) all(isTRUE(x)))
  }

  n_removed <- sum(row_is_outlier, na.rm = TRUE)
  outlier_rows <- which(row_is_outlier)
  summary_df <- data.frame(
    n_rows_before = nrow(data),
    n_rows_removed = n_removed,
    n_rows_after = nrow(data) - n_removed,
    outlier_rows = paste(outlier_rows, collapse = ",")
  )

  if (!quiet) {
    message(sprintf("Removed %d outlier row%s using '%s' outlier criteria.",
                    n_removed, ifelse(n_removed != 1, "s", ""), how))
  }
  data_cleaned <- data[!row_is_outlier, , drop = FALSE]
  attr(data_cleaned, "outlier_removal_summary") <- summary_df
  return(data_cleaned)
}
