#' Split a Column into Multiple Columns
#'
#' Splits a string column into several columns using a delimiter.
#'
#' @param data A data frame or tibble.
#' @param column Column name to split (character).
#' @param into Character vector of new column names.
#' @param sep Separator character or regular expression.
#' @param remove_original Logical. Remove the original column? Default: TRUE.
#' @return Tibble with new columns.
#' @examples
#' df <- tibble::tibble(name = c("John Smith", "Jane Doe"))
#' split_column(df, column = "name", into = c("first", "last"), sep = " ")
#'
#' @importFrom tibble as_tibble
#' @export
split_column <- function(data, column, into, sep = " ", remove_original = TRUE) {
  data <- tibble::as_tibble(data)
  if (!column %in% names(data)) stop("Column not found.")
  split_mat <- t(sapply(strsplit(as.character(data[[column]]), split = sep, fixed = FALSE),
                        function(x) c(x, rep(NA, length(into) - length(x)))))
  split_df <- as.data.frame(split_mat, stringsAsFactors = FALSE)
  names(split_df) <- into
  out <- dplyr::bind_cols(data, split_df)
  if (remove_original) out[[column]] <- NULL
  tibble::as_tibble(out)
}
