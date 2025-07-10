#' Remove Duplicate Rows from a Data Frame
#'
#' Removes duplicate rows based on all columns or specified key columns.
#'
#' @param data A data frame or tibble.
#' @param key Character vector of column names to consider for duplication. Default: all columns.
#' @param keep Which duplicates to keep: \code{"first"} (default), \code{"last"}, or \code{FALSE} (remove all duplicates).
#' @return Tibble with duplicates removed.
#' @examples
#' df <- tibble::tibble(a = c(1, 1, 2), b = c("x", "x", "y"))
#' remove_duplicates(df)
#' remove_duplicates(df, key = "a")
#'
#' @importFrom tibble as_tibble
#' @export
remove_duplicates <- function(data, key = NULL, keep = c("first", "last", "none")) {
  data <- tibble::as_tibble(data)
  keep <- match.arg(keep)
  if (is.null(key)) key <- names(data)
  if (!all(key %in% names(data))) stop("Some specified keys not found in data.")
  if (keep == "first") {
    data <- data[!duplicated(data[key]), ]
  } else if (keep == "last") {
    data <- data[!duplicated(data[key], fromLast = TRUE), ]
  } else { # keep == "none"
    dup <- duplicated(data[key]) | duplicated(data[key], fromLast = TRUE)
    data <- data[!dup, ]
  }
  tibble::as_tibble(data)
}
