#' Trim Whitespace from All Character Columns
#'
#' Removes leading and trailing whitespace from all character columns in a data frame.
#'
#' @param data A data frame or tibble.
#' @return Tibble with trimmed character columns.
#' @examples
#' df <- tibble::tibble(name = c(" John ", "Jane"))
#' trim_whitespace(df)
#'
#' @importFrom tibble as_tibble
#' @export
trim_whitespace <- function(data) {
  data <- tibble::as_tibble(data)
  for (col in names(data)) {
    if (is.character(data[[col]])) {
      data[[col]] <- sub("^\\s+", "", data[[col]])
      data[[col]] <- sub("\\s+$", "", data[[col]])
    }
  }
  tibble::as_tibble(data)
}
