#' Summarize Missing Data in a Data Frame
#'
#' Returns a tibble showing the count and percent of missing values by column.
#'
#' @param data A data frame or tibble.
#' @return A tibble summarizing missingness by variable.
#' @examples
#' df <- tibble::tibble(a = c(1, NA, 3), b = c(NA, NA, 3))
#' summarize_missing(df)
#'
#' @importFrom tibble tibble
#' @export
summarize_missing <- function(data) {
  n <- nrow(data)
  tibble::tibble(
    column = names(data),
    n_missing = sapply(data, function(x) sum(is.na(x))),
    percent_missing = round(100 * sapply(data, function(x) sum(is.na(x))) / n, 2)
  )
}
