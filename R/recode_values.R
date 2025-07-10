#' Recode Values in a Column
#'
#' Bulk recode values in a column (e.g., "M" to "Male", "F" to "Female").
#'
#' @param data A data frame or tibble.
#' @param column Column name as character.
#' @param from Vector of original values.
#' @param to Vector of new values (same length as from).
#' @return Data frame with values in column recoded.
#' @examples
#' df <- tibble::tibble(gender = c("M", "F", "M"))
#' recode_values(df, "gender", from = c("M", "F"), to = c("Male", "Female"))
#'
#' @importFrom tibble as_tibble
#' @export
recode_values <- function(data, column, from, to) {
  data <- tibble::as_tibble(data)
  if (!column %in% names(data)) stop("Column not found.")
  rec_map <- setNames(to, from)
  data[[column]] <- rec_map[as.character(data[[column]])]
  tibble::as_tibble(data)
}
