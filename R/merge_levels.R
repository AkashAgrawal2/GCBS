#' Merge Levels in a Factor or Character Column
#'
#' Replaces multiple values in a categorical column with a single new value (for example, merging missings).
#'
#' @param data A data frame or tibble.
#' @param column Column to modify (character).
#' @param new_level The replacement value (character).
#' @param old_levels Character vector: old levels/values to replace.
#' @return Data frame with modified column.
#' @examples
#' df <- tibble::tibble(status = c("Missing", "N/A", "Complete"))
#' merge_levels(df, "status", new_level = "Incomplete", old_levels = c("Missing", "N/A"))
#'
#' @importFrom tibble as_tibble
#' @export
merge_levels <- function(data, column, new_level, old_levels) {
  data <- tibble::as_tibble(data)
  if (!column %in% names(data)) stop("Column not found.")
  if (is.factor(data[[column]])) {
    levels(data[[column]])[levels(data[[column]]) %in% old_levels] <- new_level
    data[[column]] <- factor(as.character(data[[column]]))
  } else {
    data[[column]][data[[column]] %in% old_levels] <- new_level
  }
  tibble::as_tibble(data)
}
