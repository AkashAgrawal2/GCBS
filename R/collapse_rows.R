#' Collapse Rows by Multiple Keys into New Columns
#'
#' Collapses multiple rows with the same combination of two key columns into a single row,
#' expanding varying values across multiple new columns. This is useful for flattening
#' repeated records (e.g., multiple measurements per patient visit) into wide format.
#'
#' Columns that have the same value for all rows in a group (defined by \code{key_col1} and \code{key_col2})
#' are kept as a single column. Columns with multiple distinct values in a group are expanded
#' with suffixes (_1, _2, etc.).
#'
#' @param df A data frame that may contain repeated rows per combination of two keys.
#' @param key_col1 A string specifying the first key column (e.g., patient ID).
#' @param key_col2 A string specifying the second key column (e.g., visit date).
#'
#' @return A data frame with one row per key combination and multiple value columns
#' for each repeated variable. Columns with consistent values are not expanded.
#'
#' @examples
#' df <- data.frame(
#'   patient_id = c(1, 1, 1, 1, 1, 1),
#'   visit_date = c("2024-01-01", "2024-01-01", "2024-01-01",
#'                  "2024-02-01", "2024-02-01", "2024-02-01"),
#'   name = rep("Alice", 6),
#'   lab = c("A", "B", "C", "A", "B", "C"),
#'   value = c(10, 12, 14, 8, 9, 10)
#' )
#' collapse_rows(df, "patient_id", "visit_date")
#'
#' @importFrom dplyr select group_by mutate ungroup summarise full_join across pull n_distinct first
#' @importFrom tidyr pivot_wider
#' @importFrom rlang sym
#' @importFrom purrr reduce
#' @export
collapse_rows <- function(df, key_col1, key_col2) {

  key_sym1 <- sym(key_col1)
  key_sym2 <- sym(key_col2)

  key_cols <- c(key_col1, key_col2)

  other_cols <- setdiff(names(df), key_cols)

  # Identify constant columns (one unique value per group)
  constant_cols <- other_cols[sapply(other_cols, function(col) {
    df %>%
      group_by(across(all_of(key_cols))) %>%
      summarise(n_unique = n_distinct(.data[[col]]), .groups = "drop") %>%
      pull(n_unique) %>%
      max() == 1
  })]

  varying_cols <- setdiff(other_cols, constant_cols)

  # Extract constant values
  df_constants <- df %>%
    group_by(across(all_of(key_cols))) %>%
    summarise(across(all_of(constant_cols), ~ first(.x)), .groups = "drop")

  # Expand varying values
  expanded_list <- lapply(varying_cols, function(col) {
    df %>%
      select(all_of(key_cols), all_of(col)) %>%
      group_by(across(all_of(key_cols))) %>%
      mutate(row_num = row_number()) %>%
      ungroup() %>%
      pivot_wider(
        names_from = row_num,
        values_from = all_of(col),
        names_prefix = paste0(col, "_")
      )
  })

  # Merge all expanded columns
  if (length(expanded_list) > 0) {
    df_varying <- reduce(expanded_list, full_join, by = key_cols)
    df_final <- full_join(df_constants, df_varying, by = key_cols)
  } else {
    df_final <- df_constants
  }

  return(df_final)
}
