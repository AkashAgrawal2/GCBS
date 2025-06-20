#' Collapse Rows by Key into New Columns
#'
#' Collapses multiple rows with the same key in a data frame by expanding values
#' across new columns. This is useful when you want to flatten repeated records
#' into a single row with additional columns.
#'
#' @param df A data frame that may contain repeated values for a key.
#' @param key_col A string specifying the column name to use as the grouping key.
#'
#' @return A data frame with one row per key and multiple value columns per original variable.
#' Each repeated value is placed in a new column with a suffix (_1, _2, etc.).
#'
#' @examples
#' df <- data.frame(
#'   ID = c(1, 1, 2, 2, 2, 3),
#'   Value = c("A", "B", "C", "C", NA, "D"),
#'   Score = c(10, 20, 30, NA, 50, 60)
#' )
#' collapse_rows(df, "ID")
#'
#' @importFrom dplyr select group_by mutate ungroup full_join
#' @importFrom tidyr pivot_wider
#' @importFrom rlang sym
#' @importFrom purrr reduce
#' @export
collapse_rows <- function(df, key_col) {
  # Convert the key column name to a symbol for tidy evaluation
  key_sym <- rlang::sym(key_col)

  # Identify all columns except the key
  other_cols <- setdiff(names(df), key_col)

  # Expand each column separately to avoid type conflicts
  expanded_list <- lapply(other_cols, function(col) {
    df_temp <- df %>%
      dplyr::select(!!key_sym, dplyr::all_of(col)) %>%
      dplyr::group_by(!!key_sym) %>%
      dplyr::mutate(row_num = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(
        names_from = row_num,
        values_from = dplyr::all_of(col),
        names_prefix = paste0(col, "_")
      )
    return(df_temp)
  })

  # Merge the expanded columns by key
  df_final <- purrr::reduce(expanded_list, dplyr::full_join, by = key_col)

  return(df_final)
}
