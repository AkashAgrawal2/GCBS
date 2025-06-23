#' Collapse Rows by Visit and Add Visit-Based Suffixes
#'
#' Collapses multiple rows with the same combination of two key columns into a single row,
#' expanding varying values across multiple columns. The suffix of each varying column includes
#' the visit number (e.g., _visit_1, _visit_2). Columns that are constant per (key1, key2)
#' group are retained as a single column.
#'
#' @param df A data frame with repeated rows per key combination.
#' @param key_col1 First key column (e.g., patient ID).
#' @param key_col2 Second key column (e.g., visit date).
#'
#' @return A wide-format data frame with one row per patient per visit,
#' and varying values expanded with `_visit_1`, `_visit_2`, etc.
#'
#' @importFrom dplyr select group_by mutate ungroup summarise full_join across pull n_distinct first distinct arrange
#' @importFrom tidyr pivot_wider
#' @importFrom rlang sym
#' @importFrom purrr reduce
#' @export
collapse_rows <- function(df, key_col1, key_col2) {
  key_cols <- c(key_col1, key_col2)

  other_cols <- setdiff(names(df), key_cols)

  # Identify constant columns (one unique value per group)
  constant_cols <- other_cols[sapply(other_cols, function(col) {
    df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) %>%
      dplyr::summarise(n_unique = dplyr::n_distinct(.data[[col]]), .groups = "drop") %>%
      dplyr::pull(n_unique) %>%
      max() == 1
  })]

  varying_cols <- setdiff(other_cols, constant_cols)

  # Step 1: Assign visit numbers (e.g., visit 1, visit 2...) per patient
  df_visits <- df %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(key_cols))) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key_col1))) %>%
    dplyr::arrange(.data[[key_col2]]) %>%
    dplyr::mutate(visit_number = dplyr::row_number()) %>%
    dplyr::ungroup()

  # Join back to original data to add visit_number
  df <- df %>%
    dplyr::left_join(df_visits, by = key_cols)

  # Extract constant values
  df_constants <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(constant_cols), ~ dplyr::first(.x)), .groups = "drop")

  # Expand varying columns with visit-aware suffixes
  expanded_list <- lapply(varying_cols, function(col) {
    df_temp <- df %>%
      dplyr::select(dplyr::all_of(key_col1), visit_number, dplyr::all_of(col)) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(key_col1, "visit_number")))) %>%
      dplyr::mutate(row_num = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(
        names_from = row_num,
        values_from = dplyr::all_of(col),
        names_prefix = paste0(col, "_")
      )

    # Rename columns to add visit suffix
    non_key_cols <- setdiff(names(df_temp), c(key_col1, "visit_number"))
    names(df_temp)[names(df_temp) %in% non_key_cols] <- paste0(non_key_cols, "_visit_", df_temp$visit_number[1])

    return(df_temp)
  })

  # Merge expanded varying columns by patient and visit number
  if (length(expanded_list) > 0) {
    df_varying <- purrr::reduce(expanded_list, dplyr::full_join, by = c(key_col1, "visit_number"))
  } else {
    df_varying <- df_visits
  }

  # Merge with constant columns
  df_constants <- df_constants %>%
    dplyr::left_join(df_visits, by = key_cols)

  df_final <- dplyr::full_join(df_constants, df_varying, by = c(key_col1, "visit_number"))

  return(df_final)
}
