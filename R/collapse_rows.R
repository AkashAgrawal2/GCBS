#' Collapse Rows by Multiple Keys into New Columns
#'
#' Collapses multiple rows with the same combination of key columns into a single row,
#' expanding varying values across multiple new columns. Columns that are constant
#' within each key group are kept as single columns. Columns with multiple distinct
#' values are expanded with suffixes (_1, _2, ...).
#'
#' @param df A data frame that may contain repeated rows per combination of keys.
#' @param key_cols A character vector of key columns that define groups (e.g., c("Primprocid")).
#' @param expand_order One of c("append","grouped"). If "append" (default), expanded
#'   columns are interleaved by index: a_1, b_1, a_2, b_2, ...
#'   If "grouped", expanded columns are grouped by base name: a_1, a_2, b_1, b_2, ...
#'
#' @return A data frame with one row per key combination; constant columns remain single,
#'   varying columns are widened with numeric suffixes.
#'
#' @examples
#' df <- data.frame(
#'   patient_id = c(1,1,1, 1,1,1),
#'   visit_date = c("2024-01-01","2024-01-01","2024-01-01",
#'                  "2024-02-01","2024-02-01","2024-02-01"),
#'   name = rep("Alice", 6),
#'   lab  = c("A","B","C","A","B","C"),
#'   val  = c(10,12,14, 8,9,10)
#' )
#' collapse_rows(df, key_cols = c("patient_id","visit_date"), expand_order = "append")
#' collapse_rows(df, key_cols = c("patient_id","visit_date"), expand_order = "grouped")
#'
#' @importFrom dplyr select group_by mutate ungroup summarise full_join across pull n_distinct first
#' @importFrom dplyr all_of row_number
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#' @importFrom purrr reduce
#' @export
collapse_rows <- function(df, key_cols, expand_order = c("append","grouped")) {
  expand_order <- match.arg(expand_order)
  other_cols   <- setdiff(names(df), key_cols)

  # Identify constant vs varying columns
  constant_cols <- other_cols[sapply(other_cols, function(col) {
    df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) %>%
      dplyr::summarise(n_unique = dplyr::n_distinct(.data[[col]]), .groups = "drop") %>%
      dplyr::pull(n_unique) %>%
      max() == 1
  })]
  varying_cols <- setdiff(other_cols, constant_cols)

  # Extract constants
  df_constants <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(constant_cols), ~ dplyr::first(.x)), .groups = "drop")

  # Expand each varying column separately
  expanded_list <- lapply(varying_cols, function(col) {
    df %>%
      dplyr::select(dplyr::all_of(key_cols), dplyr::all_of(col)) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) %>%
      dplyr::mutate(row_num = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(
        names_from  = row_num,
        values_from = dplyr::all_of(col),
        names_prefix = paste0(col, "_")
      )
  })

  # Merge expanded pieces and join constants
  if (length(expanded_list) > 0) {
    df_varying <- purrr::reduce(expanded_list, dplyr::full_join, by = key_cols)
    df_final   <- dplyr::full_join(df_constants, df_varying, by = key_cols)
  } else {
    df_final   <- df_constants
  }

  # Reorder columns per expand_order
  # Keep keys first, then constants, then expanded varying columns in chosen order
  all_names   <- names(df_final)
  key_names   <- key_cols
  const_names <- setdiff(intersect(all_names, constant_cols), key_names)

  # Detect expanded columns like "<base>_<number>"
  # Only for bases present in varying_cols to avoid accidental matches
  pattern <- paste0("^(", paste0(varying_cols, collapse = "|"), ")_([0-9]+)$")
  is_expanded <- grepl(pattern, all_names)

  expanded_names <- all_names[is_expanded]
  if (length(expanded_names) > 0) {
    # Parse base and index
    base <- sub(pattern, "\\1", expanded_names)
    idx  <- as.integer(sub(pattern, "\\2", expanded_names))

    # Order logic
    if (expand_order == "append") {
      # Interleave by index, then preserve original varying_cols order for ties
      ord <- order(
        idx,
        match(base, varying_cols),
        expanded_names,
        na.last = TRUE
      )
    } else { # "grouped"
      # Group by base (in original varying order), then by index
      ord <- order(
        match(base, varying_cols),
        idx,
        expanded_names,
        na.last = TRUE
      )
    }
    expanded_names <- expanded_names[ord]
  }

  new_order <- c(key_names, const_names, expanded_names)
  # Include any remaining columns (unlikely, but safe)
  new_order <- unique(c(new_order, all_names))

  df_final[, new_order, drop = FALSE]
}
