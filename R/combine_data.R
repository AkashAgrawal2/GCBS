#' Combine Two Dataframes by One or Two Key Columns
#'
#' Performs a full join on two dataframes using one or two shared key columns,
#' identifies matched and unmatched rows, and optionally returns all three sets.
#'
#' Identical columns (other than keys) are merged to avoid duplicate `.x` / `.y` suffixes.
#'
#' @param df1 First dataframe.
#' @param df2 Second dataframe.
#' @param key_cols A character vector of one or two column names used for joining.
#'
#' @return Either a dataframe of matched rows or a list containing matched rows,
#' all rows (full join), and unmatched rows from \code{df1}.
#'
#' @import dplyr
#' @importFrom tidyr drop_na
#' @export
combine_data <- function(df1, df2, key_cols) {
  # Safety: ensure key_cols is character vector of length 1 or 2
  if (!(length(key_cols) %in% c(1, 2))) {
    stop("`key_cols` must be a character vector of length 1 or 2.")
  }

  # Perform full join
  df_combined <- dplyr::full_join(df1, df2, by = key_cols, suffix = c(".x", ".y"))

  # Identify identical non-key columns (not duplicated during merge)
  shared_cols <- intersect(setdiff(names(df1), key_cols), setdiff(names(df2), key_cols))

  # For identical columns: remove .y version and rename .x back
  for (col in shared_cols) {
    col_x <- paste0(col, ".x")
    col_y <- paste0(col, ".y")
    if (col_x %in% names(df_combined) && col_y %in% names(df_combined)) {
      same_values <- dplyr::coalesce(df_combined[[col_x]], df_combined[[col_y]])
      df_combined[[col]] <- same_values
      df_combined[[col_x]] <- NULL
      df_combined[[col_y]] <- NULL
    }
  }

  # Remove remaining .y columns (conflicting columns from df2)
  df_combined <- df_combined %>% dplyr::select(-dplyr::matches("\\.y$"))

  # Get matched rows (inner join)
  df_full_cases <- dplyr::inner_join(df1, df2, by = key_cols)

  # Get unmatched rows (rows in full join not in inner join)
  df_missing <- dplyr::anti_join(df_combined, df_full_cases, by = key_cols)

  len <- nrow(df_missing)

  if (len > 0) {
    cat("Number of rows from", deparse(substitute(df1)), "not matched in", deparse(substitute(df2)), ":", len, "\n")

    input <- NULL
    cat("\nWould you like a list of the removed/unmatched rows? (Enter yes/no)\n")
    input <- readline("> ")

    if (tolower(input) %in% c("yes", "y")) {
      dataframes <- list(
        matched = df_full_cases,
        combined = df_combined,
        unmatched = df_missing
      )

      cat("Matched, all, and unmatched rows returned in a list:\n")
      cat("  [[1]] matched rows\n")
      cat("  [[2]] full combined dataset\n")
      cat("  [[3]] unmatched rows from df1\n")
      return(dataframes)
    } else {
      cat("Only the matched rows were returned.\n")
      return(df_full_cases)
    }
  }

  return(df_full_cases)
}
