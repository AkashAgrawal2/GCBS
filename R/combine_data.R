#' Combine Two Dataframes by Reference Column
#'
#' This function performs a full join on two dataframes by a shared key, then identifies unmatched rows.
#'
#' @param df1 First dataframe.
#' @param df2 Second dataframe.
#' @param key Name of the column to join on (as a string).
#'
#' @return A combined dataframe or a list with matched and unmatched dataframes.
#' @import dplyr
#' @importFrom tidyr drop_na
#' @export
combine_data <- function(df1, df2, key) {
  df_combined <- dplyr::full_join(df1, df2, by = key)

  df_full_cases <- dplyr::inner_join(df1, df2, by = key)

  df_missing <- dplyr::anti_join(df_combined, df_full_cases, by = key)

  len <- length(df_missing[[key]])

  if (len > 0) {
    cat("Number of rows from", deparse(substitute(df1)), "that were not able to be matched to", deparse(substitute(df2)), ":", len, "\n")

    input <- NULL
    print("Success")
    cat("\nWould you like a list of the removed/unmatched rows? (Enter yes/no)\n")
    input <- readline("> ")

    if (tolower(input) == "yes" | tolower(input) == "y") {
      dataframes <- list(df_full_cases, df_combined, df_missing)

      print("Both the matched and unmatched rows were returned.")
      print("A list was returned containing the dataframes with matched (1st index), all (2nd index), and unmatched rows (3rd index).")
      print("To access each individual dataframe, perform the following command: new_variable <- your_variable_name[[index]]")
      print("Replace index with the index of the dataframe you wish to access (Matched 1, All data 2, Unmatched 3)")
      cat("\nExample: \nIf my function call was: \ndata_combined <- combine_dbs(data1, data2, key) \n\nThen to get the matched rows I would call: \ndata_matched rows <- data_combined[[1]] \n\nFor unmatched rows: \ndata_unmatched rows <- data_combined[[3]]")

      return(dataframes)
    } else {
      cat("Only the matched rows were returned")
      return(df_full_cases)
    }
  }
  return(df_full_cases)
}
