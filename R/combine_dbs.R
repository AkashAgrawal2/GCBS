#' Combine Two Dataframes by Reference Column
#'
#' This function performs a full join on two dataframes by a shared key, then identifies unmatched rows.
#'
#' @param df1 First dataframe.
#' @param df2 Second dataframe.
#' @param key Name of the column to join on (as a string).
#'
#' @return A combined dataframe or a list with matched and unmatched dataframes.
#' @export
combine_dbs <- function(df1, df2, key) {
  df_combined <- full_join(df1, df2, by = key)

  df_full_cases <- inner_join(df1, df2, by = key)

  df_missing <- anti_join(df_combined, df_full_cases, by = key)

  len <- length(df_missing[[key]])

  if (len > 0) {
    cat("Number of rows from", deparse(substitute(df1)), "that were not able to be matched to", deparse(substitute(df2)), ":", len, "\n")

    input <- NULL
    print("Success")
    cat("\nWould you like a list of the removed/unmatched rows?\n")
    input <- readline("> ")

    if (tolower(input) == "yes" | tolower(input) == "y") {
      dataframes <- list(df_full_cases, df_combined, df_missing)

      print("Both the matched and unmatched rows were returned.")
      print("To access the dataframe with matched rows, access the first item in the list that was returned using the following command: new_variable <- your_variable_name[[1]].")
      print("To access the dataframe with ALL rows, access the second item in the list that was returned using the following command: new_variable <- your_variable_name[[2]].")
      print("To access the dataframe with UNmatched rows, access the third item in the list that was returned using the following command: new_variable <- your_variable_name[[3].")
      cat("\nExample: \nIf my function call was: \n\ndata_combined <- combine_dbs(data1, data2, key) \n\nThen to get the matched rows I would call: \n\ndata_matched rows <- data_combined[[1]] \nFor unmatched rows: \n\ndata_unmatched rows <- data_combined[[3]]")

      return(dataframes)
    } else {
      cat("Only the matched rows were returned")
      return(df_full_cases)
    }
  }
  return(df_full_cases)
}
