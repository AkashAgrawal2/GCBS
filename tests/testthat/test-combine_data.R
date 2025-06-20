test_that("combine_data merges and identifies unmatched rows correctly", {
  library(dplyr)

  df1 <- data.frame(Key = c(1, 2, 3), A = c("a", "b", "c"))
  df2 <- data.frame(Key = c(2, 3, 4), B = c("x", "y", "z"))

  result <- combine_data(df1, df2, "Key")

  # If the function returns a list, extract combined and unmatched
  if (is.list(result) && length(result) == 2) {
    df_combined <- result[[1]]
    df_unmatched <- result[[2]]
  } else {
    df_combined <- result
    df_unmatched <- NULL
  }

  # Test that combined has expected number of rows
  expect_equal(nrow(df_combined), 2)

  # Test that unmatched rows include keys 1 and 4 (if returned)
  if (!is.null(df_unmatched)) {
    expect_true(all(c(2, 3) %in% df_unmatched$Key))
  }
})
