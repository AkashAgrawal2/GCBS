#' Impute Missing Values in a Data Frame
#'
#' Impute missing values in specified columns of a data frame using the mean, median, mode, or a constant value.
#' Allows specifying different imputation methods (and constants) for different columns.
#' Returns the imputed data frame (as a tibble), with details of changes as an attribute.
#'
#' @param data A data frame or tibble.
#' @param impute_map A named list where each name is a column to impute and each value is either:
#'   - a string (`"mean"`, `"median"`, `"mode"`, or `"constant"`) for method,
#'   - or a list of the form `list(method = "constant", constant = 5)`, etc.
#'   If NULL (default), will impute all columns with missing data using `"mean"` for numerics or `"mode"` otherwise.
#' @return Imputed tibble. Returns details of imputation as the `"impute_summary"` attribute.
#' @examples
#' df <- data.frame(
#'   age = c(20, NA, 22, 24, NA, 21),
#'   gender = c("M", "F", NA, "F", "F", "M"),
#'   income = c(50000, 45000, NA, 48000, 47000, NA)
#' )
#' impute_missing(df)
#' impute_missing(df, impute_map = list(
#'   age = list(method = "constant", constant = 30),
#'   gender = "mode",
#'   income = "median"
#' ))
#' View summary of what was imputed:
#' attr(result, "impute_summary")
#'
#' @importFrom tibble as_tibble
#' @importFrom rlang .data
#' @export
impute_missing <- function(data, impute_map = NULL) {
  # Helper functions
  mode_fun <- function(x) {
    ux <- unique(na.omit(x))
    ux[which.max(tabulate(match(x, ux)))]
  }

  get_default_method <- function(x) {
    if (is.numeric(x)) {
      "mean"
    } else {
      "mode"
    }
  }

  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("The 'tibble' package is required for this function.")
  }

  # Convert to tibble for consistency
  data <- tibble::as_tibble(data)

  if (is.null(impute_map)) {
    # Infer columns with missing, and assign default method
    cols_na <- names(data)[colSums(is.na(data)) > 0]
    impute_map <- lapply(data[cols_na], get_default_method)
    names(impute_map) <- cols_na
  }

  # Normalize impute_map to a standard list format
  impute_setup <- list()
  for (col in names(impute_map)) {
    val <- impute_map[[col]]
    if (is.character(val)) {
      impute_setup[[col]] <- list(method = val)
    } else if (is.list(val)) {
      impute_setup[[col]] <- val
    } else {
      stop("impute_map values must be strings or lists.")
    }
  }

  # Prepare summary
  impute_summary <- list()

  # Impute
  for (col in names(impute_setup)) {
    if (!(col %in% names(data))) {
      warning(paste0("Column '", col, "' not found; skipping."))
      next
    }
    na_idx <- which(is.na(data[[col]]))
    if (length(na_idx) == 0) next

    method <- impute_setup[[col]]$method
    if (is.null(method)) stop("Method not specified for column: ", col)
    method <- match.arg(method, choices = c("mean", "median", "mode", "constant"))

    if (method == "mean") {
      if (!is.numeric(data[[col]])) next
      impute_value <- mean(data[[col]], na.rm = TRUE)
    } else if (method == "median") {
      if (!is.numeric(data[[col]])) next
      impute_value <- median(data[[col]], na.rm = TRUE)
    } else if (method == "mode") {
      impute_value <- mode_fun(data[[col]])
    } else if (method == "constant") {
      if (is.null(impute_setup[[col]]$constant))
        stop("Constant value must be supplied for method 'constant' in column: ", col)
      impute_value <- impute_setup[[col]]$constant
    }
    # Record summary BEFORE imputation
    impute_summary[[col]] <- data.frame(
      row = na_idx,
      old_value = NA,
      new_value = impute_value,
      method = method,
      stringsAsFactors = FALSE
    )

    data[[col]][na_idx] <- impute_value
  }
  attr(data, "impute_summary") <- impute_summary
  return(data)
}

