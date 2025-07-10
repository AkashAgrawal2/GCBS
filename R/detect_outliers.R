#' Detect and Optionally Plot Outliers in Numeric Columns
#'
#' Identifies outliers in specified numeric columns of a data frame using the z-score (default), IQR,
#' or a custom function. Optionally produces plots visualizing outliers in each column.
#'
#' @param data A data frame or tibble.
#' @param columns Character vector of column names to check for outliers. Default: all numeric columns.
#' @param method Outlier detection method: \code{"zscore"} (default), \code{"iqr"}, or a custom function.
#' @param threshold Numeric. For z-score, absolute value above which a point is flagged (default: 3). For IQR, multiplier for IQR (default: 1.5).
#' @param plot Logical. Whether to plot the results for each `columns`. Default: FALSE.
#' @param plot_type "boxplot" or "scatter" (default "boxplot").
#' @param point_labels Optional. Column name (character) to use as plot point labels.
#' @param quiet Logical. If TRUE, suppresses all messages/warnings.
#' @return The input tibble with additional \code{*_outlier} logical columns. Outlier summary is returned as the "outlier_summary" attribute.
#' @examples
#' df <- tibble::tibble(
#'   id = letters[1:6],
#'   height = c(160, 165, 170, 171, 168, 250),
#'   weight = c(60, 65, 62, 1000, 64, 61)
#' )
#' flagged <- detect_outliers(df, plot = TRUE, point_labels = "id")
#'
#' @importFrom tibble as_tibble
#' @importFrom ggplot2 ggplot aes geom_point geom_jitter geom_boxplot scale_color_manual labs theme_minimal geom_text
#' @export
detect_outliers <- function(
    data,
    columns = NULL,
    method = c("zscore", "iqr"),
    threshold = NULL,
    plot = TRUE,
    plot_type = c("boxplot", "scatter"),
    point_labels = NULL,
    quiet = FALSE
) {
  if (!requireNamespace("tibble", quietly = TRUE)) stop("The 'tibble' package is required.")
  if (plot && !requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required for plotting.")
  }
  data <- tibble::as_tibble(data)
  plot_type <- match.arg(plot_type)

  # Pick columns
  if (is.null(columns)) {
    columns <- names(which(sapply(data, is.numeric)))
  }
  if (length(columns) == 0) stop("No numeric columns specified or found for outlier detection.")

  # Setup method
  if (is.function(method)) {
    method_fun <- method
    method_name <- "custom"
  } else {
    method <- match.arg(method)
    method_name <- method
    if (is.null(threshold)) {
      threshold <- if (method == "zscore") 3 else 1.5
    }
    method_fun <- switch(method,
                         zscore = function(x, threshold) {
                           if (!is.numeric(x)) return(rep(NA, length(x)))
                           z <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
                           abs(z) > threshold
                         },
                         iqr = function(x, threshold) {
                           if (!is.numeric(x)) return(rep(NA, length(x)))
                           Q1 <- quantile(x, 0.25, na.rm = TRUE)
                           Q3 <- quantile(x, 0.75, na.rm = TRUE)
                           IQR_val <- Q3 - Q1
                           (x < (Q1 - threshold * IQR_val)) | (x > (Q3 + threshold * IQR_val))
                         }
    )
  }

  # Internal plotting helper
  plot_numeric_outliers_internal <- function(data, column, outlier_col, plot_type, point_labels) {
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x = if(plot_type == "boxplot") "factor('')" else column, y = column))
    if (plot_type == "boxplot") {
      p <- p + ggplot2::geom_boxplot(outlier.shape = NA)
      p <- p + ggplot2::geom_jitter(ggplot2::aes_string(color = outlier_col), width = 0.2, height = 0)
    } else {
      p <- p + ggplot2::geom_point(ggplot2::aes_string(color = outlier_col))
    }
    p <- p + ggplot2::scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'red'))
    if (!is.null(point_labels) && (point_labels %in% names(data))) {
      p <- p +
        ggplot2::geom_text(
          ggplot2::aes_string(label = point_labels),
          data = data[data[[outlier_col]] == TRUE & !is.na(data[[outlier_col]]), ],
          vjust = -0.4, color = "red", size = 3
        )
    }
    p <- p +
      ggplot2::labs(
        title = paste("Outlier visualization for", column),
        color = "Outlier"
      ) +
      ggplot2::theme_minimal()
    print(p)
    invisible(p)
  }

  # Main detection
  outlier_summary <- list()
  for (col in columns) {
    if (!(col %in% names(data))) {
      warning(sprintf("Column '%s' not found in data; skipping.", col))
      next
    }
    if (!is.numeric(data[[col]])) {
      warning(sprintf("Column '%s' is not numeric; skipping.", col))
      next
    }
    out_flag <- method_fun(data[[col]], threshold)
    flag_col <- paste0(col, "_outlier")
    data[[flag_col]] <- out_flag
    outlier_rows <- which(isTRUE(out_flag))
    outlier_summary[[col]] <- data.frame(
      column = col,
      method = method_name,
      n_outliers = length(outlier_rows),
      outlier_rows = paste(outlier_rows, collapse = ","),
      stringsAsFactors = FALSE
    )
    if (!quiet && length(outlier_rows) > 0) {
      message(sprintf("In column '%s', %d outlier%s detected using %s.",
                      col, length(outlier_rows), ifelse(length(outlier_rows) != 1, "s", ""), method_name))
    }
    if (plot) {
      plot_numeric_outliers_internal(
        data = data,
        column = col,
        outlier_col = flag_col,
        plot_type = plot_type,
        point_labels = point_labels
      )
    }
  }


  print("Use the remove_outliers function to remove these outliers! Here is the basic syntax: remove_outliers(data, c(outlier_col_names)")

  attr(data, "outlier_summary") <- do.call(rbind, outlier_summary)
  return(data)
}
