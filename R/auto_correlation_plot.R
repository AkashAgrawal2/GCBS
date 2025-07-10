#' Automatic Correlation Analysis with Plot
#'
#' Computes and plots Pearson correlation between two columns, with regression line.
#'
#' @param data A data frame or tibble.
#' @param x Name of x variable (character).
#' @param y Name of y variable (character).
#' @return Invisible list with cor.test result and plot.
#' @examples
#' df <- tibble::tibble(x = rnorm(30), y = 2*x + rnorm(30))
#' auto_correlation_plot(df, x = "x", y = "y")
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth labs theme_minimal annotate
#' @export
auto_correlation_plot <- function(data, x, y) {
  data <- tibble::as_tibble(data)
  if (!(x %in% names(data))) stop("x column not found."); if (!(y %in% names(data))) stop("y column not found.")
  ct <- cor.test(data[[x]], data[[y]], method = "pearson")
  title_line <- sprintf("Pearson r = %.3f, p = %.4f", ct$estimate, ct$p.value)
  library(ggplot2)
  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, color = "blue") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = title_line,
      subtitle = sprintf("t = %.3f, df = %d", ct$statistic, ct$parameter)
    ) +
    ggplot2::annotate(
      "text",
      x = Inf, y = Inf,
      hjust = 1.1, vjust = 2,
      label = if (ct$p.value < 0.001) "p < 0.001" else paste0("p = ", format(round(ct$p.value,4), nsmall=4)),
      size = 5, color = "red"
    )
  print(p)
  print(ct)
  invisible(list(correlation = ct, plot = p))
}
