#' Automatic t-test with Plot
#'
#' Performs an independent t-test of a numeric variable by group, and creates a plot with results.
#'
#' @param data A data frame or tibble.
#' @param value Numeric column to compare (character).
#' @param group Grouping variable (character or factor, with 2 levels).
#' @param var_equal Logical. Assume equal variances (defaults to FALSE).
#' @param plot_type Either "boxplot" (default) or "violin".
#' @return Invisible list with t-test result and the ggplot object; also prints both.
#' @examples
#' df <- tibble::tibble(
#'   group = rep(c("A", "B"), each=20),
#'   score = c(rnorm(20, mean=70), rnorm(20, mean=75))
#' )
#' auto_ttest_plot(df, value = "score", group = "group")
#'
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_violin geom_jitter labs theme_minimal annotate
#' @export
auto_ttest_plot <- function(
    data,
    value,
    group,
    var_equal = FALSE,
    plot_type = c("boxplot", "violin")
) {
  plot_type <- match.arg(plot_type)
  data <- tibble::as_tibble(data)
  if (!(value %in% names(data))) stop("Value column not found.")
  if (!(group %in% names(data))) stop("Group column not found.")
  data <- data[!is.na(data[[value]]) & !is.na(data[[group]]), ]
  if (length(unique(data[[group]])) != 2) stop("Grouping variable must have 2 groups.")
  t_res <- t.test(data[[value]] ~ data[[group]], var.equal = var_equal)
  title_line <- sprintf("t(%.2f) = %.3f, p = %.4f", t_res$parameter, t_res$statistic, t_res$p.value)
  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = group, y = value, fill = group)) +
    {
      if (plot_type == "boxplot") ggplot2::geom_boxplot(alpha = 0.6)
      else ggplot2::geom_violin(alpha = 0.6)
    } +
    ggplot2::geom_jitter(width = 0.15, alpha = 0.7, show.legend = FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = title_line,
                  subtitle = paste0("Means: ", round(t_res$estimate[1],2), ", ", round(t_res$estimate[2],2)),
                  y = value,
                  x = group) +
    ggplot2::annotate("text", x = 1.5, y = max(data[[value]], na.rm=TRUE),
                      label = if (t_res$p.value < 0.001) "p < 0.001" else paste0("p = ", format(round(t_res$p.value,4), nsmall=4)), size = 5)
  print(p)
  print(t_res)
  invisible(list(test = t_res, plot = p))
}
