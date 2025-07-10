#' Automatic One-way ANOVA with Plot
#'
#' Runs one-way ANOVA for a numeric variable by group, and makes a plot including group means and (optionally) post-hoc results.
#'
#' @param data A data frame or tibble.
#' @param value Numeric column (character).
#' @param group Factor or character column as group.
#' @param plot_type "boxplot" or "violin". Default: "boxplot".
#' @param posthoc If TRUE, runs Tukey HSD and adds group significance letters.
#' @return Invisible list with ANOVA, Tukey (if any), and plot.
#' @examples
#' df <- tibble::tibble(
#'   group = rep(c("A", "B", "C"), each=20),
#'   score = c(rnorm(20, 50), rnorm(20, 60), rnorm(20, 70))
#' )
#' auto_anova_plot(df, value = "score", group = "group")
#'
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_violin geom_jitter labs theme_minimal annotate
#' @export
auto_anova_plot <- function(
    data,
    value,
    group,
    plot_type = c("boxplot", "violin"),
    posthoc = TRUE
) {
  plot_type <- match.arg(plot_type)
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 required.")
  if (posthoc && !requireNamespace("multcompView", quietly = TRUE)) stop("Install multcompView for posthoc group letters.")

  data <- tibble::as_tibble(data)
  if (!(value %in% names(data))) stop("Value column not found.")
  if (!(group %in% names(data))) stop("Group column not found.")
  anova_res <- aov(data[[value]] ~ as.factor(data[[group]]))
  anova_sum <- summary(anova_res)
  title_line <- sprintf("ANOVA F(%d,%d) = %.3f, p = %.4f",
                        anova_sum[[1]][["Df"]][1],
                        anova_sum[[1]][["Df"]][2],
                        anova_sum[[1]][["F value"]][1],
                        anova_sum[[1]][["Pr(>F)"]][1])
  tukey_res <- NULL
  significance_labels <- NULL
  if (posthoc) {
    tukey_res <- TukeyHSD(anova_res)
    # Generate group significance labels using multcompView
    library(multcompView)
    tk_tbl <- tukey_res[[1]][, "p adj", drop=FALSE]
    # names(tk_tbl) format "A-B"
    lvl <- levels(as.factor(data[[group]]))
    sig_letters <- multcompView::multcompLetters(tk_tbl)$Letters
    significance_labels <- data.frame(group = names(sig_letters), label = sig_letters)
  }
  library(ggplot2)
  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = group, y = value, fill = group)) +
    {
      if (plot_type == "boxplot") ggplot2::geom_boxplot(alpha=0.7)
      else ggplot2::geom_violin(alpha=0.7)
    } +
    ggplot2::geom_jitter(width = 0.2, alpha = 0.6, show.legend=FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = title_line,
      x = group, y = value
    )
  if (!is.null(significance_labels)) {
    p <- p + ggplot2::geom_text(
      data = significance_labels,
      aes(x = group, y = max(data[[value]], na.rm=TRUE), label = label),
      vjust = -0.6, color = "red", size = 5, inherit.aes = FALSE
    )
  }
  print(p)
  print(anova_sum)
  if (!is.null(tukey_res)) print(tukey_res)
  invisible(list(anova = anova_res, anova_summary = anova_sum, tukey = tukey_res, plot = p))
}
