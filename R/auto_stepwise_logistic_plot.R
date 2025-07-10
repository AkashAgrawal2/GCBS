#' Automatic Stepwise Multiple Logistic Regression with ROC Curve and Predicted Plot
#'
#' Fits a multiple logistic regression (glm) on the binary outcome and given predictors,
#' with optional stepwise variable selection. Plots predicted probability and ROC curve.
#'
#' @param data Data frame or tibble.
#' @param outcome Name of binary outcome variable (0/1 or 2-level factor; character).
#' @param predictors Character vector of predictors.
#' @param stepwise Logical; if TRUE, stepwise model selection by AIC/BIC.
#' @param direction Stepwise direction ("both", "backward", "forward"); default: "both".
#' @param criterion "AIC" (default) or "BIC".
#' @param add_roc Logical; add ROC curve plot.
#' @return Invisible list with model, summary, predictors, plot(s).
#' @examples
#' set.seed(2)
#' df <- tibble::tibble(
#'   y = factor(rbinom(100, 1, 0.3)),
#'   x1 = rnorm(100),
#'   x2 = sample(letters[1:3], 100, replace = TRUE)
#' )
#' auto_stepwise_logistic_plot(df, outcome = "y", predictors = c("x1", "x2"))
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth geom_hline theme_minimal labs
#' @export
auto_stepwise_logistic_plot <- function(
    data,
    outcome,
    predictors,
    stepwise = TRUE,
    direction = c("both", "backward", "forward"),
    criterion = c("AIC", "BIC"),
    add_roc = TRUE
) {
  if (!requireNamespace("pROC", quietly = TRUE)) stop("pROC package required for ROC plot.")
  direction <- match.arg(direction)
  criterion <- match.arg(criterion)
  data <- tibble::as_tibble(data)
  if (!(outcome %in% names(data))) stop("Outcome not found.")
  if (any(!predictors %in% names(data))) stop("Some predictors not found.")
  # Convert outcome to binary 0/1
  if (is.factor(data[[outcome]])) {
    lv <- levels(data[[outcome]])
    if (length(lv) != 2) stop("Outcome factor must have two levels.")
    data[[outcome]] <- as.integer(data[[outcome]] == lv[2])
  }
  if (!all(data[[outcome]] %in% c(0, 1))) stop("Outcome must be 0/1 or 2-level factor.")
  fm <- stats::as.formula(paste(outcome, "~", paste(predictors, collapse = "+")))
  fit0 <- stats::glm(fm, data=data, family=binomial)
  # Stepwise if needed
  if (stepwise) {
    penalty <- if (criterion == "AIC") 2 else log(nrow(data))
    fit <- stats::step(fit0, direction=direction, k=penalty, trace=0)
  } else {
    fit <- fit0
  }
  fit_sum <- summary(fit)
  best_predictors <- names(coef(fit))[-1]
  # Fitted probabilities
  prob <- predict(fit, type="response")
  data$.fitted <- prob
  # Plot: Fitted probability by actual outcome
  p1 <- ggplot2::ggplot(data, ggplot2::aes(x = .fitted, fill = factor(!!rlang::sym(outcome)))) +
    ggplot2::geom_histogram(alpha = 0.6, bins = 30, position = "identity") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Predicted probabilities by actual outcome",
                  x = "Predicted Pr(Y=1)",
                  fill = "Actual")
  print(p1)
  roc_obj <- NULL
  p2 <- NULL
  if (add_roc) {
    roc_obj <- pROC::roc(response = data[[outcome]], predictor = prob)
    auc <- pROC::auc(roc_obj)
    # ROC plot
    p2 <- ggplot2::ggplot(data.frame(fpr = rev(roc_obj$specificities),
                                     tpr = rev(roc_obj$sensitivities)),
                          ggplot2::aes(x = 1 - fpr, y = tpr)) +
      ggplot2::geom_line(color = "blue") +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey") +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = sprintf("ROC Curve (AUC = %.3f)", auc),
                    x = "False Positive Rate",
                    y = "True Positive Rate")
    print(p2)
  }
  print(fit_sum)
  invisible(list(
    model = fit,
    summary = fit_sum,
    selected_predictors = best_predictors,
    predicted_prob_plot = p1,
    roc_plot = p2,
    auc = if (!is.null(roc_obj)) pROC::auc(roc_obj) else NULL
  ))
}
