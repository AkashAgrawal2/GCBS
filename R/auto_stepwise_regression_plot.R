#' Automatic Stepwise Multiple Linear Regression with Diagnostics Plots
#'
#' Fits a multiple linear regression (lm) on the specified outcome and predictors,
#' with optional stepwise model selection. Visualizes fitted vs observed and residuals.
#'
#' @param data Data frame or tibble.
#' @param outcome Name of numeric outcome variable (character).
#' @param predictors Character vector of predictors (can mix numeric/factor).
#' @param stepwise Logical; if TRUE, performs backward/forward stepwise selection (default TRUE).
#' @param direction Direction for stepwise: "both" (default), "backward", or "forward".
#' @param criterion "AIC" (default) or "BIC" (use for penalizing model size more).
#' @param add_diagnostics Logical; if TRUE, shows fitted vs actual and residual plots.
#' @return Invisible list with model, summary, selected predictors, and plots.
#' @examples
#' df <- tibble::tibble(
#'   y = rnorm(100),
#'   x1 = rnorm(100),
#'   x2 = runif(100),
#'   x3 = sample(letters[1:3], 100, replace = TRUE)
#' )
#' auto_stepwise_regression_plot(df, outcome = "y", predictors = c("x1", "x2", "x3"))
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth geom_hline theme_minimal labs
#' @export
auto_stepwise_regression_plot <- function(
    data,
    outcome,
    predictors,
    stepwise = TRUE,
    direction = c("both", "backward", "forward"),
    criterion = c("AIC", "BIC"),
    add_diagnostics = TRUE
) {
  direction <- match.arg(direction)
  criterion <- match.arg(criterion)
  data <- tibble::as_tibble(data)
  if (!(outcome %in% names(data))) stop("Outcome not found.")
  if (any(!predictors %in% names(data))) stop("Some predictors not found.")
  fm <- stats::as.formula(paste(outcome, "~", paste(predictors, collapse = "+")))
  # Initial linear model
  fit0 <- stats::lm(fm, data = data)
  # Stepwise model selection
  if (stepwise) {
    penalty <- if (criterion == "AIC") 2 else log(nrow(data))
    fit <- stats::step(fit0, direction = direction, k = penalty, trace = 0)
  } else {
    fit <- fit0
  }
  fit_sum <- summary(fit)
  best_predictors <- names(coef(fit))[-1]
  fitted <- fit$fitted.values
  # Diagnostics plots
  if (add_diagnostics) {
    # Fitted vs actual
    p1 <- ggplot2::ggplot(data.frame(actual = data[[outcome]], fitted = fitted),
                          ggplot2::aes(x = actual, y = fitted)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "lm", se = FALSE, linetype = 2, color = "red") +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Fitted vs Actual", x = "Actual", y = "Fitted")
    print(p1)

    # Residuals vs fitted
    resid <- fit$residuals
    p2 <- ggplot2::ggplot(data.frame(fitted = fitted, resid = resid),
                          ggplot2::aes(x = fitted, y = resid)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, linetype = 2) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Residuals vs Fitted", x = "Fitted", y = "Residuals")
    print(p2)
  }
  print(fit_sum)
  invisible(list(
    model = fit,
    summary = fit_sum,
    selected_predictors = best_predictors,
    fitted_plot = if (add_diagnostics) p1 else NULL,
    residual_plot = if (add_diagnostics) p2 else NULL
  ))
}
