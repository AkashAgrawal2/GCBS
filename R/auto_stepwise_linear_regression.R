#' Robust Stepwise Multi-Predictor Linear Regression with Diagnostics and Report
#'
#' Fits a linear regression model, with stepwise AIC/BIC selection, support for interactions, diagnostics (plots, VIF), and cross-validation.
#'
#' @param data A data frame or tibble.
#' @param outcome Name of numeric outcome variable.
#' @param predictors Character vector of predictor variable names.
#' @param interactions Logical: include all two-way interactions? Default FALSE.
#' @param stepwise Logical: Perform stepwise selection if TRUE (default).
#' @param direction "both" (default), "backward", or "forward".
#' @param criterion "AIC" (default) or "BIC".
#' @param crossval Logical; run k-fold cross-validation (default FALSE).
#' @param k Integer number of folds for CV (default 5, min 2).
#' @param add_diagnostics Logical; make diagnostic plots (default TRUE).
#' @param report Logical; print a markdown/HTML report (requires broom/knitr).
#' @return Named list: model, summary, selected predictors, plots, VIF, cross-validation results, and optionally a markdown report.
#' @examples
#' set.seed(3)
#' df <- tibble::tibble(
#'   y = 3 + 2*x1 + 0.2*x2 + ifelse(x3 == "b", 1, 0) + rnorm(100),
#'   x1 = rnorm(100), x2 = runif(100), x3 = sample(letters[1:3], 100, replace = TRUE)
#' )
#' auto_stepwise_linear_regression(df, "y", c("x1", "x2", "x3"), interactions = TRUE, crossval = TRUE)
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth geom_hline theme_minimal labs
#' @importFrom car vif
#' @importFrom boot cv.glm
#' @export
auto_stepwise_linear_regression <- function(
    data,
    outcome,
    predictors,
    interactions = FALSE,
    stepwise = TRUE,
    direction = c("both", "backward", "forward"),
    criterion = c("AIC", "BIC"),
    crossval = FALSE,
    k = 5,
    add_diagnostics = TRUE,
    report = FALSE
) {
  direction <- match.arg(direction)
  criterion <- match.arg(criterion)
  data <- tibble::as_tibble(data)
  if (!(outcome %in% names(data))) stop("Outcome not found.")
  if (any(!predictors %in% names(data))) stop("Some predictors not found.")
  rhs <- predictors
  if (interactions && length(predictors) > 1) {
    int_terms <- apply(utils::combn(predictors, 2), 2, function(x) paste(x, collapse=":"))
    rhs <- c(rhs, int_terms)
  }
  formula_str <- paste(outcome, "~", paste(rhs, collapse = "+"))
  fm <- stats::as.formula(formula_str)
  fit0 <- stats::lm(fm, data = data)
  if (stepwise) {
    penalty <- if (criterion == "AIC") 2 else log(nrow(data))
    fit <- stats::step(fit0, direction = direction, k = penalty, trace = 0)
  } else {
    fit <- fit0
  }
  fit_sum <- summary(fit)
  best_predictors <- names(coef(fit))[-1]
  has_broom <- requireNamespace("broom", quietly=TRUE)
  # Coefficients with 95% CI
  if (has_broom) {
    coefs <- broom::tidy(fit, conf.int = TRUE)
  } else {
    se <- sqrt(diag(vcov(fit)))
    ci <- confint(fit)
    coefs <- data.frame(term=names(coef(fit)), estimate=coef(fit), std.error = se,
                        conf.low=ci[,1], conf.high=ci[,2])
  }
  print("Model coefficients (95% CI):")
  print(coefs, row.names = FALSE, digits=3)
  # VIF
  has_car <- requireNamespace("car", quietly = TRUE)
  vif_table <- if (has_car) tryCatch(car::vif(fit), error = function(e) NULL) else NULL
  # Cross-validation
  cv_out <- NULL
  if (crossval) {
    if (!requireNamespace("boot", quietly = TRUE)) stop("Install the 'boot' package for cross-validation.")
    set.seed(1)
    cv_out <- boot::cv.glm(data, fit, K = max(k, 2))
  }
  # Diagnostics
  p1 <- p2 <- NULL
  if (add_diagnostics) {
    p1 <- ggplot2::ggplot(data.frame(actual = data[[outcome]], fitted = fit$fitted.values),
                          ggplot2::aes(x = actual, y = fitted)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "lm", se = FALSE, linetype = 2, color = "red") +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Fitted vs Actual", x = "Actual", y = "Fitted")
    p2 <- ggplot2::ggplot(data.frame(fitted = fit$fitted.values, resid = fit$residuals),
                          ggplot2::aes(x = fitted, y = resid)) +
      ggplot2::geom_point() + ggplot2::geom_hline(yintercept = 0, linetype = 2, color = "gray60") +
      ggplot2::theme_minimal() + ggplot2::labs(title = "Residuals vs Fitted", x = "Fitted", y = "Residuals")
    print(p1); print(p2)
  }
  # Report
  report_md <- NULL
  if (report) {
    if (!requireNamespace("broom", quietly = TRUE) || !requireNamespace("knitr", quietly = TRUE)) stop("Install 'broom' and 'knitr' for report.")
    knit <- knitr::kable(coefs, caption = "Regression coefficients (with 95% CI)")
    residsum <- sprintf("Residual std error: %.3f, Adj. R2: %.3f", fit_sum$sigma, fit_sum$adj.r.squared)
    vif_txt <- if (!is.null(vif_table)) knitr::kable(as.data.frame(vif_table), caption = "VIF Table") else "VIF: car not installed"
    cv_txt <- if (!is.null(cv_out)) sprintf("CV MSE: %.3f", cv_out$delta[1]) else ""
    report_md <- paste0("## Model summary \n\n", knit, "\n\n", residsum, "\n\n", vif_txt, "\n", cv_txt)
    cat(report_md)
  }
  res <- list(
    model = fit, summary = fit_sum, selected_predictors = best_predictors,
    fit_plot = p1, residual_plot = p2,
    coefficients = coefs, vif = vif_table,
    cross_validation = cv_out,
    report_md = report_md
  )
  invisible(res)
}
