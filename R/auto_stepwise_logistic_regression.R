#' Robust Stepwise Multi-Predictor Logistic Regression with ROC, Diagnostics, and Report
#'
#' Fits logistic regression (glm) with stepwise selection, supports interactions, VIF, cross-validation, and report.
#'
#' @param data Data frame or tibble.
#' @param outcome Name of binary outcome variable (0/1 or two-level factor).
#' @param predictors Character vector of predictors.
#' @param interactions Logical: include all two-way interactions? Default FALSE.
#' @param stepwise Use stepwise selection? Default TRUE.
#' @param direction "both" (default), "backward", or "forward".
#' @param criterion "AIC" or "BIC" (default AIC).
#' @param crossval Logical; run k-fold cross-validation for AUC (default FALSE).
#' @param k Folds for CV (default 5).
#' @param add_roc Logical: show ROC curve (default TRUE).
#' @param report Logical: print HTML summary (broom & knitr needed).
#' @return Named list with model, summary, selected predictors, plots, VIF, cross-validation, report markdown.
#' @examples
#' set.seed(3)
#' df <- tibble::tibble(
#'   y = factor(rbinom(150, 1, 0.4)),
#'   x1 = rnorm(150),
#'   x2 = runif(150),
#'   x3 = sample(letters[1:3], 150, replace = TRUE)
#' )
#' auto_stepwise_logistic_regression(df, "y", c("x1", "x2", "x3"), interactions = TRUE, crossval = TRUE)
#'
#' @importFrom ggplot2 ggplot aes geom_histogram geom_line geom_abline theme_minimal labs
#' @importFrom pROC roc auc
#' @importFrom car vif
#' @importFrom boot cv.glm
#' @export
auto_stepwise_logistic_regression <- function(
    data,
    outcome,
    predictors,
    interactions = FALSE,
    stepwise = TRUE,
    direction = c("both", "backward", "forward"),
    criterion = c("AIC", "BIC"),
    crossval = FALSE,
    k = 5,
    add_roc = TRUE,
    report = FALSE
) {
  direction <- match.arg(direction)
  criterion <- match.arg(criterion)
  data <- tibble::as_tibble(data)
  if (!(outcome %in% names(data))) stop("Outcome not found.")
  if (any(!predictors %in% names(data))) stop("Some predictors not found.")
  if (is.factor(data[[outcome]])) {
    lv <- levels(data[[outcome]])
    if (length(lv) != 2) stop("Outcome must be 2-level factor.")
    data[[outcome]] <- as.integer(data[[outcome]] == lv[2])
  }
  if (!all(data[[outcome]] %in% c(0,1))) stop("Outcome must be binary (0/1 or two-level factor).")
  rhs <- predictors
  if (interactions && length(predictors) > 1) {
    int_terms <- apply(utils::combn(predictors, 2), 2, function(x) paste(x, collapse=":"))
    rhs <- c(rhs, int_terms)
  }
  fm <- stats::as.formula(paste(outcome, "~", paste(rhs, collapse = "+")))
  fit0 <- stats::glm(fm, data=data, family=binomial)
  if (stepwise) {
    penalty <- if (criterion == "AIC") 2 else log(nrow(data))
    fit <- stats::step(fit0, direction=direction, k=penalty, trace=0)
  } else {
    fit <- fit0
  }
  fit_sum <- summary(fit)
  best_predictors <- names(coef(fit))[-1]
  # Odds ratios, CIs
  has_broom <- requireNamespace("broom", quietly=TRUE)
  if (has_broom) {
    coefs <- broom::tidy(fit, conf.int = TRUE, exponentiate = TRUE)
    names(coefs)[names(coefs) == "estimate"] <- "odds_ratio"
  } else {
    est <- coef(fit)
    se <- sqrt(diag(vcov(fit)))
    ci <- suppressWarnings(confint(fit))
    # Exponentiate everything
    or <- exp(est); ci_low <- exp(ci[,1]); ci_high <- exp(ci[,2])
    coefs <- data.frame(term=names(est), odds_ratio=or, std.error=se,
                        conf.low=ci_low, conf.high=ci_high)
  }
  print("Model odds ratios (95% CI):")
  print(coefs, row.names = FALSE, digits=3)
  # VIF
  has_car <- requireNamespace("car", quietly = TRUE)
  vif_table <- if (has_car) tryCatch(car::vif(fit), error = function(e) NULL) else NULL
  # CV AUC (see previous)
  cv_out <- NULL
  if (crossval) {
    if (!requireNamespace("boot", quietly = TRUE)) stop("package 'boot' required.")
    if (!requireNamespace("pROC", quietly = TRUE)) stop("package 'pROC' required for AUC.")
    set.seed(2)
    auc_cost <- function(actual, predicted) pROC::auc(pROC::roc(actual, predicted))
    cv_auc <- function(data, indices) {
      train <- data[indices, ]
      test <- data[-indices, ]
      mod <- stats::glm(fm, data=train, family=binomial)
      pred <- predict(mod, newdata = test, type="response")
      act <- test[[outcome]]
      auc_cost(act, pred)
    }
    folds <- split(seq_len(nrow(data)), sort(seq_len(nrow(data)) %% k))
    aucs <- mapply(function(idx) cv_auc(data, setdiff(seq_len(nrow(data)), idx)), folds)
    cv_out <- list(cv_auc = mean(unlist(aucs)), aucs = aucs)
  }
  # Plots
  prob <- predict(fit, type="response")
  data$.fitted <- prob
  p1 <- ggplot2::ggplot(data, ggplot2::aes(x = .fitted, fill = factor(!!rlang::sym(outcome)))) +
    ggplot2::geom_histogram(alpha=0.6, bins=30, position = "identity") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Predicted probabilities by actual outcome", x="Predicted Pr(Y=1)", fill="Actual")
  print(p1)
  roc_obj <- NULL; p2 <- NULL
  if (add_roc) {
    if (!requireNamespace("pROC", quietly = TRUE)) stop("Install 'pROC' for ROC analysis.")
    roc_obj <- pROC::roc(response=data[[outcome]], predictor=prob)
    auc <- as.numeric(pROC::auc(roc_obj))
    p2 <- ggplot2::ggplot(data.frame(fpr = 1 - roc_obj$specificities, tpr = roc_obj$sensitivities),
                          ggplot2::aes(x = fpr, y = tpr)) +
      ggplot2::geom_line(color = "blue") +
      ggplot2::geom_abline(intercept=0, slope=1, linetype=2, color="grey60") +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = sprintf("ROC Curve (AUC = %.3f)", auc),
                    x = "False Positive Rate", y = "True Positive Rate")
    print(p2)
  }
  # Report
  report_md <- NULL
  if (report) {
    if (!requireNamespace("broom", quietly = TRUE) || !requireNamespace("knitr", quietly = TRUE)) stop("Install 'broom' and 'knitr' for report.")
    knit <- knitr::kable(coefs, caption = "Odds ratios (95% CI)")
    auc_txt <- if (!is.null(roc_obj)) sprintf("ROC AUC: %.3f", as.numeric(pROC::auc(roc_obj))) else ""
    vif_txt <- if (!is.null(vif_table)) knitr::kable(as.data.frame(vif_table), caption = "VIF Table") else "VIF: car not installed"
    cv_txt <- if (!is.null(cv_out)) sprintf("CV mean AUC: %.3f", cv_out$cv_auc) else ""
    report_md <- paste0("## Model summary \n\n", knit, "\n\n", auc_txt, "\n", vif_txt, "\n", cv_txt)
    cat(report_md)
  }
  res <- list(
    model = fit, summary = fit_sum, selected_predictors = best_predictors,
    coefficients = coefs, vif = vif_table,
    pred_prob_plot = p1, roc_plot = p2,
    cross_validation = cv_out,
    report_md = report_md
  )
  invisible(res)
}
