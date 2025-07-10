#' Stepwise Multinomial Logistic Regression with Diagnostics and Report
#'
#' Fits a multinomial (nominal, >2-level) logistic regression with optional stepwise selection, VIF, cross-validation,
#' diagnostic plot of predicted class probabilities, and markdown report ready for teaching or publication.
#'
#' @param data Data frame or tibble.
#' @param outcome Name of categorical outcome variable (must have 3+ levels).
#' @param predictors Character vector of predictors.
#' @param interactions Logical; include all two-way interactions? Default FALSE.
#' @param stepwise Logical; use stepwise AIC/BIC selection? Default TRUE.
#' @param direction "both" (default), "backward", or "forward".
#' @param criterion "AIC" (default) or "BIC".
#' @param crossval Logical; perform k-fold cross-validation (default FALSE).
#' @param k Number of folds for CV (default 5).
#' @param report Logical; print an HTML/Markdown report if TRUE.
#' @return Named list with model, summary, coefficients (odds ratios and CIs), VIF, cross-validation results, plots, and markdown.
#' @examples
#' set.seed(101)
#' df <- tibble::tibble(
#'   y = factor(sample(c("A", "B", "C"), 120, replace=TRUE)),
#'   x1 = rnorm(120),
#'   x2 = runif(120),
#'   x3 = sample(letters[1:3], 120, replace=TRUE)
#' )
#' auto_multinom_logistic_regression(df, outcome = "y", predictors = c("x1", "x2", "x3"),
#'                      stepwise = FALSE, crossval = TRUE, k = 4)
#'
#' @importFrom nnet multinom
#' @importFrom ggplot2 ggplot aes geom_boxplot theme_minimal labs geom_point geom_bar
#' @importFrom tibble as_tibble
#' @export
auto_multinom_logistic_regression <- function(
    data,
    outcome,
    predictors,
    interactions = FALSE,
    stepwise = TRUE,
    direction = c("both", "backward", "forward"),
    criterion = c("AIC", "BIC"),
    crossval = FALSE,
    k = 5,
    report = FALSE
) {
  # --- Checks and setup ---
  direction <- match.arg(direction)
  criterion <- match.arg(criterion)
  if (!requireNamespace("nnet", quietly = TRUE)) stop("Please install the 'nnet' package.")
  data <- tibble::as_tibble(data)
  if (!(outcome %in% names(data))) stop("Outcome not found.")
  if (any(!predictors %in% names(data))) stop("Some predictors not found.")
  y <- data[[outcome]]
  if (!is.factor(y)) y <- factor(y)
  if (nlevels(y) < 3) stop("Outcome must have 3 or more levels for multinomial logit.")
  # Formula & (optionally) interactions
  rhs <- predictors
  if (interactions && length(predictors) > 1) {
    int_terms <- apply(utils::combn(predictors, 2), 2, function(x) paste(x, collapse=":"))
    rhs <- c(rhs, int_terms)
  }
  formula_str <- paste(outcome, "~", paste(rhs, collapse = "+"))
  fm <- as.formula(formula_str)
  # --- Fit model ---
  fit0 <- nnet::multinom(fm, data = data, trace = FALSE)
  # --- Stepwise selection (manual, since stats::step will not work with nnet::multinom) ---
  fit <- fit0
  if (stepwise) {
    if (!requireNamespace("MASS", quietly = TRUE)) stop("Install 'MASS' package for stepAIC on multinom models.")
    penalty <- if (criterion == "AIC") 2 else log(nrow(data))
    fit <- MASS::stepAIC(fit0, direction = direction, k = penalty, trace = FALSE)
  }
  # --- Summarize: Odds ratios, CIs ---
  has_broom <- requireNamespace("broom", quietly=TRUE)
  if (has_broom) {
    coefs <- broom::tidy(fit, conf.int = TRUE, exponentiate = TRUE)
    names(coefs)[names(coefs) == "estimate"] <- "odds_ratio"
  } else {
    # Manual odds ratios/CIs (approximate, 1.96*se)
    co <- summary(fit)$coefficients
    se <- summary(fit)$standard.errors
    or <- exp(co)
    or_low <- exp(co - 1.96*se)
    or_high <- exp(co + 1.96*se)
    coefs <- as.data.frame(matrix(NA, nrow = length(or), ncol = 4))
    names(coefs) <- c("term", "odds_ratio", "conf.low", "conf.high")
    rownum <- 1
    for (lev in rownames(or)) {
      for (nm in colnames(or)) {
        coefs[rownum,] <- c(paste(lev, nm, sep=":"), or[lev,nm], or_low[lev, nm], or_high[lev, nm])
        rownum <- rownum + 1
      }
    }
  }
  print("Odds ratios (95% CI) for each outcome vs baseline:")
  print(coefs, row.names = FALSE, digits=3)
  # VIF
  has_car <- requireNamespace("car", quietly=TRUE)
  vif_table <- if (has_car) tryCatch(car::vif(fit), error=function(e) NULL) else NULL
  # --- Cross-validation: mean misclassification rate ---
  cv_out <- NULL
  if (crossval) {
    n <- nrow(data)
    folds <- sample(rep(1:k, length.out = n))
    misclass <- numeric(k)
    for (fold in 1:k) {
      train <- data[folds != fold,]
      test <- data[folds == fold,]
      mod <- nnet::multinom(fm, data = train, trace = FALSE)
      pred <- predict(mod, newdata = test)
      misclass[fold] <- mean(pred != test[[outcome]])
    }
    cv_out <- list(cv_misclass_rate = mean(misclass), misclass_by_fold = misclass)
    message(sprintf("Cross-validated mean misclassification: %.3f", cv_out$cv_misclass_rate))
  }
  # --- Diagnostic plot: predicted class probabilities, by group ---
  probs <- as.data.frame(predict(fit, type="probs"))
  pred_class <- predict(fit)
  tab_pred <- table(Predicted = pred_class, Actual = data[[outcome]])
  print(tab_pred)
  # Show per-class boxplots for one continuous predictor, if available
  numpreds <- predictors[sapply(predictors, function(p) is.numeric(data[[p]]))]
  p1 <- NULL
  if (length(numpreds) >= 1) {
    firstnum <- numpreds[1]
    dfplot <- data.frame(y = data[[outcome]], x = data[[firstnum]])
    p1 <- ggplot2::ggplot(dfplot, ggplot2::aes(x = y, y = x, fill = y)) +
      ggplot2::geom_boxplot() +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = paste("Distribution of", firstnum, "by outcome"))
    print(p1)
  } else {
    # barplot for first non-numeric predictor
    cat("No numeric predictors for boxplot; skipping.")
    tabcat <- table(data[[outcome]])
    bar_df <- data.frame(outcome = names(tabcat), count = as.integer(tabcat))
    p1 <- ggplot2::ggplot(bar_df, ggplot2::aes(x = outcome, y = count, fill = outcome)) +
      ggplot2::geom_bar(stat="identity") +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Outcome frequencies")
    print(p1)
  }
  # --- Markdown report ---
  report_md <- NULL
  if (report) {
    if (!requireNamespace("broom", quietly = TRUE) || !requireNamespace("knitr", quietly = TRUE)) stop("Install 'broom' and 'knitr' for report.")
    knit <- knitr::kable(coefs, caption = "Odds ratios by outcome (95% CI)")
    confusion_txt <- paste(capture.output(print(tab_pred)), collapse = "<br>")
    vif_txt <- if (!is.null(vif_table)) knitr::kable(as.data.frame(vif_table), caption = "VIF Table") else "VIF: car not installed"
    cv_txt <- if (!is.null(cv_out)) sprintf("CV mean misclass rate: %.3f", cv_out$cv_misclass_rate) else ""
    report_md <- paste0(
      "## Model summary \n\n",
      knit, "\n\n",
      "### Confusion matrix\n", confusion_txt, "\n\n",
      vif_txt, "\n",
      cv_txt, "\n"
    )
    cat(report_md)
  }
  # --- Output ---
  res <- list(
    model = fit,
    summary = summary(fit),
    coefficients = coefs,
    vif = vif_table,
    confusion_matrix = tab_pred,
    pred_prob_plot = p1,
    cross_validation = cv_out,
    report_md = report_md
  )
  invisible(res)
}
