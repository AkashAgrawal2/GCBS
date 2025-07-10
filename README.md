
# GCBS

The GCBS R package provides a suite of easy, robust functions for data
cleaning and statistical analysis—designed for reproducible and
high-quality research, especially in the GCBS research department.

> This package is optimized for both new and experienced R users. Its
> functions enable: Painless data cleaning (missing values, recoding,
> trimming, splitting, level merging, and more) One-line statistical
> analysis: t-tests, ANOVA, regression (linear, logistic, multinomial),
> with stepwise model selection and cross-validation Publication-ready
> plots for analyses and diagnostics Markdown/HTML model summaries for
> reports and teaching

## Installation

Install the development version from GitHub with:

    install.packages("pak")
    # pak::pak("AkashAgrawal2/GCBS")

## Key Features

- Streamlined, readable functions for cleaning and transforming data
- Automatic outlier detection and removal tools
- Comprehensive statistical tests and modeling (t-test, ANOVA, linear,
  logistic, multinomial regression) with diagnostics and plots
- Cross-validation, VIF reports, and confidence intervals built-in
- Markdown-friendly reporting for reproducible documents

## Example Workflow

#### 1. Data Cleaning

    library(GCBS)
    library(tibble)

    df <- tibble::tibble(
      id = 1:6,
      age = c(28, 29, NA, 31, 32, 999),
      gender = c("M", "F", "F", "M", NA, "F"),
      score = c("  90", "85 ", " 76", NA, "83", "88")
    )

    # Replace extreme code with NA
    df$age[df$age == 999] <- NA

    # Impute missing values as appropriate
    cleaned <- impute_missing(df, impute_map = list(
      age = "median", 
      gender = "mode", 
      score = list(method = "constant", constant = "0")
    ))
    cleaned <- trim_whitespace(cleaned)
    cleaned <- convert_types(cleaned, type_map = list(score = "numeric"))

    # Summarize missingness
    summarize_missing(cleaned)

#### 2. Outlier Detection

    df2 <- tibble::tibble(
      height = c(160, 165, 170, 171, 168, 250),
      weight = c(60, 65, 62, 1000, 64, 61)
    )
    flagged <- detect_outliers(df2, plot = TRUE) # Plots outliers instantly!
    remove_outliers(flagged)

#### 3. Statistical Analysis, Plots, and Reporting

##### t-test (with automatic plot)

    df3 <- tibble::tibble(
      group = rep(c("A", "B"), each=20),
      value = c(rnorm(20, 5), rnorm(20, 7))
    )
    auto_ttest_plot(df3, value = "value", group = "group")

##### Linear regression with stepwise selection and full diagnostics

    set.seed(42)
    df4 <- tibble::tibble(
      y = rnorm(60, 10 + 0.5*1:60, 4),
      x1 = rnorm(60),
      x2 = rnorm(60, 5)
    )
    auto_stepwise_regression_report(
      df4,
      outcome = "y",
      predictors = c("x1", "x2"),
      stepwise = TRUE,
      crossval = TRUE,
      report = TRUE
    )

##### Multinomial logistic regression with report

    set.seed(123)
    df5 <- tibble::tibble(
      outcome = factor(sample(c("Low", "Med", "High"), 75, replace=TRUE)),
      pred1 = rnorm(75),
      pred2 = sample(LETTERS[1:3], 75, replace=TRUE)
    )
    auto_multinom_report(df5, outcome = "outcome", predictors = c("pred1", "pred2"), report = TRUE)

#### Questions or Suggestions?

> Email me at <akashagrsm@gmail.com>

*Github link: <https://github.com/AkashAgrawal2/GCBS>*  
*Developed by Akash Agrawal*

*Making statistical analysis accessible, reproducible, and rigorous for
your research at Goodman Campbell Brain and Spine.*
