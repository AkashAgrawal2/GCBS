library(shiny)
library(bslib)
library(ggplot2)

set.seed(1001)

example_data <- data.frame(
  patient_id = sprintf("P%03d", 1:120),
  treatment = factor(rep(c("Control", "Low dose", "High dose"), each = 40)),
  sex = factor(rep(c("Female", "Male"), 60)),
  age = round(rnorm(120, mean = 52, sd = 11)),
  baseline_score = round(rnorm(120, mean = 70, sd = 10), 1),
  outcome_score = round(c(
    rnorm(40, mean = 72, sd = 9),
    rnorm(40, mean = 77, sd = 10),
    rnorm(40, mean = 83, sd = 11)
  ), 1),
  response = factor(ifelse(runif(120) > 0.45, "Responder", "Non-responder")),
  visit_date = seq.Date(as.Date("2026-01-01"), by = "day", length.out = 120)
)

method_catalog <- data.frame(
  Category = c(
    rep("Test statistic calculators", 27),
    rep("Regression calculators", 6),
    rep("Confidence interval / sample size / effect size", 4),
    rep("Distribution calculators", 10),
    rep("Statistical tables", 2),
    rep("Basic statistics calculators", 22),
    rep("Visualization", 14)
  ),
  Method = c(
    "One Sample Z-Test", "One Sample T-Test", "Two Sample Z-Test",
    "Two Sample T-Test (Pooled variance)", "Two Sample T-Test (Welch's)",
    "Two Sample Mann-Whitney U Test", "Paired T-Test",
    "Paired Wilcoxon Sign Rank Test", "One Way ANOVA Test",
    "Repeated Measures ANOVA Test", "Sphericity", "Friedman Test",
    "Kruskal-Wallis Test", "Two Way ANOVA Test", "One Way MANOVA Test",
    "One Sample Proportion Test", "Two Sample Proportion Test",
    "Chi-Squared Test For Variance", "F Test For Variances",
    "Levene's Test For Variances", "Chi-Squared Test For Goodness Of Fit",
    "McNemar test", "Chi-squared test for association", "Fisher test",
    "Binomial test", "Shapiro-Wilk Test", "Kolmogorov Smirnov Test",
    "Simple Linear Regression", "Multiple Linear Regression", "Bulk Linear Regression",
    "Binary Logistic Regression", "Multinomial Logistic Regression",
    "Propensity Score Matching",
    "Sample size calculators", "Test power calculators", "Effect size calculators",
    "Confidence intervals calculators",
    "P-value", "Distributions", "Normal Distribution", "Binomial Distribution",
    "T Distribution (Student's)", "Chi-Squared Distribution",
    "F Distribution (Fisher-Snedecor)", "Poisson Distribution",
    "Weibull Distribution", "Exponential Distribution",
    "Z table", "T table",
    "Statistics calculator", "Descriptive statistics calculator",
    "Percentile calculator", "Mean Median Mode calculator",
    "Measures Of Dispersion calculator", "Standard Deviation calculator",
    "Variance calculator", "Coefficient of Variation calculator",
    "Average calculator", "Sample Mean calculator", "Geometric Mean calculator",
    "Harmonic Mean calculator", "Probability calculator", "Combinations calculator",
    "Permutation calculator", "Correlation calculator", "Outliers",
    "Markov chain calculator", "Odds calculator", "Betting odds calculator",
    "Mean absolute deviation calculator", "Median absolute deviation calculator",
    "Principal component analysis", "Cluster analysis", "Histogram maker",
    "Box plot maker", "Bar graph maker", "Pie chart maker", "Line chart maker",
    "Scatter chart maker", "Area chart maker", "Violin plot maker",
    "Venn diagram maker", "Venn diagram calculator", "Stem and Leaf plot",
    "ROC Calculator"
  ),
  Description = c(
    "Compares a sample mean to a known population mean when population SD is known.",
    "Compares a numeric sample mean to a hypothesized value.",
    "Compares two means when population SDs are known.",
    "Compares two means assuming equal variances.",
    "Compares two means without assuming equal variances.",
    "Compares ranks between two independent groups.",
    "Compares paired numeric measurements.",
    "Compares paired ranks.",
    "Compares a numeric outcome across three or more groups.",
    "Compares repeated measurements across dependent groups.",
    "Assesses equal variances of differences in repeated-measures designs.",
    "Non-parametric repeated-measures comparison.",
    "Compares ranks across two or more independent groups.",
    "Tests two categorical factors against a numeric outcome.",
    "Compares multiple numeric outcomes across one grouping variable.",
    "Tests whether one proportion differs from a hypothesized value.",
    "Tests whether two proportions differ.",
    "Tests whether a variance differs from a hypothesized value.",
    "Compares two variances.",
    "Tests equality of variance across groups.",
    "Tests observed categorical counts against expected counts.",
    "Tests paired categorical changes in a 2x2 table.",
    "Tests association between categorical variables.",
    "Exact test for categorical association, useful with small counts.",
    "Exact test for binary outcomes.",
    "Tests whether a numeric variable is consistent with normality.",
    "Distributional goodness-of-fit test.",
    "Models one numeric outcome against one predictor.",
    "Models one numeric outcome against multiple predictors.",
    "Runs linear models for multiple outcomes with shared predictors.",
    "Models a binary outcome.",
    "Models an unordered categorical outcome with more than two levels.",
    "Balances treatment groups by estimated treatment propensity.",
    "Planning calculator outside uploaded-data workflow.",
    "Planning calculator outside uploaded-data workflow.",
    "Effect-size helper outside uploaded-data workflow.",
    "Confidence-interval helper outside uploaded-data workflow.",
    "Distribution probability helper.", "Distribution probability helper.",
    "Normal distribution helper.", "Binomial distribution helper.",
    "Student t distribution helper.", "Chi-squared distribution helper.",
    "F distribution helper.", "Poisson distribution helper.",
    "Weibull distribution helper.", "Exponential distribution helper.",
    "Reference table.", "Reference table.",
    "Grouped summaries for numeric variables.",
    "Descriptive summaries for numeric variables.",
    "Percentiles for numeric variables.",
    "Mean, median, and mode.",
    "Range, IQR, variance, SD, and related dispersion measures.",
    "Standard deviation and standard error.",
    "Variance and related summaries.",
    "Standard deviation divided by mean.",
    "Average, sum, and count.",
    "Sample mean, sum, and count.",
    "Geometric mean for positive numeric values.",
    "Harmonic mean for positive numeric values.",
    "Probability helper outside uploaded-data workflow.",
    "Counting helper outside uploaded-data workflow.",
    "Counting helper outside uploaded-data workflow.",
    "Pearson and Spearman correlations.",
    "Tukey fence and z-score outlier flags.",
    "Markov-chain helper outside uploaded-data workflow.",
    "Odds/probability converter.",
    "Betting-odds converter.",
    "Mean absolute deviation.",
    "Median absolute deviation.",
    "Principal components for numeric columns.",
    "Cluster observations using selected numeric columns.",
    "Histogram for numeric columns.",
    "Box plots for numeric columns.",
    "Bar graph for categorical columns.",
    "Pie chart for categorical columns.",
    "Line chart for ordered data.",
    "Scatter chart for numeric pairs.",
    "Area chart for ordered data.",
    "Violin plots for numeric outcomes by groups.",
    "Set visualization outside current workflow.",
    "Set calculator outside current workflow.",
    "Text-style distribution display.",
    "ROC analysis requires outcome and score columns; cataloged for future extension."
  ),
  Link = c(
    "https://www.statskingdom.com/110MeanNormal1.html",
    "https://www.statskingdom.com/130MeanT1.html",
    "https://www.statskingdom.com/120MeanNormal2.html",
    "https://www.statskingdom.com/140MeanT2eq.html",
    "https://www.statskingdom.com/150MeanT2uneq.html",
    "https://www.statskingdom.com/170median_mann_whitney.html",
    "https://www.statskingdom.com/160MeanT2paired.html",
    "https://www.statskingdom.com/175wilcoxon_signed_ranks.html",
    "https://www.statskingdom.com/180Anova1way.html",
    "https://www.statskingdom.com/215repeated_measures_anova.html",
    "https://www.statskingdom.com/215mauchly.html",
    "https://www.statskingdom.com/220friedman.html",
    "https://www.statskingdom.com/kruskal-wallis-calculator.html",
    "https://www.statskingdom.com/190Anova2way.html",
    "https://www.statskingdom.com/230manova.html",
    "https://www.statskingdom.com/301proportion_z_test.html",
    "https://www.statskingdom.com/302proportion_z_test2.html",
    "https://www.statskingdom.com/220VarChi2.html",
    "https://www.statskingdom.com/230VarF2.html",
    "https://www.statskingdom.com/230var_levenes.html",
    "https://www.statskingdom.com/310GoodnessChi.html",
    "https://www.statskingdom.com/320McNemar.html",
    "https://www.statskingdom.com/310GoodnessChi.html",
    "https://www.statskingdom.com/310GoodnessChi.html",
    "https://www.statskingdom.com/binomial-test-calculator.html",
    "https://www.statskingdom.com/320ShapiroWilk.html",
    "https://www.statskingdom.com/320KolmogorovSmirnov.html",
    "https://www.statskingdom.com/linear-regression-calculator.html",
    "https://www.statskingdom.com/multiple-linear-regression-calculator.html",
    "https://www.statskingdom.com/bulk-linear-regression-calculator.html",
    "https://www.statskingdom.com/logistic-regression-calculator.html",
    "https://www.statskingdom.com/multinomial-logistic-regression.html",
    "https://www.statskingdom.com/propensity-score-matching.html",
    rep("https://www.statskingdom.com/index.html", 52)
  ),
  stringsAsFactors = FALSE
)

method_catalog$AppStatus <- ifelse(
  method_catalog$Method %in% c(
    "One Sample T-Test", "Two Sample T-Test (Welch's)",
    "Two Sample Mann-Whitney U Test", "Paired T-Test",
    "Paired Wilcoxon Sign Rank Test", "One Way ANOVA Test",
    "Friedman Test", "Kruskal-Wallis Test", "Two Way ANOVA Test",
    "One Sample Proportion Test", "Two Sample Proportion Test",
    "Chi-Squared Test For Variance", "F Test For Variances",
    "Levene's Test For Variances", "Chi-Squared Test For Goodness Of Fit",
    "McNemar test", "Chi-squared test for association", "Fisher test",
    "Binomial test", "Shapiro-Wilk Test", "Kolmogorov Smirnov Test",
    "Simple Linear Regression", "Multiple Linear Regression",
    "Bulk Linear Regression", "Binary Logistic Regression",
    "Multinomial Logistic Regression", "Statistics calculator",
    "Descriptive statistics calculator", "Percentile calculator",
    "Mean Median Mode calculator", "Measures Of Dispersion calculator",
    "Standard Deviation calculator", "Variance calculator",
    "Coefficient of Variation calculator", "Average calculator",
    "Sample Mean calculator", "Geometric Mean calculator",
    "Harmonic Mean calculator", "Correlation calculator", "Outliers",
    "Mean absolute deviation calculator", "Median absolute deviation calculator",
    "Principal component analysis", "Cluster analysis", "Histogram maker",
    "Box plot maker", "Bar graph maker", "Pie chart maker",
    "Line chart maker", "Scatter chart maker", "Area chart maker",
    "Violin plot maker"
  ),
  "Runnable",
  "Catalog only"
)

runnable_methods <- method_catalog$Method[
  method_catalog$AppStatus == "Runnable" &
    method_catalog$Category != "Visualization"
]

colourInputFallback <- function(inputId, label, value) {
  textInput(inputId, label, value = value, placeholder = "#2a9d8f")
}

plot_color_choices <- c(
  "Teal" = "#2a9d8f",
  "Coral" = "#e76f51",
  "Blue" = "#1d4ed8",
  "Sky" = "#0ea5e9",
  "Green" = "#16a34a",
  "Purple" = "#7c3aed",
  "Rose" = "#e11d48",
  "Amber" = "#f59e0b",
  "Slate" = "#334155",
  "Custom" = "custom"
)

valid_hex_color <- function(x) {
  is_nonempty(x) && grepl("^#[0-9A-Fa-f]{6}$", x)
}

resolve_plot_color <- function(hex_value, dropdown_value, fallback = "#2a9d8f") {
  if (valid_hex_color(hex_value)) return(hex_value)
  if (is_nonempty(dropdown_value) && dropdown_value != "custom") return(dropdown_value)
  fallback
}

dropdown_for_color <- function(hex_value) {
  if (!valid_hex_color(hex_value)) return("custom")
  matched <- plot_color_choices[plot_color_choices == hex_value]
  if (length(matched)) unname(matched[[1]]) else "custom"
}

`%||%` <- function(x, y) {
  if (is.null(x) || !length(x)) y else x
}

is_nonempty <- function(x) {
  length(x) == 1 && !is.na(x) && nzchar(x)
}

date_format_choices <- c(
  "Auto-detect" = "auto",
  "YYYY-MM-DD" = "%Y-%m-%d",
  "YYYY/MM/DD" = "%Y/%m/%d",
  "YYYYMMDD" = "%Y%m%d",
  "MM/DD/YYYY" = "%m/%d/%Y",
  "DD/MM/YYYY" = "%d/%m/%Y",
  "MM-DD-YYYY" = "%m-%d-%Y",
  "DD-MM-YYYY" = "%d-%m-%Y",
  "MM/DD/YY" = "%m/%d/%y",
  "DD/MM/YY" = "%d/%m/%y",
  "Month DD, YYYY" = "%B %d, %Y",
  "Mon DD, YYYY" = "%b %d, %Y"
)

date_format_orders <- c(
  "%Y-%m-%d" = "ymd",
  "%Y/%m/%d" = "ymd",
  "%Y%m%d" = "ymd",
  "%m/%d/%Y" = "mdy",
  "%d/%m/%Y" = "dmy",
  "%m-%d-%Y" = "mdy",
  "%d-%m-%Y" = "dmy",
  "%m/%d/%y" = "mdy",
  "%d/%m/%y" = "dmy",
  "%B %d, %Y" = "mdy",
  "%b %d, %Y" = "mdy"
)

date_order_score <- function(values, order) {
  sample_values <- head(trimws(as.character(values[!is.na(values) & nzchar(as.character(values))])), 50)
  if (!length(sample_values) || !order %in% c("ymd", "mdy", "dmy")) return(0)
  pieces <- regmatches(sample_values, gregexpr("\\d+", sample_values))
  pieces <- pieces[lengths(pieces) >= 3]
  if (!length(pieces)) return(0)
  matrix_values <- do.call(rbind, lapply(pieces, function(x) as.integer(x[1:3])))
  unique_counts <- apply(matrix_values, 2, function(x) length(unique(x[!is.na(x)])))
  medians <- apply(matrix_values, 2, stats::median, na.rm = TRUE)
  positions <- strsplit(order, "", fixed = TRUE)[[1]]
  names(unique_counts) <- positions
  names(medians) <- positions
  score <- 0
  if (!is.na(unique_counts["y"]) && !is.na(unique_counts["m"]) && unique_counts["y"] <= unique_counts["m"]) score <- score + 0.15
  if (!is.na(unique_counts["m"]) && !is.na(unique_counts["d"]) && unique_counts["m"] <= unique_counts["d"]) score <- score + 0.15
  if (!is.na(medians["m"]) && medians["m"] >= 1 && medians["m"] <= 12) score <- score + 0.1
  if (!is.na(medians["d"]) && medians["d"] >= 1 && medians["d"] <= 31) score <- score + 0.1
  score
}

guess_date_format <- function(x) {
  values <- trimws(as.character(x))
  values <- values[!is.na(values) & nzchar(values)]
  if (!length(values)) {
    return(list(format = NA_character_, label = "No non-missing values", confidence = 0, parsed = 0, total = 0))
  }
  sample_values <- head(values, 200)
  candidates <- unname(date_format_choices[date_format_choices != "auto"])
  scored <- lapply(candidates, function(fmt) {
    parsed <- suppressWarnings(as.Date(sample_values, format = fmt))
    parsed_n <- sum(!is.na(parsed))
    order <- date_format_orders[[fmt]] %||% ""
    ratio <- parsed_n / length(sample_values)
    data.frame(
      Format = fmt,
      Parsed = parsed_n,
      Total = length(sample_values),
      Score = ratio + date_order_score(sample_values, order),
      stringsAsFactors = FALSE
    )
  })
  scored <- do.call(rbind, scored)
  scored <- scored[order(scored$Score, scored$Parsed, decreasing = TRUE), , drop = FALSE]
  best <- scored[1, ]
  label <- names(date_format_choices)[match(best$Format, date_format_choices)]
  if (is.na(label)) label <- best$Format
  list(
    format = best$Format,
    label = label,
    confidence = round(100 * best$Parsed / best$Total, 1),
    parsed = best$Parsed,
    total = best$Total
  )
}

parse_date_column <- function(x, format = "auto") {
  if (inherits(x, "Date")) return(x)
  fmt <- format
  if (!is_nonempty(fmt) || identical(fmt, "auto")) {
    fmt <- guess_date_format(x)$format
  }
  validate(need(is_nonempty(fmt), "No usable date format could be inferred. Choose a specific format."))
  suppressWarnings(as.Date(as.character(x), format = fmt))
}

coerce_recode_value <- function(value, target_type, reference = NULL) {
  if (identical(target_type, "missing")) return(NA)
  if (!is_nonempty(value)) return(value)
  switch(
    target_type,
    numeric = suppressWarnings(as.numeric(value)),
    logical = {
      lower <- tolower(trimws(value))
      if (lower %in% c("true", "t", "1", "yes", "y")) TRUE else if (lower %in% c("false", "f", "0", "no", "n")) FALSE else NA
    },
    character = as.character(value),
    keep = {
      if (is.logical(reference)) {
        coerce_recode_value(value, "logical", reference)
      } else if (is.numeric(reference)) {
        suppressWarnings(as.numeric(value))
      } else if (inherits(reference, "Date")) {
        parse_date_column(value, "auto")
      } else {
        value
      }
    },
    value
  )
}

format_conversion_value <- function(x, quote_value = FALSE) {
  if (length(x) != 1 || is.na(x)) return("NA")
  text <- as.character(x)
  if (nchar(text) > 60) text <- paste0(substr(text, 1, 57), "...")
  if (isTRUE(quote_value)) {
    text <- gsub("\\\\", "\\\\\\\\", text)
    text <- gsub("\"", "\\\\\"", text)
    return(paste0('"', text, '"'))
  }
  if (identical(text, "")) return('""')
  text
}

conversion_example_ui <- function(data, column, target) {
  if (!is_nonempty(column) || !is_nonempty(target) || !column %in% names(data)) {
    return(NULL)
  }

  values <- data[[column]]
  candidate_rows <- which(!is.na(values))
  if (!length(candidate_rows)) {
    return(tags$div(class = "small text-muted mt-1", "No non-missing values are available to preview this conversion."))
  }

  converted_values <- switch(
    target,
    Factor = as.factor(values),
    Numeric = suppressWarnings(as.numeric(values)),
    Character = as.character(values),
    values
  )

  if (identical(target, "Numeric")) {
    usable_rows <- candidate_rows[!is.na(converted_values[candidate_rows])]
    if (length(usable_rows)) candidate_rows <- usable_rows
  }

  sample_row <- candidate_rows[[1]]
  original_is_categorical <- is.factor(values) || is.character(values) || is.logical(values)
  converted_is_categorical <- is.factor(converted_values) || is.character(converted_values) || is.logical(converted_values)
  tags$div(
    class = "small text-muted mt-1",
    "Example from current data: ",
    tags$code(format_conversion_value(values[[sample_row]], original_is_categorical)),
    " -> ",
    tags$code(format_conversion_value(converted_values[[sample_row]], converted_is_categorical))
  )
}

read_dataset <- function(file, skip = 0, header = TRUE, delimiter = "auto", select_columns = NULL, n_max = Inf) {
  ext <- tolower(tools::file_ext(file$name))
  skip <- max(0, as.integer(skip))
  nrows <- if (is.infinite(n_max)) -1 else as.integer(n_max)

  data <- switch(
    ext,
    csv = {
      sep <- if (delimiter == "auto") "," else delimiter
      read.table(file$datapath, sep = sep, header = header, skip = skip, nrows = nrows,
        stringsAsFactors = FALSE, check.names = FALSE, comment.char = "", quote = "\""
      )
    },
    txt = {
      sep <- if (delimiter == "auto") "" else delimiter
      read.table(file$datapath, sep = sep, header = header, skip = skip, nrows = nrows,
        stringsAsFactors = FALSE, check.names = FALSE, comment.char = "", quote = "\""
      )
    },
    tsv = {
      sep <- if (delimiter == "auto") "\t" else delimiter
      read.table(file$datapath, sep = sep, header = header, skip = skip, nrows = nrows,
        stringsAsFactors = FALSE, check.names = FALSE, comment.char = "", quote = "\""
      )
    },
    xls = readxl::read_excel(file$datapath, skip = skip, col_names = header, n_max = n_max) |> as.data.frame(),
    xlsx = readxl::read_excel(file$datapath, skip = skip, col_names = header, n_max = n_max) |> as.data.frame(),
    stop("Unsupported file type. Upload csv, tsv, txt, xls, or xlsx.")
  )

  if (!is.null(select_columns) && length(select_columns)) {
    data <- data[, intersect(select_columns, names(data)), drop = FALSE]
  }

  data
}

read_lookup_files <- function(files) {
  if (is.null(files) || !nrow(files)) return(list())
  lookups <- vector("list", nrow(files))
  names(lookups) <- make.unique(files$name)
  for (i in seq_len(nrow(files))) {
    lookups[[i]] <- read_dataset(files[i, , drop = FALSE])
  }
  lookups
}

join_type_description <- function(join_type) {
  switch(
    join_type,
    left = "left_join keeps every row from the primary dataset. Matching lookup rows are added when keys match; unmatched lookup-only rows are ignored.",
    right = "right_join keeps every row from the lookup dataset. Matching primary rows are added when keys match; unmatched primary-only rows are ignored.",
    inner = "inner_join keeps only rows where the key appears in both datasets. Unmatched rows from either side are dropped.",
    full = "full_join keeps all rows from both datasets. Matching rows are combined; unmatched rows are retained with missing values on the other side.",
    "Choose a join type."
  )
}

relationship_description <- function(relationship) {
  switch(
    relationship,
    one_to_one = "One-to-one: each key appears at most once in the primary dataset and at most once in the lookup dataset.",
    one_to_many = "One-to-many: primary keys are unique, but lookup keys may repeat. One primary row can expand into multiple rows.",
    many_to_one = "Many-to-one: primary keys may repeat, but lookup keys are unique. Several primary rows can share one lookup row.",
    many_to_many = "Many-to-many: keys may repeat on both sides. This is allowed, but it can multiply rows quickly.",
    "Choose the expected key relationship."
  )
}

perform_join <- function(primary, lookup, primary_key, lookup_key, join_type) {
  all_x <- join_type %in% c("left", "full")
  all_y <- join_type %in% c("right", "full")
  merge(
    primary,
    lookup,
    by.x = primary_key,
    by.y = lookup_key,
    all.x = all_x,
    all.y = all_y,
    sort = FALSE,
    suffixes = c("", ".lookup")
  )
}

detect_join_relationship <- function(primary_counts, lookup_counts, shared_keys) {
  primary_repeats <- any(primary_counts[shared_keys] > 1)
  lookup_repeats <- any(lookup_counts[shared_keys] > 1)
  if (primary_repeats && lookup_repeats) {
    "many_to_many"
  } else if (!primary_repeats && lookup_repeats) {
    "one_to_many"
  } else if (primary_repeats && !lookup_repeats) {
    "many_to_one"
  } else {
    "one_to_one"
  }
}

join_diagnostics <- function(primary, lookup, primary_key, lookup_key, join_type, expected_relationship = "many_to_many") {
  joined <- perform_join(primary, lookup, primary_key, lookup_key, join_type)
  primary_keys <- primary[[primary_key]]
  lookup_keys <- lookup[[lookup_key]]
  primary_counts <- table(primary_keys, useNA = "ifany")
  lookup_counts <- table(lookup_keys, useNA = "ifany")
  shared_keys <- intersect(names(primary_counts), names(lookup_counts))
  detected_relationship <- detect_join_relationship(primary_counts, lookup_counts, shared_keys)
  many_to_many_keys <- shared_keys[primary_counts[shared_keys] > 1 & lookup_counts[shared_keys] > 1]
  unmatched_primary <- sum(!primary_keys %in% lookup_keys)
  unmatched_lookup <- sum(!lookup_keys %in% primary_keys)
  row_growth <- if (nrow(primary) == 0) NA_real_ else nrow(joined) / nrow(primary)
  warnings <- character()

  allowed_relationships <- switch(
    expected_relationship,
    one_to_one = "one_to_one",
    one_to_many = c("one_to_one", "one_to_many"),
    many_to_one = c("one_to_one", "many_to_one"),
    many_to_many = c("one_to_one", "one_to_many", "many_to_one", "many_to_many"),
    c("one_to_one", "one_to_many", "many_to_one", "many_to_many")
  )

  if (!detected_relationship %in% allowed_relationships) {
    warnings <- c(
      warnings,
      paste0(
        "Detected ", detected_relationship,
        " key matching, but the expected relationship is ", expected_relationship,
        ". Choose a broader relationship if this expansion is intentional."
      )
    )
  }

  if (length(many_to_many_keys) && !"many_to_many" %in% allowed_relationships) {
    warnings <- c(
      warnings,
      paste0(
        "Many-to-many key matches detected for ", length(many_to_many_keys),
        " key value(s). This can multiply rows because repeated primary keys match repeated lookup keys."
      )
    )
  }
  if (!length(many_to_many_keys) && join_type %in% c("left", "inner", "full") &&
    any(lookup_counts[shared_keys] > 1) && !"one_to_many" %in% allowed_relationships) {
    warnings <- c(
      warnings,
      "The lookup dataset has repeated keys that match the primary dataset. One primary row can become multiple rows."
    )
  }
  if (!length(many_to_many_keys) && join_type %in% c("right", "inner", "full") &&
    any(primary_counts[shared_keys] > 1) && !"many_to_one" %in% allowed_relationships) {
    warnings <- c(
      warnings,
      "The primary dataset has repeated keys that match the lookup dataset. One lookup row can become multiple rows."
    )
  }
  if (!is.na(row_growth) && row_growth > 1.1) {
    warnings <- c(
      warnings,
      paste0("The join is expected to expand rows by about ", round(row_growth, 2), "x.")
    )
  }
  if (join_type %in% c("inner", "right") && unmatched_primary > 0) {
    warnings <- c(warnings, paste0(unmatched_primary, " primary row(s) have no lookup match and may be dropped."))
  }
  if (join_type %in% c("inner", "left") && unmatched_lookup > 0) {
    warnings <- c(warnings, paste0(unmatched_lookup, " lookup row(s) have no primary match and will not add rows."))
  }
  if (join_type == "full" && (unmatched_primary > 0 || unmatched_lookup > 0)) {
    warnings <- c(warnings, "A full join will retain unmatched rows from both datasets and fill missing values where no match exists.")
  }

  list(
    joined = joined,
    summary = data.frame(
      Metric = c(
        "Primary rows", "Primary columns", "Lookup rows", "Lookup columns",
        "Estimated joined rows", "Estimated joined columns", "Row growth factor",
        "Expected relationship", "Detected relationship",
        "Primary duplicate keys", "Lookup duplicate keys", "Many-to-many key values",
        "Unmatched primary rows", "Unmatched lookup rows"
      ),
      Value = c(
        nrow(primary), ncol(primary), nrow(lookup), ncol(lookup),
        nrow(joined), ncol(joined), round(row_growth, 3),
        expected_relationship, detected_relationship,
        sum(duplicated(primary_keys)), sum(duplicated(lookup_keys)), length(many_to_many_keys),
        unmatched_primary, unmatched_lookup
      ),
      stringsAsFactors = FALSE
    ),
    warnings = warnings,
    detected_relationship = detected_relationship,
    expected_relationship = expected_relationship,
    join_type = join_type,
    primary_key = primary_key,
    lookup_key = lookup_key
  )
}

join_example_tables <- function(join_type, relationship = "many_to_many") {
  examples <- switch(
    relationship,
    one_to_one = list(
      primary = data.frame(id = c(1, 2, 3), patient = c("A", "B", "C"), stringsAsFactors = FALSE),
      lookup = data.frame(id = c(2, 3, 4), lab = c("CBC", "BMP", "A1c"), stringsAsFactors = FALSE)
    ),
    one_to_many = list(
      primary = data.frame(id = c(1, 2, 3), patient = c("A", "B", "C"), stringsAsFactors = FALSE),
      lookup = data.frame(id = c(2, 2, 2, 4), lab = c("CBC", "BMP", "Lipids", "A1c"), stringsAsFactors = FALSE)
    ),
    many_to_one = list(
      primary = data.frame(id = c(1, 2, 2, 3), visit = c("A baseline", "B baseline", "B follow-up", "C baseline"), stringsAsFactors = FALSE),
      lookup = data.frame(id = c(2, 3, 4), site = c("North", "South", "East"), stringsAsFactors = FALSE)
    ),
    many_to_many = list(
      primary = data.frame(id = c(1, 2, 2, 3), visit = c("A baseline", "B baseline", "B follow-up", "C baseline"), stringsAsFactors = FALSE),
      lookup = data.frame(id = c(2, 2, 4), lab = c("CBC", "BMP", "A1c"), stringsAsFactors = FALSE)
    ),
    list(
      primary = data.frame(id = c(1, 2, 3), patient = c("A", "B", "C"), stringsAsFactors = FALSE),
      lookup = data.frame(id = c(2, 3, 4), value = c("X", "Y", "Z"), stringsAsFactors = FALSE)
    )
  )
  list(
    primary = examples$primary,
    lookup = examples$lookup,
    result = perform_join(examples$primary, examples$lookup, "id", "id", join_type)
  )
}

quote_name <- function(x) paste0("`", x, "`")

infer_column_type <- function(x) {
  non_missing <- x[!is.na(x)]
  n_unique <- length(unique(non_missing))
  if (inherits(x, "Date")) {
    "date"
  } else if (is.factor(x)) {
    "factor"
  } else if (is.character(x)) {
    if (n_unique <= max(12, length(non_missing) * 0.2)) "character / categorical" else "character"
  } else if (is.numeric(x) || is.integer(x)) {
    if (n_unique <= 5) "numeric / discrete" else "numeric"
  } else if (is.logical(x)) {
    "logical"
  } else {
    class(x)[1]
  }
}

profile_data <- function(data) {
  data.frame(
    Column = names(data),
    Type = vapply(data, infer_column_type, character(1)),
    Missing = vapply(data, function(x) sum(is.na(x)), integer(1)),
    Unique = vapply(data, function(x) length(unique(x[!is.na(x)])), integer(1)),
    Example = vapply(data, function(x) {
      values <- unique(x[!is.na(x)])
      paste(head(as.character(values), 3), collapse = ", ")
    }, character(1)),
    stringsAsFactors = FALSE
  )
}

data_with_type_headers <- function(data) {
  if (!ncol(data)) return(data)
  display_data <- data
  names(display_data) <- paste0(names(data), "\n", "(", vapply(data, infer_column_type, character(1)), ")")
  display_data
}

effective_figure_type <- function(data, requested_type, x_col, y_col) {
  if (!identical(requested_type, "auto")) return(requested_type)
  if (!ncol(data) || length(x_col) != 1 || !x_col %in% names(data)) return("bar")
  if (length(y_col) == 1 && y_col %in% names(data) && is.numeric(data[[y_col]]) && is_categorical(data[[x_col]])) {
    "box"
  } else if (length(y_col) == 1 && y_col %in% names(data) && is.numeric(data[[y_col]]) && is.numeric(data[[x_col]])) {
    "scatter"
  } else if (is.numeric(data[[x_col]])) {
    "histogram"
  } else {
    "bar"
  }
}

is_categorical <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x) || length(unique(x[!is.na(x)])) <= 8
}

numeric_columns <- function(data) names(data)[vapply(data, is.numeric, logical(1))]
categorical_columns <- function(data) names(data)[vapply(data, is_categorical, logical(1))]

clean_for_columns <- function(data, columns) {
  columns <- columns[columns %in% names(data)]
  if (!length(columns)) return(data[0, , drop = FALSE])
  data[stats::complete.cases(data[, columns, drop = FALSE]), columns, drop = FALSE]
}

recommend_pair <- function(data, outcome, predictor) {
  if (outcome == predictor) return(NULL)
  y <- data[[outcome]]
  x <- data[[predictor]]
  y_num <- is.numeric(y)
  x_num <- is.numeric(x)
  y_cat <- is_categorical(y)
  x_cat <- is_categorical(x)
  y_levels <- length(unique(y[!is.na(y)]))
  x_levels <- length(unique(x[!is.na(x)]))
  n_complete <- sum(stats::complete.cases(data[, c(outcome, predictor), drop = FALSE]))
  rows <- list()

  add <- function(method, why, feasible = TRUE) {
    rows[[length(rows) + 1]] <<- data.frame(
      Outcome = outcome,
      Predictor = predictor,
      Method = method,
      Why = why,
      Feasible = feasible,
      stringsAsFactors = FALSE
    )
  }

  if (n_complete < 3) add("No reliable test", "Fewer than three complete rows are available.", FALSE)
  if (y_num && x_cat && x_levels == 2) {
    add("Two Sample T-Test (Welch's)", "Numeric outcome with exactly two independent groups.")
    add("Two Sample Mann-Whitney U Test", "Numeric outcome with two groups; rank-based alternative.")
  }
  if (y_num && x_cat && x_levels >= 3) {
    add("One Way ANOVA Test", "Numeric outcome with three or more groups.")
    add("Kruskal-Wallis Test", "Rank-based comparison across two or more groups.")
    add("Levene's Test For Variances", "Checks equality of variance across groups.")
  }
  if (y_num && x_num) {
    add("Simple Linear Regression", "Numeric outcome with one numeric predictor.")
    add("Kolmogorov Smirnov Test", "Can compare the observed numeric distribution to a normal reference.")
  }
  if (y_cat && x_cat) {
    add("Chi-squared test for association", "Two categorical columns.")
    add("Fisher test", "Categorical association test useful with sparse counts.")
  }
  if (y_cat && y_levels == 2) add("Binary Logistic Regression", "Binary outcome with a selected predictor.")
  if (y_cat && y_levels > 2) add("Multinomial Logistic Regression", "Categorical outcome has more than two levels.")
  if (!length(rows)) add("No clear recommendation", "This column pair does not match a supported data pattern.", FALSE)
  do.call(rbind, rows)
}

recommend_batch <- function(data, outcomes, predictors) {
  outcomes <- outcomes[outcomes %in% names(data)]
  predictors <- predictors[predictors %in% names(data)]
  rows <- list()

  for (outcome in outcomes) {
    rows[[length(rows) + 1]] <- data.frame(
      Outcome = outcome,
      Predictor = "",
      Method = "Descriptive statistics calculator",
      Why = "Every selected column can be summarized; numeric columns receive expanded summaries.",
      Feasible = TRUE,
      stringsAsFactors = FALSE
    )
    if (is.numeric(data[[outcome]])) {
      rows[[length(rows) + 1]] <- data.frame(
        Outcome = outcome, Predictor = "", Method = "One Sample T-Test",
        Why = "Numeric column can be compared with a hypothesized mean of 0.",
        Feasible = TRUE, stringsAsFactors = FALSE
      )
      rows[[length(rows) + 1]] <- data.frame(
        Outcome = outcome, Predictor = "", Method = "Shapiro-Wilk Test",
        Why = "Numeric column can be screened for normality when sample size is between 3 and 5000.",
        Feasible = sum(!is.na(data[[outcome]])) >= 3 && sum(!is.na(data[[outcome]])) <= 5000,
        stringsAsFactors = FALSE
      )
    }
  }

  for (outcome in outcomes) {
    for (predictor in predictors) {
      rec <- recommend_pair(data, outcome, predictor)
      if (!is.null(rec)) rows[[length(rows) + 1]] <- rec
    }
  }

  numeric_selected <- intersect(unique(c(outcomes, predictors)), numeric_columns(data))
  if (length(numeric_selected) >= 2) {
    rows[[length(rows) + 1]] <- data.frame(
      Outcome = paste(numeric_selected, collapse = ", "),
      Predictor = "",
      Method = "Correlation calculator",
      Why = "Two or more numeric columns can be correlated pairwise.",
      Feasible = TRUE,
      stringsAsFactors = FALSE
    )
    rows[[length(rows) + 1]] <- data.frame(
      Outcome = paste(numeric_selected, collapse = ", "),
      Predictor = "",
      Method = "Principal component analysis",
      Why = "Two or more numeric columns can be reduced into principal components.",
      Feasible = TRUE,
      stringsAsFactors = FALSE
    )
    rows[[length(rows) + 1]] <- data.frame(
      Outcome = paste(numeric_selected, collapse = ", "),
      Predictor = "",
      Method = "Cluster analysis",
      Why = "Two or more numeric columns can be used to cluster rows.",
      Feasible = nrow(stats::na.omit(data[, numeric_selected, drop = FALSE])) >= 2,
      stringsAsFactors = FALSE
    )
  }

  if (!length(rows)) {
    return(data.frame(
      Outcome = "",
      Predictor = "",
      Method = "Select columns",
      Why = "Choose one or more outcomes and optional predictors to generate recommendations.",
      Feasible = FALSE,
      stringsAsFactors = FALSE
    ))
  }

  unique(do.call(rbind, rows))
}

method_feasibility_advice <- function(data, outcomes, predictors, methods) {
  if (identical(methods, "All feasible recommended analyses") || !length(methods)) {
    return(data.frame())
  }
  recs <- recommend_batch(data, outcomes, predictors)
  feasible_methods <- unique(recs$Method[recs$Feasible])
  missing_methods <- setdiff(methods, feasible_methods)
  if (!length(missing_methods)) return(data.frame())

  rows <- list()
  add <- function(method, reason, suggestion, fix_column = "", fix_type = "") {
    rows[[length(rows) + 1]] <<- data.frame(
      Method = method,
      Reason = reason,
      Suggestion = suggestion,
      FixColumn = fix_column,
      FixType = fix_type,
      stringsAsFactors = FALSE
    )
  }

  first_existing <- function(cols) {
    cols <- cols[cols %in% names(data)]
    if (length(cols)) cols[[1]] else ""
  }
  first_non_numeric <- function(cols) {
    cols <- cols[cols %in% names(data)]
    cols <- cols[!vapply(data[, cols, drop = FALSE], is.numeric, logical(1))]
    if (length(cols)) cols[[1]] else ""
  }
  first_numeric <- function(cols) {
    cols <- cols[cols %in% names(data)]
    cols <- cols[vapply(data[, cols, drop = FALSE], is.numeric, logical(1))]
    if (length(cols)) cols[[1]] else ""
  }
  has_two_level <- function(col) {
    is_nonempty(col) && length(unique(data[[col]][!is.na(data[[col]])])) == 2
  }

  for (method in missing_methods) {
    outcome <- first_existing(outcomes)
    predictor <- first_existing(predictors)
    nonnum_outcome <- first_non_numeric(outcomes)
    num_predictor <- first_numeric(predictors)
    nonnum_predictor <- first_non_numeric(predictors)

    if (method %in% c("One Sample T-Test", "Shapiro-Wilk Test")) {
      if (is_nonempty(nonnum_outcome)) {
        add(method, "This method requires at least one numeric outcome column.", "Convert the outcome to numeric if it was imported as text.", nonnum_outcome, "Numeric")
      } else {
        add(method, "No selected outcome currently satisfies the method requirements.", "Select a numeric outcome column with enough non-missing values.")
      }
    } else if (method %in% c("Two Sample T-Test (Welch's)", "Two Sample Mann-Whitney U Test")) {
      if (is_nonempty(nonnum_outcome)) {
        add(method, "This method requires a numeric outcome and a grouping column with exactly two groups.", "Convert the outcome to numeric if it is a measurement column.", nonnum_outcome, "Numeric")
      } else if (is_nonempty(num_predictor)) {
        add(method, "The grouping column appears numeric. Two-sample tests require exactly two groups.", "Convert the grouping column to factor if the numbers are group codes.", num_predictor, "Factor")
      } else if (!is_nonempty(predictor) || !has_two_level(predictor)) {
        add(method, "This method requires exactly two groups in the predictor column.", "Choose a two-level grouping variable or recode/filter the groups.")
      } else {
        add(method, "The selected columns do not match a numeric outcome plus two-level group pattern.", "Check imported data types and selected columns.")
      }
    } else if (method %in% c("One Way ANOVA Test", "Kruskal-Wallis Test", "Levene's Test For Variances")) {
      if (is_nonempty(nonnum_outcome)) {
        add(method, "This method requires a numeric outcome.", "Convert the outcome to numeric if it is a measurement column.", nonnum_outcome, "Numeric")
      } else if (is_nonempty(num_predictor)) {
        add(method, "This method requires a categorical grouping column.", "Convert the predictor to factor if the values are group codes.", num_predictor, "Factor")
      } else {
        add(method, "This method requires a numeric outcome and a categorical grouping column with enough groups.", "Select an appropriate grouping column or check imported data types.")
      }
    } else if (method %in% c("Chi-squared test for association", "Fisher test")) {
      if (is_nonempty(first_numeric(outcomes))) {
        add(method, "Categorical association tests require categorical variables.", "Convert the selected outcome to factor if it is a coded category.", first_numeric(outcomes), "Factor")
      } else if (is_nonempty(num_predictor)) {
        add(method, "Categorical association tests require categorical variables.", "Convert the selected predictor to factor if it is a coded category.", num_predictor, "Factor")
      } else {
        add(method, "The selected variables do not form a valid categorical table.", "Select categorical outcome and predictor columns.")
      }
    } else if (method %in% c("Simple Linear Regression", "Correlation calculator", "Principal component analysis", "Cluster analysis")) {
      if (is_nonempty(nonnum_outcome)) {
        add(method, "This method requires numeric measurement columns.", "Convert measurement columns imported as text to numeric.", nonnum_outcome, "Numeric")
      } else if (is_nonempty(nonnum_predictor)) {
        add(method, "This method requires numeric predictor/feature columns.", "Convert measurement predictors imported as text to numeric.", nonnum_predictor, "Numeric")
      } else {
        add(method, "There are not enough valid numeric columns for this method.", "Select more numeric columns or check imported data types.")
      }
    } else if (method == "Binary Logistic Regression") {
      if (is_nonempty(outcome) && !has_two_level(outcome)) {
        add(method, "Binary logistic regression requires an outcome with exactly two observed levels.", "Convert the outcome to factor if it is a binary coded category, or recode it to two levels.", outcome, "Factor")
      } else {
        add(method, "The selected columns do not match a binary outcome plus predictor pattern.", "Select a binary outcome and at least one predictor.")
      }
    } else if (method == "Multinomial Logistic Regression") {
      if (is_nonempty(first_numeric(outcomes))) {
        add(method, "Multinomial regression requires a categorical outcome with more than two levels.", "Convert the outcome to factor if it is a coded category.", first_numeric(outcomes), "Factor")
      } else {
        add(method, "The selected outcome does not have more than two categorical levels.", "Choose or recode a multi-level categorical outcome.")
      }
    } else {
      add(method, "This selected analysis is not feasible with the current selected columns.", "Review the recommended analyses and check column data types.")
    }
  }

  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

capture_result <- function(expr) {
  paste(capture.output(print(expr)), collapse = "\n")
}

describe_numeric <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return("No non-missing numeric values.")
  c(
    Count = length(x),
    Mean = mean(x),
    Median = stats::median(x),
    SD = stats::sd(x),
    Variance = stats::var(x),
    Min = min(x),
    Q1 = stats::quantile(x, 0.25, names = FALSE),
    Q3 = stats::quantile(x, 0.75, names = FALSE),
    Max = max(x),
    MAD_mean = mean(abs(x - mean(x))),
    MAD_median = stats::mad(x, constant = 1),
    CV = ifelse(mean(x) == 0, NA_real_, stats::sd(x) / mean(x))
  )
}

quick_univariate_summary <- function(data, columns = NULL, numeric_stats = NULL, categorical_stats = NULL, table_format = "wide", combined_value = TRUE) {
  if (is.null(columns) || !length(columns)) columns <- names(data)
  columns <- intersect(columns, names(data))
  if (!length(columns)) {
    return(data.frame(Message = "No columns selected and no active dataset columns available."))
  }
  if (is.null(numeric_stats) || !length(numeric_stats)) numeric_stats <- c("N", "Missing", "Mean", "SD", "Median", "IQR")
  if (is.null(categorical_stats) || !length(categorical_stats)) categorical_stats <- c("N", "Missing", "Levels", "Top level", "Top percent")

  rows <- lapply(columns, function(column) {
    x <- data[[column]]
    non_missing <- x[!is.na(x)]
    type <- infer_column_type(x)
    n <- length(x)
    missing <- sum(is.na(x))
    unique_n <- length(unique(non_missing))

    if (is.numeric(x)) {
      shapiro_p <- if (length(non_missing) >= 3 && length(non_missing) <= 5000 && unique_n > 2) {
        tryCatch(round(stats::shapiro.test(non_missing)$p.value, 4), error = function(e) NA_real_)
      } else {
        NA_real_
      }
      wide <- data.frame(
        Column = column,
        Type = type,
        N = n,
        Missing = missing,
        Unique = unique_n,
        Mean = round(mean(non_missing), 4),
        SD = round(stats::sd(non_missing), 4),
        Median = round(stats::median(non_missing), 4),
        IQR = round(stats::IQR(non_missing), 4),
        Min = round(min(non_missing), 4),
        Max = round(max(non_missing), 4),
        TopLevel = "",
        TopN = NA_integer_,
        TopPercent = NA_real_,
        ShapiroP = shapiro_p,
        stringsAsFactors = FALSE
      )
      wide
    } else {
      counts <- sort(table(non_missing), decreasing = TRUE)
      top_level <- if (length(counts)) names(counts)[1] else ""
      top_n <- if (length(counts)) as.integer(counts[[1]]) else NA_integer_
      top_percent <- if (length(non_missing) && length(counts)) round(100 * top_n / length(non_missing), 2) else NA_real_
      wide <- data.frame(
        Column = column,
        Type = type,
        N = n,
        Missing = missing,
        Unique = unique_n,
        Mean = NA_real_,
        SD = NA_real_,
        Median = NA_real_,
        IQR = NA_real_,
        Min = NA_real_,
        Max = NA_real_,
        TopLevel = top_level,
        TopN = top_n,
        TopPercent = top_percent,
        ShapiroP = NA_real_,
        stringsAsFactors = FALSE
      )
      wide
    }
  })

  wide <- do.call(rbind, rows)
  if (!"TopPercent" %in% names(wide)) wide$TopPercent <- NA_real_
  wide$MeanOrPercent <- ifelse(
    is.na(wide$Mean),
    ifelse(is.na(wide$TopPercent), NA_character_, paste0(wide$TopPercent, "%")),
    as.character(wide$Mean)
  )

  numeric_map <- c(
    N = "N", Missing = "Missing", Unique = "Unique", Mean = "Mean", SD = "SD",
    Median = "Median", IQR = "IQR", Min = "Min", Max = "Max", "Shapiro p-value" = "ShapiroP"
  )
  categorical_map <- c(
    N = "N", Missing = "Missing", Levels = "Unique", "Top level" = "TopLevel",
    "Top count" = "TopN", "Top percent" = "TopPercent"
  )
  selected_columns <- unique(c("Column", "Type", unname(numeric_map[numeric_stats]), unname(categorical_map[categorical_stats])))
  if (isTRUE(combined_value)) selected_columns <- unique(c("Column", "Type", "MeanOrPercent", selected_columns))
  selected_columns <- intersect(selected_columns, names(wide))
  wide <- wide[, selected_columns, drop = FALSE]

  if (identical(table_format, "long")) {
    metric_columns <- setdiff(names(wide), c("Column", "Type"))
    long_rows <- lapply(seq_len(nrow(wide)), function(i) {
      data.frame(
        Column = wide$Column[i],
        Type = wide$Type[i],
        Metric = metric_columns,
        Value = as.character(unlist(wide[i, metric_columns, drop = FALSE], use.names = FALSE)),
        stringsAsFactors = FALSE
      )
    })
    return(do.call(rbind, long_rows))
  }

  wide
}

parse_rename_map <- function(text) {
  if (is.null(text) || !nzchar(text)) return(character())
  lines <- trimws(strsplit(text, "\n", fixed = TRUE)[[1]])
  lines <- lines[nzchar(lines) & grepl("=", lines, fixed = TRUE)]
  pairs <- strsplit(lines, "=", fixed = TRUE)
  old <- trimws(vapply(pairs, `[`, character(1), 1))
  new <- trimws(vapply(pairs, function(x) paste(x[-1], collapse = "="), character(1)))
  stats::setNames(new, old)
}

format_number <- function(x, digits = 1) {
  if (is.na(x)) return("")
  formatC(x, format = "f", digits = digits)
}

format_table1_value <- function(x, style = "mean", digits = 1) {
  non_missing <- x[!is.na(x)]
  if (!length(non_missing)) return("")
  if (is.numeric(x)) {
    if (identical(style, "median_iqr")) {
      return(paste0(
        format_number(stats::median(non_missing), digits),
        " [",
        format_number(stats::quantile(non_missing, 0.25, names = FALSE), digits),
        ", ",
        format_number(stats::quantile(non_missing, 0.75, names = FALSE), digits),
        "]"
      ))
    }
    if (identical(style, "mean_sd")) {
      return(paste0(format_number(mean(non_missing), digits), " (", format_number(stats::sd(non_missing), digits), ")"))
    }
    return(format_number(mean(non_missing), digits))
  }
  counts <- sort(table(non_missing), decreasing = TRUE)
  top_n <- as.integer(counts[[1]])
  pct <- 100 * top_n / length(non_missing)
  paste0(top_n, " (", format_number(pct, digits), "%)")
}

table1_summary <- function(data, columns = NULL, variable_names = character(), level_names = character(),
                           numeric_style = "mean", categorical_style = "top",
                           digits = 1, variable_header = "Variable", value_header = "Value",
                           group_columns = NULL) {
  if (is.null(columns) || !length(columns)) columns <- names(data)
  columns <- intersect(columns, names(data))
  group_columns <- intersect(group_columns %||% character(), names(data))
  columns <- setdiff(columns, group_columns)
  if (!length(columns)) return(data.frame(Message = "No columns selected."))

  group_slices <- list()
  if (length(group_columns)) {
    group_factor <- interaction(data[, group_columns, drop = FALSE], drop = TRUE, sep = " / ")
    for (level in levels(group_factor)) {
      group_slices[[level]] <- data[group_factor == level & !is.na(group_factor), , drop = FALSE]
    }
  } else {
    group_slices[[value_header]] <- data
  }

  rows <- list()
  for (column in columns) {
    x <- data[[column]]
    display_name <- if (column %in% names(variable_names)) variable_names[[column]] else column
    if (!is.numeric(x) && identical(categorical_style, "levels")) {
      header_row <- data.frame(Variable = display_name, stringsAsFactors = FALSE)
      for (group_name in names(group_slices)) header_row[[group_name]] <- ""
      rows[[length(rows) + 1]] <- header_row
      counts <- sort(table(x[!is.na(x)]), decreasing = TRUE)
      for (level in names(counts)) {
        key <- paste0(column, ":", level)
        level_name <- if (key %in% names(level_names)) level_names[[key]] else level
        row <- data.frame(Variable = paste0("  ", level_name), stringsAsFactors = FALSE)
        for (group_name in names(group_slices)) {
          group_x <- group_slices[[group_name]][[column]]
          group_counts <- table(group_x[!is.na(group_x)])
          level_n <- if (level %in% names(group_counts)) as.integer(group_counts[[level]]) else 0L
          denom <- sum(group_counts)
          pct <- if (denom > 0) 100 * level_n / denom else NA_real_
          row[[group_name]] <- paste0(level_n, " (", format_number(pct, digits), "%)")
        }
        rows[[length(rows) + 1]] <- row
      }
    } else {
      if (!is.numeric(x) && identical(categorical_style, "top_label")) {
        top_level <- names(sort(table(x[!is.na(x)]), decreasing = TRUE))[1]
        key <- paste0(column, ":", top_level)
        level_name <- if (key %in% names(level_names)) level_names[[key]] else top_level
        display_name <- paste0(display_name, " (", level_name, ")")
      }
      row <- data.frame(Variable = display_name, stringsAsFactors = FALSE)
      for (group_name in names(group_slices)) {
        row[[group_name]] <- format_table1_value(group_slices[[group_name]][[column]], numeric_style, digits)
      }
      rows[[length(rows) + 1]] <- row
    }
  }
  out <- do.call(rbind, rows)
  names(out)[1] <- variable_header
  if (!length(group_columns)) names(out)[2] <- value_header
  out
}

run_pair_test <- function(data, outcome, predictor, method) {
  analysis_data <- clean_for_columns(data, c(outcome, predictor))
  y <- analysis_data[[outcome]]
  x <- analysis_data[[predictor]]
  validate(
    need(nrow(analysis_data) >= 3, "Not enough complete rows."),
    need(length(unique(y)) >= 2, "The outcome column has fewer than two observed values."),
    need(length(unique(x)) >= 2, "The predictor column has fewer than two observed values.")
  )
  f <- stats::as.formula(paste(quote_name(outcome), "~", quote_name(predictor)))

  switch(
    method,
    "Two Sample T-Test (Welch's)" = {
      analysis_data[[predictor]] <- as.factor(analysis_data[[predictor]])
      validate(need(is.numeric(y), "Welch t-test requires a numeric outcome."))
      validate(need(nlevels(analysis_data[[predictor]]) == 2, "Welch t-test requires exactly two groups."))
      stats::t.test(f, data = analysis_data)
    },
    "Two Sample Mann-Whitney U Test" = {
      analysis_data[[predictor]] <- as.factor(analysis_data[[predictor]])
      validate(need(is.numeric(y), "Mann-Whitney requires a numeric or ordinal outcome."))
      validate(need(nlevels(analysis_data[[predictor]]) == 2, "Mann-Whitney requires exactly two groups."))
      stats::wilcox.test(f, data = analysis_data)
    },
    "One Way ANOVA Test" = {
      analysis_data[[predictor]] <- as.factor(analysis_data[[predictor]])
      validate(need(is.numeric(y), "ANOVA requires a numeric outcome."))
      validate(need(nlevels(analysis_data[[predictor]]) >= 3, "ANOVA requires at least three groups."))
      summary(stats::aov(f, data = analysis_data))
    },
    "Kruskal-Wallis Test" = {
      analysis_data[[predictor]] <- as.factor(analysis_data[[predictor]])
      validate(need(is.numeric(y), "Kruskal-Wallis requires a numeric or ordinal outcome."))
      validate(need(nlevels(analysis_data[[predictor]]) >= 2, "Kruskal-Wallis requires at least two groups."))
      stats::kruskal.test(f, data = analysis_data)
    },
    "Levene's Test For Variances" = {
      analysis_data[[predictor]] <- as.factor(analysis_data[[predictor]])
      validate(need(is.numeric(y), "Levene-style variance check requires a numeric outcome."))
      abs_dev <- abs(y - ave(y, analysis_data[[predictor]], FUN = median))
      summary(stats::aov(abs_dev ~ analysis_data[[predictor]]))
    },
    "Chi-squared test for association" = stats::chisq.test(table(y, x)),
    "Fisher test" = stats::fisher.test(table(y, x)),
    "Simple Linear Regression" = {
      validate(need(is.numeric(y), "Linear regression requires a numeric outcome."))
      summary(stats::lm(f, data = analysis_data))
    },
    "Binary Logistic Regression" = {
      analysis_data[[outcome]] <- as.factor(analysis_data[[outcome]])
      validate(need(nlevels(analysis_data[[outcome]]) == 2, "Binary logistic regression requires two outcome levels."))
      summary(stats::glm(f, data = analysis_data, family = stats::binomial()))
    },
    "Multinomial Logistic Regression" = {
      validate(need(requireNamespace("nnet", quietly = TRUE), "Install nnet to run multinomial models."))
      analysis_data[[outcome]] <- as.factor(analysis_data[[outcome]])
      validate(need(nlevels(analysis_data[[outcome]]) > 2, "Multinomial regression requires more than two outcome levels."))
      summary(nnet::multinom(f, data = analysis_data, trace = FALSE))
    },
    "Kolmogorov Smirnov Test" = {
      validate(need(is.numeric(y), "Kolmogorov-Smirnov requires a numeric outcome."))
      stats::ks.test(scale(y), "pnorm")
    },
    stop("Unsupported pairwise method.")
  )
}

run_selected_analyses <- function(data, outcomes, predictors, methods) {
  recs <- recommend_batch(data, outcomes, predictors)
  if (!identical(methods, "All feasible recommended analyses")) {
    recs <- recs[recs$Method %in% methods, , drop = FALSE]
  }
  recs <- recs[recs$Feasible, , drop = FALSE]
  if (!nrow(recs)) return("No feasible analyses match the current selections.")

  output <- character()

  for (i in seq_len(nrow(recs))) {
    row <- recs[i, ]
    header <- paste0("\n\n### ", row$Method, " | Outcome: ", row$Outcome)
    if (nzchar(row$Predictor)) header <- paste0(header, " | Predictor: ", row$Predictor)
    output <- c(output, header, paste0("Reason: ", row$Why))

    result <- tryCatch({
      method <- row$Method
      if (method == "Descriptive statistics calculator") {
        x <- data[[row$Outcome]]
        if (is.numeric(x)) describe_numeric(x) else sort(table(x), decreasing = TRUE)
      } else if (method == "One Sample T-Test") {
        stats::t.test(data[[row$Outcome]], mu = 0)
      } else if (method == "Shapiro-Wilk Test") {
        stats::shapiro.test(stats::na.omit(data[[row$Outcome]]))
      } else if (method == "Correlation calculator") {
        cols <- intersect(strsplit(row$Outcome, ", ")[[1]], numeric_columns(data))
        stats::cor(data[, cols, drop = FALSE], use = "pairwise.complete.obs")
      } else if (method == "Principal component analysis") {
        cols <- intersect(strsplit(row$Outcome, ", ")[[1]], numeric_columns(data))
        summary(stats::prcomp(stats::na.omit(data[, cols, drop = FALSE]), scale. = TRUE))
      } else if (method == "Cluster analysis") {
        cols <- intersect(strsplit(row$Outcome, ", ")[[1]], numeric_columns(data))
        matrix_data <- stats::na.omit(scale(data[, cols, drop = FALSE]))
        stats::kmeans(matrix_data, centers = min(3, nrow(matrix_data)), nstart = 10)
      } else {
        run_pair_test(data, row$Outcome, row$Predictor, method)
      }
    }, error = function(e) paste("Blocked:", conditionMessage(e)))

    output <- c(output, capture_result(result))
  }

  paste(output, collapse = "\n")
}

ui <- page_navbar(
  title = "GCBS Statistical Analysis Workbench",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  header = tags$style(HTML("
    html, body {
      min-height: 100%;
      overflow-y: auto;
    }

    .bslib-sidebar-layout > .main {
      overflow: visible;
      min-width: 0;
    }

    .card {
      min-height: 220px;
      max-width: 100%;
      overflow: auto;
    }

    .card-body {
      overflow: auto;
    }

    .shiny-table-output,
    .shiny-html-output {
      overflow-x: auto;
      max-width: 100%;
    }

    .transform-controls-card .card-body {
      max-height: calc(100vh - 255px);
      overflow-y: auto;
    }

    .transform-controls-scroll {
      min-width: 0;
    }

    table {
      min-width: 640px;
      width: max-content;
      max-width: none;
    }

    th {
      white-space: pre-line;
      vertical-align: bottom;
    }

    pre {
      white-space: pre-wrap;
      overflow-x: auto;
    }

    .selectize-control {
      min-width: 100%;
    }

    #quick_columns + .selectize-control .selectize-input,
    #quick_group_columns + .selectize-control .selectize-input {
      min-height: 74px;
      height: auto;
      white-space: normal;
      align-items: flex-start;
      overflow-y: auto;
      max-height: 150px;
    }

    #quick_columns + .selectize-control .item,
    #quick_group_columns + .selectize-control .item {
      white-space: normal;
      overflow-wrap: anywhere;
      line-height: 1.2;
      margin-bottom: 3px;
    }

    .tab-content {
      overflow: visible;
    }

    .split-row {
      display: grid;
      grid-template-columns: minmax(280px, 1fr) 10px minmax(280px, 1fr);
      gap: 0;
      align-items: stretch;
      width: 100%;
      max-width: 100%;
      overflow: hidden;
    }

    .split-panel {
      min-width: 0;
      max-width: 100%;
      overflow: auto;
    }

    .split-panel .card {
      height: 100%;
      resize: vertical;
      max-width: 100%;
    }

    .split-handle {
      cursor: col-resize;
      background: #dee2e6;
      border-left: 1px solid #ced4da;
      border-right: 1px solid #ced4da;
    }

    .split-handle:hover {
      background: #adb5bd;
    }

    .figure-workspace {
      min-height: calc(100vh - 170px);
    }

    .figure-workspace .bslib-sidebar-layout,
    .figure-workspace .bslib-sidebar-layout > .main {
      min-height: calc(100vh - 190px);
      max-width: 100%;
      overflow: hidden;
    }

    .figure-workspace .sidebar {
      max-height: calc(100vh - 190px);
      overflow-y: auto;
      overflow-x: hidden;
    }

    .data-cleaning-workspace .bslib-sidebar-layout,
    .data-cleaning-workspace .bslib-sidebar-layout > .main {
      min-height: calc(100vh - 190px);
      max-width: 100%;
    }

    .data-cleaning-workspace .sidebar {
      max-height: calc(100vh - 190px);
      overflow-y: auto;
      overflow-x: hidden;
      padding-bottom: 1rem;
    }

    .univariate-workspace .bslib-sidebar-layout,
    .univariate-workspace .bslib-sidebar-layout > .main {
      min-height: calc(100vh - 190px);
      max-width: 100%;
    }

    .univariate-workspace .sidebar {
      max-height: calc(100vh - 190px);
      overflow-y: auto;
      overflow-x: hidden;
      padding-bottom: 1rem;
    }

    .figure-card {
      resize: none;
      min-height: 0;
      max-width: 100%;
      overflow: hidden;
    }

    .figure-card .card-body {
      overflow: auto;
      max-height: calc(100vh - 230px);
    }

    .figure-card .shiny-plot-output {
      max-width: 100%;
      width: 100% !important;
      height: min(520px, calc(100vh - 290px)) !important;
      min-height: 320px;
    }
  ")),
  footer = tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function () {
      document.querySelectorAll('.split-row').forEach(function (row) {
        var handle = row.querySelector('.split-handle');
        if (!handle) return;
        handle.addEventListener('mousedown', function (event) {
          event.preventDefault();
          var startX = event.clientX;
          var bounds = row.getBoundingClientRect();
          var columns = window.getComputedStyle(row).gridTemplateColumns.split(' ');
          var leftStart = parseFloat(columns[0]);
          var rightStart = parseFloat(columns[2]);
          function onMove(moveEvent) {
            var delta = moveEvent.clientX - startX;
            var minWidth = 280;
            var available = bounds.width - 10;
            var left = Math.max(minWidth, Math.min(available - minWidth, leftStart + delta));
            var right = available - left;
            row.style.gridTemplateColumns = left + 'px 10px ' + right + 'px';
          }
          function onUp() {
            document.removeEventListener('mousemove', onMove);
            document.removeEventListener('mouseup', onUp);
          }
          document.addEventListener('mousemove', onMove);
          document.addEventListener('mouseup', onUp);
        });
      });
    });
  ")),
  nav_panel(
    "Data Cleaning",
    div(
      class = "data-cleaning-workspace",
    navset_tab(
      nav_panel(
        "Import",
        layout_sidebar(
          sidebar = sidebar(
            width = 380,
            checkboxInput("use_example", "Use built-in example dataset", value = TRUE),
            uiOutput("sample_disclaimer_sidebar"),
            hr(),
            fileInput("data_file", "Import primary dataset", accept = c(".csv", ".tsv", ".txt", ".xls", ".xlsx")),
            numericInput("import_skip", "Rows to skip before data starts", value = 0, min = 0, step = 1),
            checkboxInput("import_header", "First imported row contains column names", value = TRUE),
            selectInput("import_delimiter", "Delimiter for csv/txt/tsv", choices = c(
              "Auto from file type" = "auto",
              "Comma (,)" = ",",
              "Tab" = "\t",
              "Semicolon (;)" = ";",
              "Pipe (|)" = "|",
              "Whitespace" = ""
            )),
            selectizeInput(
              "import_columns",
              "Columns to keep after preview",
              choices = character(),
              multiple = TRUE,
              options = list(placeholder = "Upload and preview a dataset first")
            ),
            actionButton("apply_import", "Apply import preview", class = "btn-primary")
          ),
          uiOutput("sample_disclaimer_main"),
          layout_columns(
            card(
              card_header("Import Preview"),
              uiOutput("import_status"),
              tableOutput("import_preview")
            ),
            card(
              card_header("Active Dataset"),
              tableOutput("active_dataset_summary")
            ),
            col_widths = c(7, 5)
          )
        )
      ),
      nav_panel(
        "Transform",
        layout_sidebar(
          sidebar = sidebar(
            width = 380,
            checkboxInput("drop_missing", "Drop rows missing selected analysis columns", value = TRUE),
            tags$p(class = "text-muted", "Use the cards on the right to convert types, parse dates, create derived columns, recode values, or rename columns.")
          ),
          navset_tab(
            selected = "Preview",
            nav_panel(
              "Preview",
              card(card_header("Transformed Dataset Preview"), tableOutput("preview")),
              uiOutput("transform_message")
            ),
            nav_panel(
              "Type & Date",
              layout_columns(
                card(
                  class = "transform-controls-card",
                  card_header("Column Type & Date"),
                  tags$div(
                    class = "transform-controls-scroll",
                    tags$h5("Convert Column Type"),
                    selectInput("transform_column", "Column to transform", choices = c("No active dataset" = "")),
                    radioButtons("transform_type", "Transform selected column", choices = c("No change", "Factor", "Character", "Numeric", "Date"), inline = TRUE),
                    actionButton("apply_transform", "Apply transform", class = "btn-primary"),
                    hr(),
                    tags$h5("Parse Date Column"),
                    selectInput("date_transform_column", "Column containing dates", choices = c("No active dataset" = "")),
                    uiOutput("date_format_guess"),
                    selectInput("date_format", "Date format", choices = date_format_choices, selected = "auto"),
                    checkboxInput("date_replace_column", "Replace existing column", value = TRUE),
                    textInput("date_new_column", "New date column name", value = "parsed_date"),
                    actionButton("apply_date_transform", "Parse date", class = "btn-primary")
                  )
                ),
                card(
                  card_header("Preview"),
                  tableOutput("type_date_preview")
                ),
                col_widths = c(6, 6)
              )
            ),
            nav_panel(
              "Create Column",
              layout_columns(
                card(
                  card_header("Derived Column"),
                  textInput("derive_new_column", "New column name", value = "new_column"),
                  selectInput("derive_operation", "Operation", choices = c(
                    "Copy selected column" = "copy",
                    "Numeric arithmetic" = "arithmetic",
                    "Combine text columns" = "combine_text",
                    "Date difference in days" = "date_diff",
                    "Extract date part" = "date_part"
                  )),
                  selectInput("derive_column_a", "First/source column", choices = c("No active dataset" = "")),
                  uiOutput("derive_operation_controls"),
                  actionButton("apply_derive_column", "Create column", class = "btn-primary")
                ),
                card(
                  navset_tab(
                    selected = "Preview",
                    nav_panel(
                      "Preview",
                      tableOutput("derive_preview")
                    ),
                    nav_panel(
                      "Current Columns",
                      tableOutput("profile_transform")
                    )
                  )
                ),
                col_widths = c(6, 6)
              )
            ),
            nav_panel(
              "Recode Values",
              layout_columns(
                card(
                  card_header("Convert Specific Values"),
                  selectInput("recode_column", "Column to recode", choices = c("No active dataset" = "")),
                  selectizeInput("recode_old_value", "Value to replace", choices = character(), multiple = FALSE, options = list(create = TRUE, placeholder = "Select or type a current value")),
                  textInput("recode_new_value", "New value", value = ""),
                  selectInput("recode_new_type", "Store new value as", choices = c(
                    "Keep column type where possible" = "keep",
                    "Character/text" = "character",
                    "Numeric" = "numeric",
                    "Logical TRUE/FALSE" = "logical",
                    "Missing value (NA)" = "missing"
                  )),
                  actionButton("apply_recode_value", "Apply value recode", class = "btn-primary")
                ),
                card(
                  card_header("Value Preview"),
                  tableOutput("recode_value_preview")
                ),
                col_widths = c(6, 6)
              )
            ),
            nav_panel(
              "Rename",
            card(
              card_header("Rename Columns"),
              uiOutput("rename_columns_ui"),
              actionButton("apply_column_renames", "Apply column names", class = "btn-primary"),
              uiOutput("rename_columns_message")
            ),
            )
          )
        )
      ),
      nav_panel(
        "Join",
        layout_sidebar(
          sidebar = sidebar(
            width = 380,
            fileInput(
              "join_files",
              "Lookup datasets for joining",
              accept = c(".csv", ".tsv", ".txt", ".xls", ".xlsx"),
              multiple = TRUE
            ),
            selectInput("lookup_dataset", "Lookup dataset", choices = c("No lookup dataset uploaded" = "")),
            selectInput("join_type", "Join type", choices = c(
              "left_join" = "left",
              "inner_join" = "inner",
              "full_join" = "full",
              "right_join" = "right"
            )),
            selectInput("join_relationship", "Expected key relationship", choices = c(
              "one-to-one" = "one_to_one",
              "one-to-many" = "one_to_many",
              "many-to-one" = "many_to_one",
              "many-to-many" = "many_to_many"
            ), selected = "many_to_many"),
            selectInput("primary_key", "Primary key", choices = c("No primary key selected" = "")),
            selectInput("lookup_key", "Lookup key", choices = c("No lookup key selected" = "")),
            actionButton("preview_join", "Preview join impact", class = "btn-secondary"),
            actionButton("apply_join", "Apply join", class = "btn-primary"),
            actionButton("cancel_join", "Stop / clear join preview")
          ),
          navset_tab(
            selected = "Preview",
            nav_panel(
              "Preview",
              card(
                card_header("Join Impact Preview"),
                uiOutput("join_warnings"),
                tableOutput("join_summary"),
                tableOutput("join_preview")
              )
            ),
            nav_panel(
              "Guide",
              uiOutput("join_guide_text"),
              div(
                class = "split-row",
                div(
                  class = "split-panel",
                  card(
                    card_header("Example Input Tables"),
                    tags$h6("Primary table"),
                    tableOutput("join_example_primary"),
                    tags$h6("Lookup table"),
                    tableOutput("join_example_lookup")
                  )
                ),
                div(class = "split-handle", title = "Drag to resize panels"),
                div(
                  class = "split-panel",
                  card(
                    card_header("Result for Selected Join"),
                    tableOutput("join_example_result"),
                    uiOutput("join_graphic")
                  )
                )
              )
            )
          )
        )
      ),
      nav_panel(
        "Profile",
        layout_columns(
          card(card_header("Column Profile"), tableOutput("profile")),
          card(card_header("Active Dataset Preview"), tableOutput("preview_profile")),
          col_widths = c(5, 7)
        )
      ),
      nav_panel(
        "Export",
        card(
          card_header("Export Cleaned Dataset"),
          downloadButton("download_csv", "Download CSV"),
          downloadButton("download_tsv", "Download TSV / Excel-compatible table")
        )
      )
    )
    )
  ),
  nav_panel(
    "Analyses",
    layout_sidebar(
      sidebar = sidebar(
        width = 360,
        selectizeInput(
          "outcomes",
          "Outcome / response columns",
          choices = character(),
          multiple = TRUE,
          options = list(placeholder = "Select one or more outcome columns")
        ),
        selectizeInput(
          "predictors",
          "Predictor / grouping columns",
          choices = character(),
          multiple = TRUE,
          options = list(placeholder = "Select optional predictor/grouping columns")
        ),
        selectizeInput(
          "methods",
          "Analyses to run",
          choices = character(),
          multiple = TRUE,
          options = list(placeholder = "Use all feasible analyses or select specific methods")
        ),
        actionButton("run_test", "Run selected analyses", class = "btn-primary")
      ),
      navset_tab(
        id = "analysis_tabs",
        nav_panel(
          "Recommendations",
          card(
            card_header("Recommended Tests and Guardrails"),
            tableOutput("recommendations"),
            tags$p(
              class = "text-muted",
              "Recommendations are generated for each selected outcome/predictor pair, plus standalone summaries for selected columns. Infeasible methods are shown with reasons instead of being run."
            )
          )
        ),
        nav_panel(
          "Selected Data",
          card(
            card_header("Selected Analysis Columns"),
            tableOutput("selected_analysis_data"),
            tags$p(
              class = "text-muted",
              "This view shows only the selected outcome and predictor columns from the current active dataset, after the missing-row option is applied."
            )
          )
        ),
        nav_panel(
          "Results",
          layout_columns(
            card(card_header("Analysis Warnings and Suggested Fixes"), uiOutput("analysis_warning_ui")),
            card(card_header("Batch Results"), verbatimTextOutput("test_result")),
            card(card_header("Selection Explanation"), uiOutput("test_explanation")),
            col_widths = c(12, 7, 5)
          )
        ),
        nav_panel(
          "Univariate Summary",
          div(
            class = "univariate-workspace",
          layout_sidebar(
            sidebar = sidebar(
              width = 360,
              checkboxInput("quick_use_selected", "Use selected analysis columns", value = TRUE),
              selectizeInput(
                "quick_columns",
                "Columns to summarize",
                choices = character(),
                multiple = TRUE,
                options = list(placeholder = "Leave empty to use all active dataset columns")
              ),
              selectizeInput(
                "quick_group_columns",
                "Group columns",
                choices = character(),
                multiple = TRUE,
                options = list(placeholder = "Optional: each group appears as a separate value column")
              ),
              radioButtons("quick_table_format", "Table format", choices = c("Table 1" = "table1", "Wide" = "wide", "Long" = "long"), selected = "table1", inline = TRUE),
              checkboxInput("quick_single_value", "Display each summary as a single value", value = TRUE),
              uiOutput("quick_summary_options"),
              conditionalPanel(
                condition = "input.quick_table_format === 'table1'",
                selectInput("quick_numeric_style", "Numeric value format", choices = c(
                  "Mean" = "mean",
                  "Mean (SD)" = "mean_sd",
                  "Median [IQR]" = "median_iqr"
                )),
                selectInput("quick_categorical_style", "Categorical value format", choices = c(
                  "Top level count (%)" = "top",
                  "Variable (top level) count (%)" = "top_label",
                  "Expand all levels" = "levels"
                )),
                numericInput("quick_digits", "Decimal places", value = 1, min = 0, max = 4, step = 1),
                actionButton("open_quick_label_modal", "Edit table labels", class = "btn-secondary")
              ),
              checkboxInput("quick_combined_value", "Include combined Mean or Percent column", value = TRUE),
              actionButton("run_quick_univariate", "Generate Univariate Summary", class = "btn-secondary")
            ),
            card(
              card_header(uiOutput("quick_table_title_display")),
              tableOutput("quick_univariate_table"),
              downloadButton("download_quick_summary_csv", "Export CSV"),
              downloadButton("download_quick_summary_tsv", "Export TSV"),
              tags$p(
                class = "text-muted",
                "Use selected analysis columns, choose explicit columns, or leave both empty to summarize the full active dataset."
              )
            )
          )
          )
        ),
        nav_panel(
          "Figures",
          div(
            class = "figure-workspace",
          layout_sidebar(
            sidebar = sidebar(
              width = 360,
              selectInput("figure_type", "Graph type", choices = c(
                "Auto" = "auto",
                "Scatter plot" = "scatter",
                "Box plot" = "box",
                "Histogram" = "histogram",
                "Bar graph" = "bar",
                "Violin plot" = "violin",
                "Line chart" = "line"
              )),
              selectInput("figure_x", "X axis / category", choices = c("No active dataset" = "")),
              uiOutput("figure_y_control"),
              selectInput("figure_group", "Color / group", choices = c("None" = "")),
              textInput("figure_title", "Title", value = ""),
              textInput("figure_x_label", "X axis label", value = ""),
              uiOutput("figure_y_label_control"),
              uiOutput("figure_legend_controls"),
              layout_columns(
                selectInput("figure_primary_color_name", "Primary color", choices = plot_color_choices, selected = "#2a9d8f"),
                textInput("figure_primary_color", "Custom primary hex", value = "#2a9d8f"),
                col_widths = c(6, 6)
              ),
              layout_columns(
                selectInput("figure_secondary_color_name", "Secondary color", choices = plot_color_choices, selected = "#e76f51"),
                textInput("figure_secondary_color", "Custom secondary hex", value = "#e76f51"),
                col_widths = c(6, 6)
              ),
              uiOutput("figure_group_palette_control"),
              uiOutput("figure_point_size_control"),
              uiOutput("figure_bins_control"),
              uiOutput("figure_smooth_control"),
              uiOutput("figure_flip_axes_control"),
              downloadButton("download_plot", "Export plot as PNG")
            ),
            card(
              class = "figure-card",
              card_header("Figure Preview"),
              plotOutput("plot", height = "100%")
            )
          )
          )
        ),
        nav_panel(
          "Methods",
          card(
            card_header("Stats Kingdom Method Catalog"),
            tableOutput("methods_table"),
            tags$p(
              class = "text-muted",
              "Catalog-only entries are listed for completeness from the Stats Kingdom index. They need additional dedicated inputs before they can be run safely inside this dataset workflow."
            ),
            tags$p(tags$a(href = "https://www.statskingdom.com/index.html", target = "_blank", "Stats Kingdom index"))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  data_store <- reactiveVal(example_data)
  imported_data_store <- reactiveVal(NULL)
  lookup_store <- reactiveVal(list())
  pending_join <- reactiveVal(NULL)
  last_result <- reactiveVal(NULL)
  quick_result <- reactiveVal(NULL)
  rename_message <- reactiveVal(NULL)
  transform_message <- reactiveVal(NULL)
  analysis_warnings <- reactiveVal(data.frame())
  quick_label_settings <- reactiveVal(list(
    title = "Table 1. Patient Baseline Demographics and Clinical Characteristics of the Study",
    variable_header = "Variable",
    value_header = "Value",
    variable_names = character(),
    level_names = character()
  ))

  observeEvent(input$use_example, {
    if (isTRUE(input$use_example)) {
      data_store(example_data)
    } else if (!is.null(imported_data_store())) {
      data_store(imported_data_store())
    } else {
      data_store(data.frame())
    }
    pending_join(NULL)
    last_result(NULL)
    quick_result(NULL)
  })

  import_candidate <- reactive({
    req(input$data_file)
    read_dataset(
      input$data_file,
      skip = input$import_skip,
      header = isTRUE(input$import_header),
      delimiter = input$import_delimiter,
      n_max = 50
    )
  })

  observe({
    req(input$data_file)
    preview <- tryCatch(import_candidate(), error = function(e) NULL)
    req(preview)
    current_columns <- isolate(input$import_columns)
    selected <- if (length(current_columns)) intersect(current_columns, names(preview)) else names(preview)
    updateSelectizeInput(session, "import_columns", choices = names(preview), selected = selected, server = TRUE)
  })

  observeEvent(input$apply_import, {
    req(input$data_file)
    imported <- read_dataset(
      input$data_file,
      skip = input$import_skip,
      header = isTRUE(input$import_header),
      delimiter = input$import_delimiter,
      select_columns = input$import_columns
    )
    validate(need(ncol(imported) > 0, "Choose at least one column to import."))
    imported_data_store(imported)
    data_store(imported)
    pending_join(NULL)
    last_result(NULL)
    quick_result(NULL)
    updateCheckboxInput(session, "use_example", value = FALSE)
  })

  observeEvent(input$join_files, {
    lookups <- read_lookup_files(input$join_files)
    lookup_store(lookups)
    lookup_choices <- if (length(lookups)) names(lookups) else c("No lookup dataset uploaded" = "")
    updateSelectInput(session, "lookup_dataset", choices = lookup_choices, selected = lookup_choices[[1]])
    if (length(lookups)) {
      updateSelectInput(session, "lookup_key", choices = names(lookups[[1]]))
    } else {
      updateSelectInput(session, "lookup_key", choices = c("No lookup key selected" = ""), selected = "")
    }
  })

  observeEvent(input$lookup_dataset, {
    lookups <- lookup_store()
    if (!length(input$lookup_dataset) || !nzchar(input$lookup_dataset) || !input$lookup_dataset %in% names(lookups)) {
      updateSelectInput(session, "lookup_key", choices = c("No lookup key selected" = ""), selected = "")
      return()
    }
    updateSelectInput(session, "lookup_key", choices = names(lookups[[input$lookup_dataset]]))
  })

  observeEvent(data_store(), {
    data <- data_store()
    choices <- names(data)
    default_outcomes <- intersect(c("outcome_score", "baseline_score", "response"), choices)
    default_predictors <- intersect(c("treatment", "sex", "age"), choices)
    if (!length(default_outcomes)) default_outcomes <- head(choices, min(3, length(choices)))
    if (!length(default_predictors)) default_predictors <- head(setdiff(choices, default_outcomes), min(3, length(setdiff(choices, default_outcomes))))
    current_outcomes <- isolate(input$outcomes)
    current_predictors <- isolate(input$predictors)
    selected_outcomes <- if (length(current_outcomes)) intersect(current_outcomes, choices) else default_outcomes
    selected_predictors <- if (length(current_predictors)) intersect(current_predictors, choices) else default_predictors

    updateSelectizeInput(session, "outcomes", choices = choices, selected = selected_outcomes, server = TRUE)
    updateSelectizeInput(session, "predictors", choices = choices, selected = selected_predictors, server = TRUE)
    transform_choices <- if (length(choices)) choices else c("No active dataset" = "")
    key_choices <- if (length(choices)) choices else c("No primary key selected" = "")
    group_choices <- c("None" = "", choices)
    keep_selected <- function(current, available, fallback) {
      if (length(current) && is_nonempty(current) && current %in% available) current else fallback
    }
    selected_transform_column <- keep_selected(isolate(input$transform_column), choices, transform_choices[[1]])
    selected_date_transform_column <- keep_selected(isolate(input$date_transform_column), choices, transform_choices[[1]])
    selected_derive_column_a <- keep_selected(isolate(input$derive_column_a), choices, transform_choices[[1]])
    selected_derive_column_b <- keep_selected(
      isolate(input$derive_column_b),
      choices,
      if (length(choices) > 1) choices[[2]] else transform_choices[[1]]
    )
    selected_recode_column <- keep_selected(isolate(input$recode_column), choices, transform_choices[[1]])
    selected_primary_key <- keep_selected(isolate(input$primary_key), choices, key_choices[[1]])
    updateSelectInput(session, "transform_column", choices = transform_choices, selected = selected_transform_column)
    updateSelectInput(session, "date_transform_column", choices = transform_choices, selected = selected_date_transform_column)
    updateSelectInput(session, "derive_column_a", choices = transform_choices, selected = selected_derive_column_a)
    updateSelectInput(session, "derive_column_b", choices = transform_choices, selected = selected_derive_column_b)
    updateSelectInput(session, "recode_column", choices = transform_choices, selected = selected_recode_column)
    if (length(choices) && selected_recode_column %in% choices) {
      recode_values <- sort(unique(as.character(data[[selected_recode_column]][!is.na(data[[selected_recode_column]])])))
      current_recode_value <- isolate(input$recode_old_value)
      selected_recode_value <- if (length(current_recode_value) && current_recode_value %in% recode_values) {
        current_recode_value
      } else if (length(recode_values)) {
        recode_values[[1]]
      } else {
        character()
      }
      updateSelectizeInput(session, "recode_old_value", choices = recode_values, selected = selected_recode_value, server = TRUE)
    } else {
      updateSelectizeInput(session, "recode_old_value", choices = character(), selected = character(), server = TRUE)
    }
    updateSelectInput(session, "primary_key", choices = key_choices, selected = selected_primary_key)
    updateSelectInput(session, "figure_x", choices = transform_choices, selected = if (length(selected_predictors)) selected_predictors[1] else transform_choices[[1]])
    updateSelectInput(session, "figure_y", choices = transform_choices, selected = if (length(selected_outcomes)) selected_outcomes[1] else transform_choices[[1]])
    updateSelectInput(session, "figure_group", choices = group_choices, selected = "")
    updateSelectizeInput(session, "quick_columns", choices = choices, selected = character(), server = TRUE)
    updateSelectizeInput(session, "quick_group_columns", choices = choices, selected = character(), server = TRUE)
    rename_message(NULL)
    transform_message(NULL)
  }, ignoreInit = FALSE)

  observeEvent(data_store(), {
    choices <- c("All feasible recommended analyses", runnable_methods)
    current_methods <- isolate(input$methods)
    selected <- if (length(current_methods)) intersect(current_methods, choices) else "All feasible recommended analyses"
    updateSelectizeInput(session, "methods", choices = choices, selected = selected, server = TRUE)
  }, ignoreInit = FALSE)

  observeEvent(input$figure_primary_color_name, {
    if (is_nonempty(input$figure_primary_color_name) && input$figure_primary_color_name != "custom") {
      updateTextInput(session, "figure_primary_color", value = input$figure_primary_color_name)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$figure_secondary_color_name, {
    if (is_nonempty(input$figure_secondary_color_name) && input$figure_secondary_color_name != "custom") {
      updateTextInput(session, "figure_secondary_color", value = input$figure_secondary_color_name)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$figure_primary_color, {
    updateSelectInput(session, "figure_primary_color_name", selected = dropdown_for_color(input$figure_primary_color))
  }, ignoreInit = TRUE)

  observeEvent(input$figure_secondary_color, {
    updateSelectInput(session, "figure_secondary_color_name", selected = dropdown_for_color(input$figure_secondary_color))
  }, ignoreInit = TRUE)

  observeEvent(input$recode_column, {
    data <- data_store()
    column <- input$recode_column
    if (!is_nonempty(column) || !column %in% names(data)) {
      updateSelectizeInput(session, "recode_old_value", choices = character(), selected = character(), server = TRUE)
      return()
    }
    values <- unique(as.character(data[[column]][!is.na(data[[column]])]))
    values <- sort(values)
    updateSelectizeInput(session, "recode_old_value", choices = values, selected = if (length(values)) values[[1]] else character(), server = TRUE)
  }, ignoreInit = FALSE)

  observeEvent(input$apply_transform, {
    data <- data_store()
    column <- input$transform_column
    req(column, nzchar(column), column %in% names(data))
    data[[column]] <- switch(
      input$transform_type,
      Factor = as.factor(data[[column]]),
      Character = as.character(data[[column]]),
      Numeric = suppressWarnings(as.numeric(data[[column]])),
      Date = parse_date_column(data[[column]], "auto"),
      data[[column]]
    )
    data_store(data)
    if (!isTRUE(input$use_example)) imported_data_store(data)
    quick_result(NULL)
    transform_message(paste("Updated", column, "to", infer_column_type(data[[column]]), "."))
  })

  observeEvent(input$apply_date_transform, {
    data <- data_store()
    column <- input$date_transform_column
    req(column, nzchar(column), column %in% names(data))
    parsed <- parse_date_column(data[[column]], input$date_format)
    validate(need(any(!is.na(parsed)), "No values could be parsed as dates. Choose a different date format or inspect the source values."))
    target_column <- if (isTRUE(input$date_replace_column)) column else trimws(input$date_new_column %||% "")
    validate(
      need(is_nonempty(target_column), "Enter a new date column name or choose to replace the existing column."),
      need(isTRUE(input$date_replace_column) || !target_column %in% names(data), "The new date column name already exists.")
    )
    data[[target_column]] <- parsed
    data_store(data)
    if (!isTRUE(input$use_example)) imported_data_store(data)
    quick_result(NULL)
    parsed_n <- sum(!is.na(parsed))
    transform_message(paste0("Parsed ", parsed_n, " of ", length(parsed), " value(s) in ", column, " as Date."))
  })

  observeEvent(input$apply_derive_column, {
    data <- data_store()
    new_column <- trimws(input$derive_new_column %||% "")
    column_a <- input$derive_column_a
    operation <- input$derive_operation
    validate(
      need(ncol(data) > 0, "No active dataset."),
      need(is_nonempty(new_column), "Enter a new column name."),
      need(!new_column %in% names(data), "The new column name already exists."),
      need(is_nonempty(column_a) && column_a %in% names(data), "Choose a valid source column.")
    )

    new_values <- switch(
      operation,
      copy = data[[column_a]],
      arithmetic = {
        operator <- input$derive_arithmetic_operator %||% "+"
        operand_type <- input$derive_operand_type %||% "constant"
        left <- suppressWarnings(as.numeric(data[[column_a]]))
        right <- if (identical(operand_type, "column")) {
          column_b <- input$derive_column_b
          validate(need(is_nonempty(column_b) && column_b %in% names(data), "Choose a valid second column."))
          suppressWarnings(as.numeric(data[[column_b]]))
        } else {
          as.numeric(input$derive_constant %||% 0)
        }
        validate(need(!all(is.na(left)), "The source column could not be converted to numeric."))
        switch(
          operator,
          "+" = left + right,
          "-" = left - right,
          "*" = left * right,
          "/" = ifelse(right == 0, NA_real_, left / right),
          "log" = ifelse(left > 0, log(left), NA_real_),
          "sqrt" = ifelse(left >= 0, sqrt(left), NA_real_),
          left
        )
      },
      combine_text = {
        column_b <- input$derive_column_b
        validate(need(is_nonempty(column_b) && column_b %in% names(data), "Choose a valid second column."))
        separator <- input$derive_separator %||% " "
        paste(as.character(data[[column_a]]), as.character(data[[column_b]]), sep = separator)
      },
      date_diff = {
        column_b <- input$derive_column_b
        validate(need(is_nonempty(column_b) && column_b %in% names(data), "Choose a valid second date column."))
        start_date <- parse_date_column(data[[column_a]], "auto")
        end_date <- parse_date_column(data[[column_b]], "auto")
        as.numeric(end_date - start_date)
      },
      date_part = {
        dates <- parse_date_column(data[[column_a]], "auto")
        part <- input$derive_date_part %||% "year"
        date_parts <- as.POSIXlt(dates)
        switch(
          part,
          year = date_parts$year + 1900,
          month = date_parts$mon + 1,
          day = date_parts$mday,
          weekday = weekdays(dates),
          dates
        )
      },
      data[[column_a]]
    )

    data[[new_column]] <- new_values
    data_store(data)
    if (!isTRUE(input$use_example)) imported_data_store(data)
    quick_result(NULL)
    transform_message(paste("Created new column", new_column, "."))
  })

  observeEvent(input$apply_recode_value, {
    data <- data_store()
    column <- input$recode_column
    old_value <- input$recode_old_value
    validate(
      need(ncol(data) > 0, "No active dataset."),
      need(is_nonempty(column) && column %in% names(data), "Choose a valid column."),
      need(is_nonempty(old_value), "Choose or type a value to replace.")
    )
    replacement <- coerce_recode_value(input$recode_new_value, input$recode_new_type, data[[column]])
    matches <- !is.na(data[[column]]) & as.character(data[[column]]) == old_value
    validate(need(any(matches), "No cells match the value to replace."))
    if (is.factor(data[[column]])) data[[column]] <- as.character(data[[column]])
    data[[column]][matches] <- replacement
    if (identical(input$recode_new_type, "logical")) data[[column]] <- as.logical(data[[column]])
    data_store(data)
    if (!isTRUE(input$use_example)) imported_data_store(data)
    quick_result(NULL)
    transform_message(paste0("Replaced ", sum(matches), " value(s) in ", column, "."))
  })

  observeEvent(input$preview_join, {
    data <- data_store()
    lookups <- lookup_store()
    validate(need(nrow(data) > 0 && ncol(data) > 0, "No active primary dataset is available. Use the example dataset or apply an imported dataset first."))
    req(input$lookup_dataset, input$lookup_dataset %in% names(lookups))
    lookup <- lookups[[input$lookup_dataset]]
    req(input$primary_key, input$lookup_key, input$join_type)
    validate(
      need(input$primary_key %in% names(data), "Choose a valid primary key."),
      need(input$lookup_key %in% names(lookup), "Choose a valid lookup key.")
    )
    pending_join(join_diagnostics(
      data,
      lookup,
      input$primary_key,
      input$lookup_key,
      input$join_type,
      input$join_relationship
    ))
  })

  observeEvent(input$apply_join, {
    planned <- pending_join()
    req(planned)
    data_store(planned$joined)
    pending_join(NULL)
    quick_result(NULL)
  })

  observeEvent(input$cancel_join, {
    pending_join(NULL)
  })

  selected_columns <- reactive({
    unique(c(input$outcomes, input$predictors))
  })

  quick_summary_columns <- reactive({
    data <- analysis_data()
    if (!ncol(data)) return(character())
    cols <- if (isTRUE(input$quick_use_selected)) intersect(selected_columns(), names(data)) else character()
    if (!length(cols)) cols <- intersect(input$quick_columns, names(data))
    if (!length(cols)) cols <- names(data)
    setdiff(cols, intersect(input$quick_group_columns, names(data)))
  })

  quick_categorical_levels <- reactive({
    data <- analysis_data()
    cols <- quick_summary_columns()
    cols <- cols[cols %in% names(data)]
    cat_cols <- cols[!vapply(data[, cols, drop = FALSE], is.numeric, logical(1))]
    rows <- list()
    for (column in cat_cols) {
      levels <- sort(unique(as.character(data[[column]][!is.na(data[[column]])])))
      if (length(levels)) {
        rows[[length(rows) + 1]] <- data.frame(
          Column = column,
          Level = levels,
          Key = paste0(column, ":", levels),
          stringsAsFactors = FALSE
        )
      }
    }
    if (!length(rows)) {
      return(data.frame(Column = character(), Level = character(), Key = character(), stringsAsFactors = FALSE))
    }
    do.call(rbind, rows)
  })

  generate_quick_summary <- function() {
    data <- analysis_data()
    validate(need(ncol(data) > 0, "No active dataset. Use the built-in example or apply an imported dataset first."))
    cols <- quick_summary_columns()
    group_cols <- intersect(input$quick_group_columns, names(data))
    summary_stats <- input$quick_summary_stats %||% character()
    numeric_stats <- intersect(summary_stats, c("N", "Missing", "Unique", "Mean", "SD", "Median", "IQR", "Min", "Max", "Shapiro p-value"))
    categorical_stats <- intersect(summary_stats, c("N", "Missing", "Unique", "Top level", "Top count", "Top percent"))
    if ("Percent" %in% summary_stats) categorical_stats <- unique(c(categorical_stats, "Top percent"))
    if ("Count (%)" %in% summary_stats) categorical_stats <- unique(c(categorical_stats, "Top count", "Top percent"))

    if (identical(input$quick_table_format, "table1")) {
      numeric_style <- input$quick_numeric_style
      if ("Median [IQR]" %in% summary_stats) numeric_style <- "median_iqr"
      if ("Mean (SD)" %in% summary_stats) numeric_style <- "mean_sd"
      if ("Mean" %in% summary_stats && !"Mean (SD)" %in% summary_stats) numeric_style <- "mean"
      categorical_style <- input$quick_categorical_style
      if ("Percent" %in% summary_stats) categorical_style <- "top_label"
      if ("Count (%)" %in% summary_stats) categorical_style <- "top"
      settings <- quick_label_settings()
      return(table1_summary(
        data,
        cols,
        variable_names = settings$variable_names %||% character(),
        level_names = settings$level_names %||% character(),
        numeric_style = numeric_style,
        categorical_style = categorical_style,
        digits = input$quick_digits,
        variable_header = settings$variable_header %||% "Variable",
        value_header = settings$value_header %||% "Value",
        group_columns = group_cols
      ))
    }

    quick_univariate_summary(
      data,
      cols,
      numeric_stats = numeric_stats,
      categorical_stats = categorical_stats,
      table_format = input$quick_table_format,
      combined_value = isTRUE(input$quick_combined_value)
    )
  }

  analysis_data <- reactive({
    data <- data_store()
    if (!ncol(data)) return(data)
    cols <- selected_columns()
    if (isTRUE(input$drop_missing) && length(cols)) {
      keep_cols <- intersect(cols, names(data))
      if (!length(keep_cols)) return(data)
      keep <- stats::complete.cases(data[, keep_cols, drop = FALSE])
      return(data[keep, , drop = FALSE])
    }
    data
  })

  recommendations <- reactive({
    if (!ncol(data_store())) {
      return(data.frame(
        Outcome = "",
        Predictor = "",
        Method = "No active dataset",
        Why = "Use the built-in example dataset or apply an imported dataset before selecting analyses.",
        Feasible = FALSE,
        stringsAsFactors = FALSE
      ))
    }
    recommend_batch(data_store(), input$outcomes, input$predictors)
  })

  output$sample_disclaimer_sidebar <- renderUI({
    if (!isTRUE(input$use_example)) return(NULL)
    div(
      class = "alert alert-warning py-2",
      strong("Sample data notice: "),
      "The built-in dataset is simulated demonstration data only. It does not contain or represent real patient data, protected health information (PHI), or clinical records."
    )
  })

  output$sample_disclaimer_main <- renderUI({
    if (!isTRUE(input$use_example)) return(NULL)
    div(
      class = "alert alert-warning",
      strong("Built-in dataset disclaimer: "),
      "When the example dataset is enabled, all rows and values are synthetic and generated for testing the app workflow. They should not be interpreted as real patient observations or health information."
    )
  })

  output$profile <- renderTable({
    data <- data_store()
    if (!ncol(data)) return(data.frame(Message = "No active dataset. Use the example dataset or apply an imported dataset."))
    profile_data(data)
  })

  transform_preview_data <- reactive({
    data <- data_store()
    if (!ncol(data)) return(data.frame(Message = "No active dataset."))
    data_with_type_headers(head(data, 12))
  })

  output$preview <- renderTable({
    transform_preview_data()
  })

  output$type_date_preview <- renderTable({
    transform_preview_data()
  })

  output$derive_preview <- renderTable({
    transform_preview_data()
  })

  output$transform_message <- renderUI({
    msg <- transform_message()
    if (is.null(msg)) return(NULL)
    tags$div(class = "alert alert-info py-2 mt-2", msg)
  })

  output$date_format_guess <- renderUI({
    data <- data_store()
    column <- input$date_transform_column
    if (!is_nonempty(column) || !column %in% names(data)) {
      return(tags$p(class = "text-muted", "Select a column to estimate its date format."))
    }
    guess <- guess_date_format(data[[column]])
    example_values <- head(as.character(data[[column]][!is.na(data[[column]])]), 3)
    tags$div(
      class = "alert alert-secondary py-2",
      tags$div(strong("Predicted format: "), guess$label),
      tags$div(strong("Parsed in sample: "), paste0(guess$parsed, " of ", guess$total, " values (", guess$confidence, "%)")),
      if (length(example_values)) tags$div(strong("Examples: "), paste(example_values, collapse = ", "))
    )
  })

  output$derive_operation_controls <- renderUI({
    data <- data_store()
    choices <- if (ncol(data)) names(data) else c("No active dataset" = "")
    operation <- input$derive_operation %||% "copy"
    switch(
      operation,
      arithmetic = tags$div(
        selectInput("derive_arithmetic_operator", "Arithmetic operation", choices = c("+", "-", "*", "/", "log", "sqrt")),
        radioButtons("derive_operand_type", "Use second operand from", choices = c("Constant value" = "constant", "Another column" = "column"), inline = TRUE),
        conditionalPanel(
          "input.derive_operand_type == 'constant'",
          numericInput("derive_constant", "Constant value", value = 0)
        ),
        conditionalPanel(
          "input.derive_operand_type == 'column'",
          selectInput("derive_column_b", "Second column", choices = choices)
        )
      ),
      combine_text = tags$div(
        selectInput("derive_column_b", "Second text column", choices = choices),
        textInput("derive_separator", "Separator", value = " ")
      ),
      date_diff = tags$div(
        selectInput("derive_column_b", "End date column", choices = choices),
        tags$p(class = "text-muted", "The new value is end date minus first/source date, in days. Date formats are auto-detected.")
      ),
      date_part = tags$div(
        selectInput("derive_date_part", "Date part", choices = c("Year" = "year", "Month" = "month", "Day" = "day", "Weekday" = "weekday")),
        tags$p(class = "text-muted", "Date formats are auto-detected before extracting the selected part.")
      ),
      tags$p(class = "text-muted", "Copies the selected source column into a new column.")
    )
  })

  output$profile_transform <- renderTable({
    data <- data_store()
    if (!ncol(data)) return(data.frame(Message = "No active dataset."))
    profile_data(data)
  })

  output$recode_value_preview <- renderTable({
    data <- data_store()
    column <- input$recode_column
    if (!is_nonempty(column) || !column %in% names(data)) {
      return(data.frame(Message = "Choose a column to inspect values."))
    }
    counts <- sort(table(as.character(data[[column]]), useNA = "ifany"), decreasing = TRUE)
    data.frame(
      Value = names(counts),
      Count = as.integer(counts),
      stringsAsFactors = FALSE
    )
  })

  output$preview_profile <- renderTable({
    data <- data_store()
    if (!ncol(data)) return(data.frame(Message = "No active dataset."))
    data_with_type_headers(head(data, 12))
  })

  output$rename_columns_ui <- renderUI({
    data <- data_store()
    if (!ncol(data)) {
      return(tags$p(class = "text-muted", "No active dataset. Use the example dataset or apply an imported dataset first."))
    }
    tags$div(
      class = "shiny-table-output",
      tags$table(
        class = "table table-sm table-striped align-middle",
        tags$thead(
          tags$tr(
            tags$th("Current name"),
            tags$th("New name")
          )
        ),
        tags$tbody(lapply(seq_along(names(data)), function(i) {
          column <- names(data)[i]
          tags$tr(
            tags$td(tags$code(column)),
            tags$td(textInput(
              inputId = paste0("rename_col_", i),
              label = NULL,
              value = column,
              width = "100%"
            ))
          )
        }))
      )
    )
  })

  observeEvent(input$apply_column_renames, {
    data <- data_store()
    validate(need(ncol(data) > 0, "No active dataset to rename."))
    old_names <- names(data)
    new_names <- vapply(seq_along(old_names), function(i) {
      value <- input[[paste0("rename_col_", i)]]
      if (is.null(value)) old_names[i] else trimws(value)
    }, character(1))

    validate(
      need(all(nzchar(new_names)), "Column names cannot be blank."),
      need(!anyDuplicated(new_names), "Column names must be unique.")
    )

    names(data) <- new_names
    data_store(data)
    if (!isTRUE(input$use_example)) imported_data_store(data)
    pending_join(NULL)
    last_result(NULL)
    quick_result(NULL)
    rename_message("Column names updated.")
  })

  output$rename_columns_message <- renderUI({
    msg <- rename_message()
    if (is.null(msg)) return(NULL)
    tags$div(class = "alert alert-success py-2 mt-2", msg)
  })

  output$recommendations <- renderTable(recommendations())
  output$methods_table <- renderTable(method_catalog)

  output$import_status <- renderUI({
    if (is.null(input$data_file)) {
      return(tags$p(class = "text-muted", "Upload a primary dataset to preview parsing before applying it."))
    }
    preview <- tryCatch(import_candidate(), error = function(e) e)
    if (inherits(preview, "error")) {
      return(tags$div(class = "alert alert-danger", conditionMessage(preview)))
    }
    tags$div(
      class = "alert alert-info",
      paste0(
        "Preview parsed ", nrow(preview), " row(s) and ", ncol(preview),
        " column(s). Adjust skip/header/delimiter if the column names or first rows look wrong."
      )
    )
  })

  output$import_preview <- renderTable({
    req(input$data_file)
    data_with_type_headers(head(import_candidate(), 12))
  })

  output$active_dataset_summary <- renderTable({
    data <- data_store()
    if (!ncol(data)) {
      return(data.frame(
        Metric = c("Rows", "Columns", "Status"),
        Value = c(0, 0, "No active dataset. Use the example dataset or apply an import preview."),
        stringsAsFactors = FALSE
      ))
    }
    data.frame(
      Metric = c("Rows", "Columns", "Column names"),
      Value = c(nrow(data), ncol(data), paste(names(data), collapse = ", ")),
      stringsAsFactors = FALSE
    )
  })

  selected_analysis_data <- reactive({
    data <- analysis_data()
    validate(need(ncol(data) > 0, "No active dataset. Use the built-in example or apply an imported dataset first."))
    cols <- intersect(selected_columns(), names(data))
    validate(need(length(cols) > 0, "Select at least one outcome or predictor column."))
    data[, cols, drop = FALSE]
  })

  output$selected_analysis_data <- renderTable({
    data_with_type_headers(head(selected_analysis_data(), 25))
  })

  output$join_description <- renderText({
    join_type_description(input$join_type)
  })

  output$relationship_description <- renderText({
    relationship_description(input$join_relationship)
  })

  output$join_graphic <- renderUI({
    join_type <- input$join_type
    left_fill <- if (join_type %in% c("left", "full", "inner")) "#8ecae6" else "#f8f9fa"
    right_fill <- if (join_type %in% c("right", "full", "inner")) "#ffb703" else "#f8f9fa"
    overlap_fill <- if (join_type %in% c("left", "right", "full", "inner")) "#2a9d8f" else "#f8f9fa"
    tags$div(
      style = "display:flex;gap:16px;align-items:center;flex-wrap:wrap;",
      tags$div(style = paste0("width:120px;height:90px;border:2px solid #457b9d;background:", left_fill, ";display:flex;align-items:center;justify-content:center;"), "Primary"),
      tags$div(style = paste0("width:120px;height:90px;border:2px solid #2a9d8f;background:", overlap_fill, ";display:flex;align-items:center;justify-content:center;"), "Matched keys"),
      tags$div(style = paste0("width:120px;height:90px;border:2px solid #fb8500;background:", right_fill, ";display:flex;align-items:center;justify-content:center;"), "Lookup"),
      tags$div(
        style = "flex-basis:100%;font-size:0.9rem;color:#495057;",
        "Colored sections are retained by the selected join. White sections are excluded unless they have a matching key."
      )
    )
  })

  output$join_guide_text <- renderUI({
    tags$div(
      class = "alert alert-info",
      tags$p(strong("Join type: "), join_type_description(input$join_type)),
      tags$p(strong("Expected key relationship: "), relationship_description(input$join_relationship)),
      tags$p(
        "The example tables below intentionally change when you change the expected key relationship, so row expansion is visible for one-to-many and many-to-many joins."
      )
    )
  })

  output$join_example_primary <- renderTable({
    join_example_tables(input$join_type, input$join_relationship)$primary
  })

  output$join_example_lookup <- renderTable({
    join_example_tables(input$join_type, input$join_relationship)$lookup
  })

  output$join_example_result <- renderTable({
    join_example_tables(input$join_type, input$join_relationship)$result
  })

  output$join_warnings <- renderUI({
    planned <- pending_join()
    if (is.null(planned)) {
      return(tags$p(class = "text-muted", "Preview a join to see row-count estimates and warnings before applying it."))
    }
    if (!length(planned$warnings)) {
      return(tags$div(
        class = "alert alert-success",
        paste0(
          "No unexpected row-expansion warnings detected. Detected relationship: ",
          planned$detected_relationship,
          ". Review the estimated dimensions before applying."
        )
      ))
    }
    tags$div(
      class = if (identical(planned$expected_relationship, "many_to_many")) "alert alert-warning" else "alert alert-danger",
      strong("Review before applying. The join is still allowed:"),
      tags$ul(lapply(planned$warnings, tags$li))
    )
  })

  output$join_summary <- renderTable({
    planned <- pending_join()
    req(planned)
    planned$summary
  })

  output$join_preview <- renderTable({
    planned <- pending_join()
    req(planned)
    data_with_type_headers(head(planned$joined, 8))
  })

  output$quick_summary_options <- renderUI({
    if (isTRUE(input$quick_single_value)) {
      checkboxGroupInput(
        "quick_summary_stats",
        "Summary values",
        choices = c("N", "Missing", "Mean", "Median", "Mean (SD)", "Median [IQR]", "Percent", "Count (%)"),
        selected = c("N", "Mean", "Percent")
      )
    } else {
      checkboxGroupInput(
        "quick_summary_stats",
        "Summary values",
        choices = c("N", "Missing", "Unique", "Mean", "SD", "Median", "IQR", "Min", "Max", "Shapiro p-value", "Top level", "Top count", "Top percent"),
        selected = c("N", "Missing", "Mean", "SD", "Median", "IQR", "Top level", "Top percent")
      )
    }
  })

  observeEvent(input$open_quick_label_modal, {
    data <- analysis_data()
    validate(need(ncol(data) > 0, "No active dataset."))
    cols <- quick_summary_columns()
    cat_levels <- quick_categorical_levels()
    settings <- quick_label_settings()
    variable_names <- settings$variable_names %||% character()

    showModal(modalDialog(
      title = "Edit Univariate Summary Labels",
      size = "l",
      easyClose = TRUE,
      textInput("modal_quick_table_title", "Table title", value = settings$title %||% ""),
      layout_columns(
        textInput("modal_quick_variable_header", "Variable column header", value = settings$variable_header %||% "Variable"),
        textInput("modal_quick_value_header", "Value column header", value = settings$value_header %||% "Value"),
        col_widths = c(6, 6)
      ),
      tags$hr(),
      tags$h6("Variable display names"),
      tags$div(
        class = "shiny-table-output",
        tags$table(
          class = "table table-sm table-striped align-middle",
          tags$thead(tags$tr(tags$th("Current variable"), tags$th("Display name"))),
          tags$tbody(lapply(seq_along(cols), function(i) {
            column <- cols[i]
            tags$tr(
              tags$td(tags$code(column)),
              tags$td(textInput(
                paste0("modal_quick_var_", i),
                label = NULL,
                value = if (column %in% names(variable_names)) variable_names[[column]] else column,
                width = "100%"
              ))
            )
          }))
        )
      ),
      if (nrow(cat_levels)) {
        tagList(
          tags$hr(),
          checkboxInput("modal_edit_quick_levels", "Modify categorical level labels", value = FALSE),
          uiOutput("modal_quick_level_editors")
        )
      },
      footer = tagList(
        modalButton("Cancel"),
        actionButton("apply_quick_label_settings", "Apply labels", class = "btn-primary")
      )
    ))
  })

  output$modal_quick_level_editors <- renderUI({
    if (!isTRUE(input$modal_edit_quick_levels)) return(NULL)
    cat_levels <- quick_categorical_levels()
    settings <- quick_label_settings()
    level_names <- settings$level_names %||% character()
    if (!nrow(cat_levels)) return(NULL)

    tags$div(
      class = "shiny-table-output",
      tags$table(
        class = "table table-sm table-striped align-middle",
        tags$thead(tags$tr(tags$th("Variable"), tags$th("Current level"), tags$th("Display label"))),
        tags$tbody(lapply(seq_len(nrow(cat_levels)), function(i) {
          key <- cat_levels$Key[i]
          tags$tr(
            tags$td(tags$code(cat_levels$Column[i])),
            tags$td(cat_levels$Level[i]),
            tags$td(textInput(
              paste0("modal_quick_level_", i),
              label = NULL,
              value = if (key %in% names(level_names)) level_names[[key]] else cat_levels$Level[i],
              width = "100%"
            ))
          )
        }))
      )
    )
  })

  observeEvent(input$apply_quick_label_settings, {
    cols <- quick_summary_columns()
    variable_names <- vapply(seq_along(cols), function(i) {
      value <- input[[paste0("modal_quick_var_", i)]]
      if (is.null(value) || !nzchar(trimws(value))) cols[i] else trimws(value)
    }, character(1))
    names(variable_names) <- cols
    current_settings <- quick_label_settings()
    level_names <- current_settings$level_names %||% character()
    if (isTRUE(input$modal_edit_quick_levels)) {
      cat_levels <- quick_categorical_levels()
      level_names <- vapply(seq_len(nrow(cat_levels)), function(i) {
        value <- input[[paste0("modal_quick_level_", i)]]
        if (is.null(value) || !nzchar(trimws(value))) cat_levels$Level[i] else trimws(value)
      }, character(1))
      names(level_names) <- cat_levels$Key
    }
    quick_label_settings(list(
      title = input$modal_quick_table_title %||% "",
      variable_header = input$modal_quick_variable_header %||% "Variable",
      value_header = input$modal_quick_value_header %||% "Value",
      variable_names = variable_names,
      level_names = level_names
    ))
    if (!is.null(quick_result())) {
      quick_result(generate_quick_summary())
    }
    removeModal()
  })

  observeEvent(input$run_test, {
    req(input$outcomes)
    warnings <- method_feasibility_advice(data_store(), input$outcomes, input$predictors, input$methods)
    analysis_warnings(warnings)
    last_result(run_selected_analyses(analysis_data(), input$outcomes, input$predictors, input$methods))
    bslib::nav_select("analysis_tabs", selected = "Results", session = session)
  })

  observeEvent(input$analysis_auto_convert, {
    info <- input$analysis_auto_convert
    column <- info$column
    target <- info$type
    data <- data_store()
    req(column, target, column %in% names(data))
    data[[column]] <- switch(
      target,
      Factor = as.factor(data[[column]]),
      Numeric = suppressWarnings(as.numeric(data[[column]])),
      Character = as.character(data[[column]]),
      data[[column]]
    )
    data_store(data)
    if (!isTRUE(input$use_example)) imported_data_store(data)

    rerun_data <- data
    selected_columns <- intersect(unique(c(input$outcomes, input$predictors)), names(rerun_data))
    if (isTRUE(input$drop_missing) && length(selected_columns)) {
      complete_rows <- stats::complete.cases(rerun_data[, selected_columns, drop = FALSE])
      rerun_data <- rerun_data[complete_rows, , drop = FALSE]
    }

    analysis_warnings(method_feasibility_advice(data, input$outcomes, input$predictors, input$methods))
    last_result(run_selected_analyses(rerun_data, input$outcomes, input$predictors, input$methods))
    bslib::nav_select("analysis_tabs", selected = "Results", session = session)
  })

  observeEvent(input$run_quick_univariate, {
    quick_result(generate_quick_summary())
  })

  output$test_result <- renderText({
    req(last_result())
    last_result()
  })

  output$analysis_warning_ui <- renderUI({
    warnings <- analysis_warnings()
    if (is.null(warnings) || !nrow(warnings)) {
      return(tags$div(class = "alert alert-success", "No infeasible selected analyses were detected."))
    }
    current_data <- data_store()

    tags$div(
      class = "alert alert-warning",
      tags$p(strong("Some selected analyses are not feasible with the current selected columns.")),
      tags$p("This can happen when a dataset imports numeric measurements as character text, or when coded categories import as numeric values."),
      tags$div(lapply(seq_len(nrow(warnings)), function(i) {
        row <- warnings[i, ]
        fix_button <- NULL
        if (is_nonempty(row$FixColumn) && is_nonempty(row$FixType)) {
          payload <- jsonlite::toJSON(
            list(column = row$FixColumn, type = row$FixType, nonce = as.numeric(Sys.time()) + i),
            auto_unbox = TRUE
          )
          fix_button <- tags$button(
            type = "button",
            class = "btn btn-sm btn-primary mt-1",
            onclick = paste0("Shiny.setInputValue('analysis_auto_convert', ", payload, ", {priority: 'event'});"),
            paste("Convert", row$FixColumn, "to", row$FixType)
          )
        }
        fix_example <- if (is_nonempty(row$FixColumn) && is_nonempty(row$FixType)) {
          conversion_example_ui(current_data, row$FixColumn, row$FixType)
        } else {
          NULL
        }
        tags$div(
          class = "mb-3 p-2 border rounded bg-light",
          tags$p(strong(row$Method)),
          tags$p(row$Reason),
          tags$p(class = "text-muted", row$Suggestion),
          fix_button,
          fix_example
        )
      }))
    )
  })

  output$quick_univariate_table <- renderTable({
    if (is.null(quick_result())) {
      return(data.frame(Message = "Click 'Generate Univariate Summary' to generate this table."))
    }
    quick_result()
  })

  output$quick_table_title_display <- renderUI({
    settings <- quick_label_settings()
    if (identical(input$quick_table_format, "table1") && is_nonempty(settings$title)) {
      tags$span(settings$title)
    } else {
      "Univariate Summary"
    }
  })

  output$test_explanation <- renderUI({
    recs <- recommendations()
    feasible <- recs[recs$Feasible, , drop = FALSE]
    blocked <- recs[!recs$Feasible, , drop = FALSE]
    tags$div(
      tags$p("Selected outcomes: ", strong(paste(input$outcomes, collapse = ", "))),
      tags$p("Selected predictors: ", strong(ifelse(length(input$predictors), paste(input$predictors, collapse = ", "), "none"))),
      tags$p("Feasible recommended analyses: ", strong(nrow(feasible))),
      if (nrow(blocked)) tags$div(
        tags$h6("Blocked or not recommended"),
        tags$ul(lapply(seq_len(min(8, nrow(blocked))), function(i) {
          tags$li(paste(blocked$Method[i], "-", blocked$Why[i]))
        }))
      ),
      tags$p(
        class = "text-muted",
        "The app bars analyses that definitely do not fit the detected data shape, such as ANOVA without a numeric outcome or t-tests without exactly two groups."
      )
    )
  })

  current_figure_type <- reactive({
    effective_figure_type(analysis_data(), input$figure_type, input$figure_x, input$figure_y)
  })

  figure_has_group <- reactive({
    data <- analysis_data()
    length(input$figure_group) == 1 && nzchar(input$figure_group) && input$figure_group %in% names(data)
  })

  output$figure_y_control <- renderUI({
    graph_type <- current_figure_type()
    if (!identical(input$figure_type, "auto") && !graph_type %in% c("scatter", "box", "violin", "line")) return(NULL)
    data <- analysis_data()
    choices <- if (ncol(data)) names(data) else c("No active dataset" = "")
    selected <- if (length(input$figure_y) == 1 && input$figure_y %in% choices) input$figure_y else choices[[1]]
    selectInput("figure_y", "Y axis / value", choices = choices, selected = selected)
  })

  output$figure_y_label_control <- renderUI({
    graph_type <- current_figure_type()
    if (!graph_type %in% c("scatter", "box", "violin", "line", "histogram", "bar")) return(NULL)
    textInput("figure_y_label", "Y axis label", value = isolate(input$figure_y_label %||% ""))
  })

  output$figure_legend_controls <- renderUI({
    if (!figure_has_group()) return(NULL)
    tagList(
      textInput("figure_legend_title", "Legend title", value = isolate(input$figure_legend_title %||% "")),
      checkboxInput("figure_show_legend", "Show legend", value = isolate(if (is.null(input$figure_show_legend)) TRUE else isTRUE(input$figure_show_legend))),
      selectInput("figure_legend_position", "Legend position",
        choices = c("Right" = "right", "Bottom" = "bottom", "Top" = "top", "Left" = "left"),
        selected = isolate(if (is.null(input$figure_legend_position)) "right" else input$figure_legend_position)
      )
    )
  })

  output$figure_group_palette_control <- renderUI({
    if (!figure_has_group()) return(NULL)
    selectInput("figure_group_palette", "Grouped color palette", choices = c(
      "Dark categorical" = "Dark2",
      "Set 2" = "Set2",
      "Accent" = "Accent",
      "Paired" = "Paired",
      "Viridis" = "viridis"
    ), selected = isolate(if (is.null(input$figure_group_palette)) "Dark2" else input$figure_group_palette))
  })

  output$figure_point_size_control <- renderUI({
    if (!current_figure_type() %in% c("scatter", "line")) return(NULL)
    numericInput("figure_point_size", "Point size",
      value = if (is.null(input$figure_point_size)) 2.5 else input$figure_point_size,
      min = 0.5, max = 10, step = 0.5
    )
  })

  output$figure_bins_control <- renderUI({
    if (!identical(current_figure_type(), "histogram")) return(NULL)
    numericInput("figure_bins", "Histogram bins",
      value = if (is.null(input$figure_bins)) 30 else input$figure_bins,
      min = 5, max = 100, step = 1
    )
  })

  output$figure_smooth_control <- renderUI({
    if (!identical(current_figure_type(), "scatter")) return(NULL)
    tagList(
      checkboxInput("figure_smooth", "Add smooth trend", value = if (is.null(input$figure_smooth)) TRUE else isTRUE(input$figure_smooth)),
      conditionalPanel(
        condition = "input.figure_smooth === true",
        selectInput("figure_smooth_method", "Smooth trend method", choices = c(
          "LOESS" = "loess",
          "Linear model" = "lm",
          "Generalized additive model" = "gam",
          "Quadratic polynomial" = "poly2",
          "Cubic polynomial" = "poly3"
        ), selected = isolate(input$figure_smooth_method %||% "loess")),
        checkboxInput("figure_smooth_se", "Show confidence band", value = isolate(if (is.null(input$figure_smooth_se)) TRUE else isTRUE(input$figure_smooth_se)))
      )
    )
  })

  output$figure_flip_axes_control <- renderUI({
    if (!current_figure_type() %in% c("box", "violin", "bar")) return(NULL)
    checkboxInput("figure_flip_axes", "Flip axes", value = if (is.null(input$figure_flip_axes)) FALSE else isTRUE(input$figure_flip_axes))
  })

  plot_object <- reactive({
    data <- analysis_data()
    validate(need(ncol(data) > 0, "No active dataset. Use the built-in example or apply an imported dataset first."))
    x_col <- input$figure_x
    y_col <- input$figure_y
    group_col <- input$figure_group
    validate(need(length(x_col) == 1 && x_col %in% names(data), "Choose a valid X axis column."))

    graph_type <- current_figure_type()
    point_size <- input$figure_point_size %||% 2.5
    bins <- input$figure_bins %||% 30
    show_smooth <- if (is.null(input$figure_smooth)) TRUE else isTRUE(input$figure_smooth)
    smooth_method <- input$figure_smooth_method %||% "loess"
    smooth_se <- if (is.null(input$figure_smooth_se)) TRUE else isTRUE(input$figure_smooth_se)
    show_legend <- if (is.null(input$figure_show_legend)) TRUE else isTRUE(input$figure_show_legend)
    legend_position <- input$figure_legend_position %||% "right"
    group_palette <- input$figure_group_palette %||% "Dark2"
    flip_axes <- if (is.null(input$figure_flip_axes)) FALSE else isTRUE(input$figure_flip_axes)

    primary_color <- resolve_plot_color(input$figure_primary_color, input$figure_primary_color_name, "#2a9d8f")
    secondary_color <- resolve_plot_color(input$figure_secondary_color, input$figure_secondary_color_name, "#e76f51")
    x_label <- if (is_nonempty(input$figure_x_label)) input$figure_x_label else x_col
    y_label <- if (is_nonempty(input$figure_y_label)) input$figure_y_label else if (length(y_col) == 1 && y_col %in% names(data)) y_col else "Count"
    legend_title <- if (is_nonempty(input$figure_legend_title)) input$figure_legend_title else if (is_nonempty(group_col)) group_col else NULL
    title <- if (is_nonempty(input$figure_title)) input$figure_title else NULL

    has_group <- is_nonempty(group_col) && group_col %in% names(data)

    p <- switch(
      graph_type,
      scatter = {
        validate(need(length(y_col) == 1 && y_col %in% names(data), "Scatter plots require a Y axis column."))
        validate(need(is.numeric(data[[x_col]]) && is.numeric(data[[y_col]]), "Scatter plots require numeric X and Y columns."))
        base <- if (has_group) {
          ggplot(data, aes(x = .data[[x_col]], y = .data[[y_col]], color = as.factor(.data[[group_col]])))
        } else {
          ggplot(data, aes(x = .data[[x_col]], y = .data[[y_col]]))
        }
        smooth_layer <- switch(
          smooth_method,
          lm = geom_smooth(method = "lm", formula = y ~ x, se = smooth_se, color = secondary_color),
          gam = geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = smooth_se, color = secondary_color),
          poly2 = geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = smooth_se, color = secondary_color),
          poly3 = geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = smooth_se, color = secondary_color),
          geom_smooth(method = "loess", formula = y ~ x, se = smooth_se, color = secondary_color)
        )
        if (has_group) {
          base + geom_point(size = point_size, alpha = 0.75) + {if (show_smooth) smooth_layer}
        } else {
          base + geom_point(size = point_size, alpha = 0.75, color = primary_color) + {if (show_smooth) smooth_layer}
        }
      },
      box = {
        validate(need(length(y_col) == 1 && y_col %in% names(data), "Box plots require a Y axis column."))
        validate(need(is.numeric(data[[y_col]]), "Box plots require a numeric Y axis column."))
        if (has_group) {
          ggplot(data, aes(x = as.factor(.data[[x_col]]), y = .data[[y_col]], fill = as.factor(.data[[group_col]]))) +
            geom_boxplot(alpha = 0.8, outlier.color = secondary_color)
        } else {
          ggplot(data, aes(x = as.factor(.data[[x_col]]), y = .data[[y_col]])) +
            geom_boxplot(fill = primary_color, alpha = 0.8, outlier.color = secondary_color)
        }
      },
      histogram = {
        validate(need(is.numeric(data[[x_col]]), "Histograms require a numeric X axis column."))
        if (has_group) {
          ggplot(data, aes(x = .data[[x_col]], fill = as.factor(.data[[group_col]]))) +
            geom_histogram(bins = bins, color = "white", alpha = 0.75, position = "identity")
        } else {
          ggplot(data, aes(x = .data[[x_col]])) +
            geom_histogram(bins = bins, fill = primary_color, color = "white", alpha = 0.85)
        }
      },
      bar = {
        if (has_group) {
          ggplot(data, aes(x = .data[[x_col]], fill = as.factor(.data[[group_col]]))) +
            geom_bar(alpha = 0.9, position = "dodge")
        } else {
          ggplot(data, aes(x = .data[[x_col]])) +
            geom_bar(fill = primary_color, alpha = 0.9)
        }
      },
      violin = {
        validate(need(length(y_col) == 1 && y_col %in% names(data), "Violin plots require a Y axis column."))
        validate(need(is.numeric(data[[y_col]]), "Violin plots require a numeric Y axis column."))
        if (has_group) {
          ggplot(data, aes(x = as.factor(.data[[x_col]]), y = .data[[y_col]], fill = as.factor(.data[[group_col]]))) +
            geom_violin(alpha = 0.8, trim = FALSE, position = position_dodge(width = 0.9)) +
            geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.45, position = position_dodge(width = 0.9))
        } else {
          ggplot(data, aes(x = as.factor(.data[[x_col]]), y = .data[[y_col]])) +
            geom_violin(fill = primary_color, alpha = 0.8, trim = FALSE) +
            geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.45)
        }
      },
      line = {
        validate(need(length(y_col) == 1 && y_col %in% names(data), "Line charts require a Y axis column."))
        validate(need(is.numeric(data[[y_col]]), "Line charts require a numeric Y axis column."))
        if (has_group) {
          ggplot(data, aes(x = .data[[x_col]], y = .data[[y_col]], color = as.factor(.data[[group_col]]), group = as.factor(.data[[group_col]]))) +
            geom_line(linewidth = 0.9) +
            geom_point(size = point_size)
        } else {
          ggplot(data, aes(x = .data[[x_col]], y = .data[[y_col]], group = 1)) +
            geom_line(color = primary_color, linewidth = 0.9) +
            geom_point(color = secondary_color, size = point_size)
        }
      }
    )

    p <- p +
      labs(title = title, x = x_label, y = y_label, color = legend_title, fill = legend_title) +
      theme_minimal(base_size = 13) +
      theme(legend.position = if (show_legend) legend_position else "none")

    if (has_group) {
      if (identical(group_palette, "viridis")) {
        p <- p + scale_color_viridis_d(option = "D") + scale_fill_viridis_d(option = "D")
      } else {
        p <- p +
          scale_color_brewer(palette = group_palette) +
          scale_fill_brewer(palette = group_palette)
      }
    }
    if (flip_axes) p <- p + coord_flip()
    p
  })

  output$plot <- renderPlot(print(plot_object()))

  output$download_plot <- downloadHandler(
    filename = function() paste0("gcbs_plot_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 1200, height = 800, res = 150)
      print(plot_object())
      dev.off()
    }
  )

  output$download_csv <- downloadHandler(
    filename = function() paste0("gcbs_cleaned_data_", Sys.Date(), ".csv"),
    content = function(file) write.csv(analysis_data(), file, row.names = FALSE)
  )

  output$download_tsv <- downloadHandler(
    filename = function() paste0("gcbs_cleaned_data_", Sys.Date(), ".tsv"),
    content = function(file) write.table(analysis_data(), file, row.names = FALSE, sep = "\t", quote = TRUE)
  )

  output$download_quick_summary_csv <- downloadHandler(
    filename = function() paste0("gcbs_univariate_summary_", Sys.Date(), ".csv"),
    content = function(file) {
      result <- quick_result()
      if (is.null(result)) result <- data.frame(Message = "No quick summary has been generated.")
      write.csv(result, file, row.names = FALSE)
    }
  )

  output$download_quick_summary_tsv <- downloadHandler(
    filename = function() paste0("gcbs_univariate_summary_", Sys.Date(), ".tsv"),
    content = function(file) {
      result <- quick_result()
      if (is.null(result)) result <- data.frame(Message = "No quick summary has been generated.")
      write.table(result, file, row.names = FALSE, sep = "\t", quote = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)
