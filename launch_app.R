`%||%` <- function(x, y) {
  if (is.null(x) || !length(x)) y else x
}

required_packages <- c("shiny", "bslib", "ggplot2")

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages)) {
  message("Installing missing R packages: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

args <- commandArgs(FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)] %||% "")
app_dir <- getwd()
if (nzchar(file_arg) && file.exists(file_arg)) {
  app_dir <- dirname(normalizePath(file_arg, mustWork = TRUE))
}

message("Starting GCBS. If your browser does not open automatically, copy the URL printed below into your browser.")
shiny::runApp(app_dir, host = "127.0.0.1", launch.browser = TRUE)
