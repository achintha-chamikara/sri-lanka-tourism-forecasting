# =============================================================================
#  install_packages.R — Run ONCE to install all dependencies
#  source("install_packages.R")
# =============================================================================

cat("Installing SLTDA project dependencies...\n\n")

pkgs <- c(
  # Core data
  "readxl",     # read Excel data file
  "writexl",    # write Excel output
  "dplyr",      # data manipulation
  "tidyr",      # reshaping
  "lubridate",  # date handling
  "jsonlite",   # JSON for dashboard

  # Visualisation
  "ggplot2",    # static plots (diagnostic PNGs)
  "scales",     # number formatting

  # Forecasting — ARIMAX
  "forecast",   # Arima(), auto.arima()
  "tseries",    # adf.test(), Box.test()

  # Forecasting — Prophet
  "prophet",    # Meta's Prophet (installs Rcpp + Stan)

  # Forecasting — BSTS
  "bsts",       # Bayesian Structural Time Series

  # Shiny dashboard
  "shiny",      # Shiny framework
  "plotly",     # interactive charts
  "DT"          # interactive tables
)

already  <- pkgs[sapply(pkgs, requireNamespace, quietly = TRUE)]
to_install <- setdiff(pkgs, already)

if (length(already)) {
  cat("Already installed:", paste(already, collapse = ", "), "\n\n")
}

if (length(to_install) == 0) {
  cat("✅  All packages already installed. You're ready to go!\n\n")
} else {
  cat("Installing:", paste(to_install, collapse = ", "), "\n\n")
  install.packages(to_install, repos = "https://cloud.r-project.org", dependencies = TRUE)
  cat("\n✅  Installation complete.\n\n")
}

cat("──────────────────────────────────────────────────────────────\n")
cat("Next step:  source('main.R')  to train models\n")
cat("            shiny::runApp('shiny_app')  for instant dashboard\n\n")
