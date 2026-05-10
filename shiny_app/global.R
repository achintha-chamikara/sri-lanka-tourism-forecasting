# =============================================================================
#  global.R  — loaded once by Shiny before ui.R / server.R
#  Also referenced by app.R (single-file version)
#  Contains: package loading check, helper text, colour constants
# =============================================================================

# Packages that must be present
required_shiny <- c("shiny", "plotly", "DT", "dplyr")
missing_pkgs   <- required_shiny[!sapply(required_shiny, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs)) {
  stop(paste("Missing packages:", paste(missing_pkgs, collapse = ", "),
             "\nRun:  source('install_packages.R')"))
}

# Colour tokens — keep in sync with CSS in app.R
MODEL_COLORS <- c(
  ARIMAX  = "#1f77b4",
  BSTS    = "#d65f00",
  Prophet = "#2a7d2e",
  History = "#b91c1c"
)

AIRPORT_CAP  <- 350000L
LAST_RUN     <- "2026-05-07 14:51"
DATA_RANGE   <- "Jan 2014 – Dec 2025"
N_OBS        <- 144L
