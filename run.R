# =============================================================================
#  run.R — ONE-CLICK LAUNCHER
#  Source this file in RStudio to get started.
#
#  Step 1: Train models    → source("main.R")
#  Step 2: Open HTML dash  → browseURL("dashboard/index.html")
#  Step 3: Launch Shiny    → shiny::runApp("shiny_app")
# =============================================================================

cat("\n┌─────────────────────────────────────────────────────────────┐\n")
cat("│    TOURISM FORECASTING DASHBOARD IN SRI LANKA — Quick Launcher      │\n")
cat("└─────────────────────────────────────────────────────────────┘\n\n")

cat("Choose an option:\n\n")
cat("  1. Train all models  (runs main.R — first time only, BSTS ~45 min)\n")
cat("  2. Open HTML dashboard  (no R server needed — just a browser)\n")
cat("  3. Launch Shiny dashboard  (instant — data already embedded)\n")
cat("  4. All of the above\n\n")

# ── Uncomment the option you want, or run individually: ──────────────────────

# Option 1 — Train models (required before HTML dashboard shows real data)
# source("main.R")

# Option 2 — Open the HTML dashboard in your default browser
# browseURL(file.path(getwd(), "dashboard/index.html"))

# Option 3 — Launch Shiny app (all real data already embedded, no main.R needed)
# shiny::runApp("shiny_app")

# Option 4 — Full pipeline
# source("main.R")
# browseURL(file.path(getwd(), "dashboard/index.html"))
# shiny::runApp("shiny_app")

cat("──────────────────────────────────────────────────────────────\n")
cat("To use: uncomment the desired option above and source this file,\n")
cat("or simply run each command manually in the RStudio console.\n\n")
cat("Quick commands:\n")
cat("  source('main.R')                    # train all models\n")
cat("  shiny::runApp('shiny_app')          # launch Shiny dashboard\n")
cat("  browseURL('dashboard/index.html')   # open HTML dashboard\n\n")
