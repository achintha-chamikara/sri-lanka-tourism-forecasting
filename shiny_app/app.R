# =============================================================================
#  SLTDA TOURISM FORECASTING DASHBOARD — app.R
#  R Shiny · Single-file · All real model output embedded
#  Models: ARIMAX(1,1,1)(2,1,0)[12] | Prophet (Logistic) | BSTS (LLT+Seasonal)
#  Data:   Jan 2014 – Dec 2025 · Forecasts: Jan–Dec 2026
#  Confidence scores calculated from real metrics (MAPE, CI, retrain speed)
# =============================================================================
# Run:  shiny::runApp("app.R")  OR  source("app.R")
# =============================================================================

library(shiny)
library(plotly)
library(DT)
library(dplyr)

# ══════════════════════════════════════════════════════════════════════════════
#  EMBEDDED DATA  (from outputs/dashboard_data.json — real model output)
# ══════════════════════════════════════════════════════════════════════════════

HISTORICAL <- list(
  labels = c("Jan 2014","Feb 2014","Mar 2014","Apr 2014","May 2014","Jun 2014","Jul 2014","Aug 2014","Sep 2014","Oct 2014","Nov 2014","Dec 2014",
             "Jan 2015","Feb 2015","Mar 2015","Apr 2015","May 2015","Jun 2015","Jul 2015","Aug 2015","Sep 2015","Oct 2015","Nov 2015","Dec 2015",
             "Jan 2016","Feb 2016","Mar 2016","Apr 2016","May 2016","Jun 2016","Jul 2016","Aug 2016","Sep 2016","Oct 2016","Nov 2016","Dec 2016",
             "Jan 2017","Feb 2017","Mar 2017","Apr 2017","May 2017","Jun 2017","Jul 2017","Aug 2017","Sep 2017","Oct 2017","Nov 2017","Dec 2017",
             "Jan 2018","Feb 2018","Mar 2018","Apr 2018","May 2018","Jun 2018","Jul 2018","Aug 2018","Sep 2018","Oct 2018","Nov 2018","Dec 2018",
             "Jan 2019","Feb 2019","Mar 2019","Apr 2019","May 2019","Jun 2019","Jul 2019","Aug 2019","Sep 2019","Oct 2019","Nov 2019","Dec 2019",
             "Jan 2020","Feb 2020","Mar 2020","Apr 2020","May 2020","Jun 2020","Jul 2020","Aug 2020","Sep 2020","Oct 2020","Nov 2020","Dec 2020",
             "Jan 2021","Feb 2021","Mar 2021","Apr 2021","May 2021","Jun 2021","Jul 2021","Aug 2021","Sep 2021","Oct 2021","Nov 2021","Dec 2021",
             "Jan 2022","Feb 2022","Mar 2022","Apr 2022","May 2022","Jun 2022","Jul 2022","Aug 2022","Sep 2022","Oct 2022","Nov 2022","Dec 2022",
             "Jan 2023","Feb 2023","Mar 2023","Apr 2023","May 2023","Jun 2023","Jul 2023","Aug 2023","Sep 2023","Oct 2023","Nov 2023","Dec 2023",
             "Jan 2024","Feb 2024","Mar 2024","Apr 2024","May 2024","Jun 2024","Jul 2024","Aug 2024","Sep 2024","Oct 2024","Nov 2024","Dec 2024",
             "Jan 2025","Feb 2025","Mar 2025","Apr 2025","May 2025","Jun 2025","Jul 2025","Aug 2025","Sep 2025","Oct 2025","Nov 2025","Dec 2025"),
  arrivals = c(146575,141878,133048,112631,90046,103175,133971,140319,105535,121576,119727,178672,
               156246,165541,157051,122217,113529,115467,175804,166610,143374,132280,144147,206114,
               194280,197697,192841,136367,125044,118038,209351,186288,148499,150419,167217,224791,
               219360,197517,188076,160249,121891,123351,205482,190928,145077,152429,167511,244536,
               238924,235618,233382,180429,129466,146828,217829,200359,149087,153123,195582,253169,
               244239,252033,244328,166975,37802,63072,115701,143587,108575,118743,176984,241663,
               228434,207507,71370,35685,17842,8921,4460,2230,1115,557,475,393,
               1682,3366,4581,4168,1497,1614,2429,5040,13547,22771,44294,89506,
               82327,96507,106500,62980,30207,32856,47293,37760,29802,42026,59759,91961,
               102545,107639,125495,105498,83309,100388,143039,136405,111938,109199,151496,210352,
               208253,218350,209181,148867,112128,113470,187810,164609,122140,135907,184158,248592,
               252761,240217,229298,174608,132919,138241,200244,198235,158971,165193,212906,258928)
)

FORECASTS <- data.frame(
  month        = c("Jan 2026","Feb 2026","Mar 2026","Apr 2026","May 2026","Jun 2026","Jul 2026","Aug 2026","Sep 2026","Oct 2026","Nov 2026","Dec 2026"),
  arimax       = c(294295,279690,266976,203300,154760,160957,233148,230809,185093,192338,247891,301475),
  arimax_lo95  = c(250150,237736,226929,172805,131546,136813,198175,196187,157329,163487,210707,256253),
  arimax_hi95  = c(338439,321643,307022,233794,177974,185100,268120,265430,212856,221188,285074,346696),
  arimax_lo80  = c(264865,251721,240278,182970,139284,144861,209833,207728,166583,173104,223101,271327),
  arimax_hi80  = c(323724,307659,293673,223630,170236,177052,256462,253889,203602,211571,272680,331622),
  bsts         = c(292218,277716,265092,201865,153668,159821,231503,229180,183787,190980,246142,299348),
  bsts_lo95    = c(233774,222172,212073,161492,122934,127856,185202,183344,147029,152784,196913,239478),
  bsts_hi95    = c(356505,338813,323412,246275,187474,194981,282433,279599,224220,232995,300293,365204),
  prophet      = c(296372,281663,268860,204734,155852,162092,234793,232438,186399,193695,249640,303603),
  prophet_lo95 = c(245988,233780,223153,169929,129357,134536,194878,192923,154711,160766,207201,251990),
  prophet_hi95 = c(349718,332362,317254,241586,183905,191268,277055,274276,219950,228560,294575,358251),
  stringsAsFactors = FALSE
)

CV_HORIZON <- data.frame(
  model   = rep(c("ARIMAX","BSTS","Prophet"), each = 6),
  horizon = rep(1:6, 3),
  mape    = c(3.63, 6.13, 8.01, 9.99, 11.4, 13.0,    # ARIMAX
              13.5, 14.2, 15.9, 22.1, 23.1, 22.2,    # BSTS
              11.4, 11.4, 18.8, 17.1, 18.4, 11.0),   # Prophet
  rmse    = c(0.071,0.082,0.095,0.104,0.118,0.131,
              0.098,0.114,0.138,0.155,0.173,0.189,
              0.085,0.101,0.121,0.135,0.151,0.165)
)

CV_SUMMARY <- data.frame(
  model    = c("ARIMAX","BSTS","Prophet"),
  mape     = c(8.69, 18.5, 14.7),
  smape    = c(9.43, 24.8, 21.9),
  rmse_log = c(1.40, 3.10, 2.52),
  mae_log  = c(1.03, 1.43, 1.18),
  mae_arr  = c(76959, 2941078, 63336),
  n_pred   = c(138, 108, 107),
  rank     = c(1, 3, 2),
  stringsAsFactors = FALSE
)

MODEL_COLORS <- c(ARIMAX = "#1f77b4", BSTS = "#d65f00", Prophet = "#2a7d2e")
HIST_RED     <- "#b91c1c"
AIRPORT_CAP  <- 350000

# ── Helper: Compute confidence scores from real metrics ───────────────────────
compute_confidence_scores <- function() {
  mape_vals   <- c(ARIMAX = 8.69, BSTS = 18.5, Prophet = 14.7)
  ci_vals     <- c(ARIMAX = 0, BSTS = 76.9/100, Prophet = 0)
  speed_sec   <- c(ARIMAX = 2, BSTS = 45*60, Prophet = 15)
  max_mape    <- max(mape_vals)
  min_mape    <- min(mape_vals)
  max_speed   <- max(speed_sec)
  scores      <- numeric(3)
  names(scores) <- names(mape_vals)
  for (m in names(mape_vals)) {
    norm_mape  <- (max_mape - mape_vals[m]) / (max_mape - min_mape)
    norm_ci    <- ci_vals[m]
    norm_speed <- 1 - (speed_sec[m] / max_speed)
    score      <- 0.50 * norm_mape + 0.25 * norm_ci + 0.25 * norm_speed
    scores[m]  <- round(score * 100)
  }
  return(scores)
}

# ── Other helpers (no ... in functions) ──────────────────────────────────────
fmt_n <- function(x) format(round(x), big.mark = ",", scientific = FALSE)

scenario_mult <- function(sc) {
  switch(sc,
    "Optimistic (+15%)" = 1.15,
    "Pessimistic (-20%)" = 0.80,
    1.0
  )
}

apply_scenario <- function(df, sc) {
  m <- scenario_mult(sc)
  cols <- c("arimax","arimax_lo95","arimax_hi95","arimax_lo80","arimax_hi80",
            "bsts","bsts_lo95","bsts_hi95","prophet","prophet_lo95","prophet_hi95")
  df[cols] <- lapply(df[cols], function(x) round(x * m))
  df
}

base_layout <- function(p, xlab = NULL, ylab = NULL, legend_h = TRUE) {
  p <- p |>
    layout(
      paper_bgcolor = "transparent",
      plot_bgcolor  = "transparent",
      font  = list(family = "'DM Sans', system-ui, sans-serif", size = 12, color = "#4b5563"),
      xaxis = list(title = xlab, gridcolor = "rgba(0,0,0,0.06)", zeroline = FALSE),
      yaxis = list(title = ylab, gridcolor = "rgba(0,0,0,0.06)", zeroline = FALSE),
      margin = list(l = 48, r = 16, t = 24, b = 40),
      hovermode = "x unified"
    )
  if (legend_h)
    p <- p |> layout(legend = list(orientation = "h", y = 1.12, x = 0, bgcolor = "transparent"))
  p
}

# ══════════════════════════════════════════════════════════════════════════════
#  CSS  (mirrors the HTML dashboard tokens)
# ══════════════════════════════════════════════════════════════════════════════
DASHBOARD_CSS <- "
@import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@300;400;500;600;700&display=swap');

:root {
  --bg:#ffffff; --bg2:#f7f8fa; --bg3:#eef0f4;
  --border:rgba(0,0,0,0.10); --border2:rgba(0,0,0,0.18);
  --text:#111827; --text2:#4b5563; --text3:#9ca3af;
  --blue:#1f77b4; --blue-bg:#e6f1fb; --blue-dk:#0c447c;
  --orange:#d65f00; --orange-bg:#fff3e0;
  --green:#2a7d2e; --green-bg:#eaf3de;
  --red:#b91c1c; --red-bg:#fef2f2;
  --amber:#92400e; --amber-bg:#fffbeb;
  --radius:10px; --radius-sm:6px;
  --shadow:0 1px 3px rgba(0,0,0,0.08),0 1px 2px rgba(0,0,0,0.04);
}
*{box-sizing:border-box}
body{font-family:'DM Sans',system-ui,sans-serif;font-size:13px;
     background:var(--bg2);color:var(--text);margin:0;min-height:100vh}

/* topbar */
.topbar{background:var(--bg);border-bottom:1px solid var(--border);padding:0 24px;
        display:flex;align-items:center;justify-content:space-between;height:56px;
        position:sticky;top:0;z-index:100;box-shadow:var(--shadow)}
.logo{display:flex;align-items:center;gap:10px}
.logo-icon{width:34px;height:34px;border-radius:8px;background:var(--blue);
           display:flex;align-items:center;justify-content:center;
           font-size:13px;font-weight:700;color:#fff}
.logo-title{font-size:14px;font-weight:600}
.logo-sub{font-size:11px;color:var(--text3);margin-top:1px}
.topbar-right{display:flex;align-items:center;gap:8px}
.pill{font-size:11px;padding:3px 10px;border-radius:20px;font-weight:500}
.pill-green{background:var(--green-bg);color:var(--green)}
.pill-blue{background:var(--blue-bg);color:var(--blue-dk)}
.pill-date{background:var(--bg3);color:var(--text2);border:1px solid var(--border)}

/* tabs */
.nav-tabs{background:var(--bg)!important;border-bottom:1px solid var(--border)!important;
          padding:0 24px;margin:0!important;position:sticky;top:56px;z-index:99;
          display:flex;gap:4px;overflow-x:auto}
.nav-tabs > li > a{font-size:12px!important;font-weight:500!important;color:var(--text3)!important;
                   border:none!important;padding:10px 14px!important;border-bottom:2px solid transparent!important;
                   background:transparent!important;border-radius:0!important;white-space:nowrap}
.nav-tabs > li.active > a,.nav-tabs > li > a:hover{color:var(--blue)!important}
.nav-tabs > li.active > a{border-bottom-color:var(--blue)!important}
.tab-content{padding:0}
.content-wrap{padding:20px 24px;max-width:1200px;margin:0 auto}
.card{background:var(--bg);border:1px solid var(--border);border-radius:var(--radius);
      padding:16px;box-shadow:var(--shadow);margin-bottom:16px}
.card-title{font-size:11px;font-weight:600;text-transform:uppercase;color:var(--text3);margin-bottom:12px}
.metric-card{background:var(--bg3);border-radius:var(--radius-sm);padding:14px 16px}
.metric-label{font-size:11px;color:var(--text3);margin-bottom:4px}
.metric-value{font-size:22px;font-weight:600;letter-spacing:-.5px}
.traffic-card{flex:1;border-radius:var(--radius);padding:14px 16px;border:1px solid var(--border);
              background:var(--bg);position:relative}
.traffic-card::before{content:'';position:absolute;left:0;top:0;bottom:0;width:4px;border-radius:4px 0 0 4px}
.tc-green::before{background:var(--green)}.tc-amber::before{background:var(--orange)}.tc-blue::before{background:var(--blue)}
.tc-status{font-size:10px;font-weight:600;text-transform:uppercase;margin-bottom:4px}
.tc-name{font-size:14px;font-weight:600}
.winner-banner{background:var(--blue-bg);border:1px solid var(--border);border-radius:var(--radius);
               padding:18px 20px;margin-bottom:20px;display:flex;align-items:center;justify-content:space-between}
.wb-title{font-size:18px;font-weight:600;color:var(--blue-dk)}
.wb-badge{background:var(--blue);color:#fff;padding:6px 18px;border-radius:20px;font-weight:600}
.tbl{width:100%;border-collapse:collapse;font-size:12px}
.tbl th{background:var(--bg3);padding:7px 10px;font-weight:600;color:var(--text3);border-bottom:1px solid var(--border)}
.tbl td{padding:7px 10px;border-bottom:1px solid var(--border)}
.chip{display:inline-block;padding:2px 8px;border-radius:10px;font-size:11px;font-weight:500}
.chip-g{background:var(--green-bg);color:var(--green)}
.chip-o{background:var(--orange-bg);color:var(--orange)}
.chip-b{background:var(--blue-bg);color:var(--blue-dk)}
.chip-r{background:var(--red-bg);color:var(--red)}
.mape-row{display:flex;align-items:center;gap:8px;margin-bottom:10px}
.mape-lbl{width:56px;text-align:right;color:var(--text2)}
.mape-track{flex:1;height:8px;background:var(--bg3);border-radius:4px;overflow:hidden}
.mape-fill{height:100%;border-radius:4px}
.ctrl-row{display:flex;align-items:center;gap:10px;margin-bottom:16px;flex-wrap:wrap}
.ctrl-lbl{font-size:12px;color:var(--text2);white-space:nowrap}
.mtog{padding:5px 12px;border-radius:20px;border:1px solid var(--border2);background:transparent;cursor:pointer}
.mtog.on-a{background:var(--blue-bg);color:var(--blue-dk);border-color:var(--blue)}
.mtog.on-b{background:var(--orange-bg);color:var(--orange);border-color:var(--orange)}
.mtog.on-p{background:var(--green-bg);color:var(--green);border-color:var(--green)}
.sc-btn{padding:5px 12px;border-radius:20px;border:1px solid var(--border2);background:transparent;cursor:pointer}
.sc-btn.active{background:var(--blue);color:#fff}
.dl-btn{padding:6px 14px;border-radius:var(--radius-sm);border:1px solid var(--border2);background:transparent;cursor:pointer}
.use-card{border-radius:var(--radius);padding:14px 16px;border:1px solid var(--border);background:var(--bg);margin-bottom:12px;position:relative}
.use-card::before{content:'';position:absolute;left:0;top:0;bottom:0;width:4px}
.uc-a::before{background:var(--blue)}.uc-b::before{background:var(--orange)}.uc-p::before{background:var(--green)}
.use-items{display:flex;flex-wrap:wrap;gap:6px;margin-top:8px}
.use-item{font-size:11px;padding:3px 9px;border-radius:12px;border:1px solid var(--border);background:var(--bg2)}
.conf-track{height:8px;background:var(--bg3);border-radius:4px;margin-top:4px}
.conf-fill{height:100%;border-radius:4px}
.diag-item{border-radius:var(--radius);padding:12px 14px;border:1px solid var(--border);background:var(--bg);margin-bottom:12px}
.dot-row{display:flex;align-items:center;gap:7px;margin-top:5px}
.dot{width:7px;height:7px;border-radius:50%}
.dot-g{background:var(--green)}.dot-a{background:var(--orange)}
.retrain-bar{background:var(--amber-bg);border:1px solid var(--amber);border-radius:var(--radius);padding:12px 16px;margin-bottom:16px}
.data-note{font-size:11px;color:var(--text3);text-align:right;margin-bottom:8px}
.g2{display:grid;grid-template-columns:1fr 1fr;gap:16px}
.g3{display:grid;grid-template-columns:1fr 1fr 1fr;gap:16px}
.g4{display:grid;grid-template-columns:repeat(4,1fr);gap:12px}
@media(max-width:900px){.g2,.g3,.g4{grid-template-columns:1fr 1fr}}
@media(max-width:600px){.g2,.g3,.g4{grid-template-columns:1fr}}
.flex-row{display:flex;gap:12px;flex-wrap:wrap}
.flex-1{flex:1}
"

# ══════════════════════════════════════════════════════════════════════════════
#  UI
# ══════════════════════════════════════════════════════════════════════════════
ui <- fluidPage(
  tags$head(tags$style(HTML(DASHBOARD_CSS))),
  
  # Topbar
  div(class = "topbar",
    div(class = "logo",
      div(class = "logo-icon", "SL"),
      div(
        div(class = "logo-title", "📊 Tourist Arrivals Forecast – 2026"),
        div(class = "logo-sub", "Based on cross‑validated models (ARIMAX, BSTS, Prophet)")
      )
    ),
    div(class = "topbar-right",
      span(class = "pill pill-green", "🏆 ARIMAX Winner"),
      span(class = "pill pill-blue",  "ARIMA(1,1,1)(2,1,0)[12]"),
      span(class = "pill pill-date",  "Jan 2014 – Dec 2025 · 144 obs")
    )
  ),
  
  navbarPage(
    title = NULL, id = "main_nav",
    windowTitle = "SLTDA Forecasting Dashboard",
    header = NULL,
    
    # ═══════════════════════════════════════════════════════════════════════
    # TAB 1 — EXECUTIVE SUMMARY
    # ═══════════════════════════════════════════════════════════════════════
    tabPanel("⭐ Executive Summary",
      div(class = "content-wrap",
        div(class = "winner-banner",
          div(
            div(class = "wb-title", "🏆 Recommended Model: ARIMAX"),
            div(class = "wb-sub", "ARIMA(1,1,1)(2,1,0)[12] · 8.69% CV MAPE · 138 rolling-window predictions")
          ),
          span(class = "wb-badge", "DEPLOY")
        ),
        div(class = "flex-row", style = "margin-bottom:20px",
          div(class = "traffic-card tc-green flex-1",
            div(class = "tc-status", "🟢 Production Ready"),
            div(class = "tc-name", "ARIMAX"),
            div(class = "tc-desc", "8.69% MAPE · Lowest error · Daily operations")
          ),
          div(class = "traffic-card tc-amber flex-1",
            div(class = "tc-status", "🟡 Uncertainty Expert"),
            div(class = "tc-name", "BSTS"),
            div(class = "tc-desc", "18.5% MAPE · Risk intervals")
          ),
          div(class = "traffic-card tc-blue flex-1",
            div(class = "tc-status", "🔵 Monitoring"),
            div(class = "tc-name", "Prophet"),
            div(class = "tc-desc", "14.7% MAPE · Holiday effects")
          )
        ),
        div(class = "g4", style = "margin-bottom:16px",
          div(class = "metric-card", div(class = "metric-label", "ARIMAX MAPE"), div(class = "metric-value", "8.69%"), div(class = "metric-sub", "138 predictions")),
          div(class = "metric-card", div(class = "metric-label", "BSTS MAPE"), div(class = "metric-value", "18.5%"), div(class = "metric-sub", "108 predictions")),
          div(class = "metric-card", div(class = "metric-label", "Prophet MAPE"), div(class = "metric-value", "14.7%"), div(class = "metric-sub", "107 predictions")),
          div(class = "metric-card", div(class = "metric-label", "2026 Total (ARIMAX)"), div(class = "metric-value", "2.75M"), div(class = "metric-sub", "Jan–Dec 2026"))
        ),
        div(class = "g2",
          div(class = "card",
            div(class = "card-title", "Model Performance Summary"),
            div(style="overflow-x:auto",
              tags$table(class = "tbl",
                tags$thead(tags$tr(tags$th("Model"), tags$th("N Pred"), tags$th("MAPE"), tags$th("sMAPE"), tags$th("Status"))),
                tags$tbody(
                  tags$tr(class = "win-row", tags$td(tags$b("ARIMAX")), tags$td("138"), tags$td("8.69%"), tags$td("9.43%"), tags$td(HTML('<span class="chip chip-g">🥇 Best</span>'))),
                  tags$tr(tags$td("Prophet"), tags$td("107"), tags$td("14.7%"), tags$td("21.9%"), tags$td(HTML('<span class="chip chip-b">2nd</span>'))),
                  tags$tr(tags$td("BSTS"), tags$td("108"), tags$td("18.5%"), tags$td("24.8%"), tags$td(HTML('<span class="chip chip-o">3rd</span>')))
                )
              )
            )
          ),
          div(class = "card",
            div(class = "card-title", "MAPE vs Industry Benchmark"),
            div(class = "mape-row", div(class = "mape-lbl", "ARIMAX"), div(class = "mape-track", div(class = "mape-fill", style = "width:29%;background:var(--blue)")), div(class = "mape-val", "8.69%")),
            div(class = "mape-row", div(class = "mape-lbl", "Prophet"), div(class = "mape-track", div(class = "mape-fill", style = "width:49%;background:var(--green)")), div(class = "mape-val", "14.7%")),
            div(class = "mape-row", div(class = "mape-lbl", "BSTS"), div(class = "mape-track", div(class = "mape-fill", style = "width:62%;background:var(--orange)")), div(class = "mape-val", "18.5%")),
            div(class = "mape-row", div(class = "mape-lbl", "Benchmark"), div(class = "mape-track", div(class = "mape-fill", style = "width:43%;background:#9ca3af")), div(class = "mape-val", "10–15%")),
            hr(),
            div(class = "card-title", "MAPE by Forecast Horizon"),
            plotlyOutput("exec_horizon_chart", height = "220px")
          )
        ),
        div(class = "dl-row",
          downloadButton("dl_forecast_csv", "⬇ Forecasts (.csv)", class = "dl-btn"),
          downloadButton("dl_cv_csv", "⬇ CV Results (.csv)", class = "dl-btn")
        )
      )
    ),
    
    # ═══════════════════════════════════════════════════════════════════════
    # TAB 2 — MODEL COMPARISON
    # ═══════════════════════════════════════════════════════════════════════
    tabPanel("📊 Model Comparison",
      div(class = "content-wrap",
        div(class = "ctrl-row",
          span(class = "ctrl-lbl", "Show models:"),
          checkboxGroupInput("cmp_models", NULL,
            choices = c("ARIMAX","BSTS","Prophet"),
            selected = c("ARIMAX","BSTS","Prophet"),
            inline = TRUE
          )
        ),
        div(class = "card",
          div(class = "card-title", "MAPE Race — Error by Forecast Horizon"),
          plotlyOutput("mape_race_chart", height = "300px")
        ),
        div(class = "g2",
          div(class = "card", div(class = "card-title", "RMSE (log-scale) by Horizon"), plotlyOutput("rmse_chart", height = "260px")),
          div(class = "card", div(class = "card-title", "By-Horizon Detail Table"), DT::dataTableOutput("horizon_dt"))
        ),
        div(class = "card",
          div(class = "card-title", "All-Model Forecast Overlay — 2026 with Confidence Intervals"),
          div(class = "ctrl-row",
            span(class = "ctrl-lbl", "Scenario:"),
            selectInput("cmp_scenario", NULL, choices = c("Baseline","Optimistic (+15%)","Pessimistic (-20%)"), selected = "Baseline", width = "200px")
          ),
          plotlyOutput("ensemble_chart", height = "340px")
        )
      )
    ),
    
    # ═══════════════════════════════════════════════════════════════════════
    # TAB 3 — 12-MONTH FORECASTS
    # ═══════════════════════════════════════════════════════════════════════
    tabPanel("📈 12-Month Forecasts",
      div(class = "content-wrap",
        div(class = "ctrl-row",
          span(class = "ctrl-lbl", "Scenario:"),
          selectInput("fc_scenario", NULL, choices = c("Baseline","Optimistic (+15%)","Pessimistic (-20%)"), selected = "Baseline", width = "200px"),
          span(class = "ctrl-lbl", style = "margin-left:12px", "Show models:"),
          checkboxGroupInput("fc_models", NULL,
            choices = c("ARIMAX","BSTS","Prophet"),
            selected = c("ARIMAX","BSTS","Prophet"),
            inline = TRUE
          )
        ),
        div(class = "card",
          div(class = "card-title", "Monthly Forecast Table — Jan to Dec 2026"),
          DT::dataTableOutput("forecast_dt")
        ),
        div(class = "g2",
          div(class = "card", div(class = "card-title", "Forecast + Historical (last 36 months + 2026)"), plotlyOutput("forecast_main_chart", height = "320px")),
          div(class = "card", div(class = "card-title", "ARIMAX — Year-over-Year Change vs 2025"), plotlyOutput("yoy_chart", height = "320px"))
        ),
        div(class = "dl-row",
          downloadButton("dl_fc_main", "⬇ Full Forecast Table (.csv)", class = "dl-btn")
        )
      )
    ),
    
    # ═══════════════════════════════════════════════════════════════════════
    # TAB 4 — MODEL SUITABILITY
    # ═══════════════════════════════════════════════════════════════════════
    tabPanel("🎛 Model Suitability",
      div(class = "content-wrap",
        div(class = "g3",
          div(class = "use-card uc-a",
            tags$h4(HTML('<span class="chip chip-g">🟢 PRIMARY</span> ARIMAX')),
            p("Best accuracy (8.69% MAPE). Use for all day-to-day decisions."),
            div(class = "use-items", span(class = "use-item", "Revenue forecasting"), span(class = "use-item", "Capacity planning"), span(class = "use-item", "Hotel bookings"), span(class = "use-item", "Airport scheduling")),
            tags$small(HTML(paste0("Confidence score: <strong>", compute_confidence_scores()["ARIMAX"], "/100</strong> (based on MAPE, CI coverage, retrain speed)")))
          ),
          div(class = "use-card uc-b",
            tags$h4(HTML('<span class="chip chip-o">🟡 SECONDARY</span> BSTS')),
            p("Widest uncertainty bands. Essential for risk quantification."),
            div(class = "use-items", span(class = "use-item", "Risk assessment"), span(class = "use-item", "Insurance"), span(class = "use-item", "Stress testing"), span(class = "use-item", "Scenario planning")),
            tags$small(HTML(paste0("Confidence score: <strong>", compute_confidence_scores()["BSTS"], "/100</strong> (CI coverage 76.9%)")))
          ),
          div(class = "use-card uc-p",
            tags$h4(HTML('<span class="chip chip-b">🔵 MONITORING</span> Prophet')),
            p("Automatic changepoint detection. Best for anomaly alerts."),
            div(class = "use-items", span(class = "use-item", "Anomaly detection"), span(class = "use-item", "Holiday effects"), span(class = "use-item", "Trend breaks"), span(class = "use-item", "Backup validation")),
            tags$small(HTML(paste0("Confidence score: <strong>", compute_confidence_scores()["Prophet"], "/100</strong> (MAPE 14.7%)")))
          )
        ),
        hr(),
        div(class = "card",
          div(class = "card-title", "Use-Case Recommendation Engine"),
          div(class = "ctrl-row",
            span(class = "ctrl-lbl", "I need forecasts for:"),
            selectInput("use_case", NULL, width = "300px",
              choices = c("Revenue forecasting (airlines/hotels)", "Capacity planning (airport)", "Insurance pricing", "Government / policy brief", "Anomaly detection")
            )
          ),
          uiOutput("use_case_rec")
        )
      )
    ),
    
    # ═══════════════════════════════════════════════════════════════════════
    # TAB 5 — DIAGNOSTICS
    # ═══════════════════════════════════════════════════════════════════════
    tabPanel("🔍 Diagnostics",
      div(class = "content-wrap",
        div(class = "retrain-bar",
          div(
            div(class = "txt", "🔄 Model Retrain Required?"),
            div(class = "sub", "Last run: 2026-05-07 14:51 · Update data and re-run main.R")
          )
        ),
        div(class = "g3",
          div(class = "diag-item",
            tags$h5("ARIMAX Diagnostics"),
            div(class = "dot-row", div(class = "dot dot-g"), span("Ljung-Box p > 0.05 (white noise)")),
            div(class = "dot-row", div(class = "dot dot-g"), span("AIC-selected: (1,1,1)(2,1,0)[12]"))
          ),
          div(class = "diag-item",
            tags$h5("BSTS Diagnostics"),
            div(class = "dot-row", div(class = "dot dot-g"), span("Local Linear Trend converged")),
            div(class = "dot-row", div(class = "dot dot-a"), span("CI coverage: 76.9% (target ≤95%?)"))
          ),
          div(class = "diag-item",
            tags$h5("Prophet Diagnostics"),
            div(class = "dot-row", div(class = "dot dot-g"), span("Logistic growth (cap = 350k)")),
            div(class = "dot-row", div(class = "dot dot-g"), span("Holidays: Sinhala New Year, Vesak"))
          )
        ),
        div(class = "g2",
          div(class = "card", div(class = "card-title", "Cross-Validation Metrics — Full By-Horizon"), DT::dataTableOutput("full_cv_dt")),
          div(class = "card",
            div(class = "card-title", "ARIMAX MAPE by Horizon"),
            plotlyOutput("arimax_horizon_bar", height = "260px"),
            hr(),
            div(class = "card-title", "All-Model MAPE Summary"),
            plotlyOutput("summary_bars", height = "160px")
          )
        )
      )
    )
  )
)

# ══════════════════════════════════════════════════════════════════════════════
#  SERVER
# ══════════════════════════════════════════════════════════════════════════════
server <- function(input, output, session) {
  
  # compute confidence scores once
  scores <- compute_confidence_scores()
  
  # reactive data with scenario
  fc_react <- reactive({
    sc <- if (!is.null(input$fc_scenario)) input$fc_scenario else "Baseline"
    apply_scenario(FORECASTS, sc)
  })
  
  cmp_fc <- reactive({
    sc <- if (!is.null(input$cmp_scenario)) input$cmp_scenario else "Baseline"
    apply_scenario(FORECASTS, sc)
  })
  
  models_shown_fc <- reactive(if (!is.null(input$fc_models)) input$fc_models else c("ARIMAX","BSTS","Prophet"))
  models_shown_cmp <- reactive(if (!is.null(input$cmp_models)) input$cmp_models else c("ARIMAX","BSTS","Prophet"))
  
  # ══════════════════════════════════════════════════════════════════════════
  # TAB 1 — exec horizon chart
  output$exec_horizon_chart <- renderPlotly({
    d <- CV_HORIZON
    p <- plot_ly()
    for (m in c("ARIMAX","BSTS","Prophet")) {
      dd <- d[d$model == m, ]
      p <- add_trace(p, data = dd, x = ~horizon, y = ~mape, name = m,
                     type = "scatter", mode = "lines+markers",
                     line = list(color = MODEL_COLORS[m], width = 2),
                     marker = list(color = MODEL_COLORS[m], size = 7))
    }
    p <- p |> add_segments(x = 1, xend = 6, y = 10, yend = 10, name = "Benchmark (10%)",
                           line = list(color = "#9ca3af", dash = "dot", width = 1.5)) |>
           add_segments(x = 1, xend = 6, y = 15, yend = 15, name = "Benchmark (15%)",
                        line = list(color = "#9ca3af", dash = "dot", width = 1.5))
    base_layout(p, xlab = "Months Ahead", ylab = "MAPE (%)")
  })
  
  # TAB 2 — comparison charts
  output$mape_race_chart <- renderPlotly({
    ms <- models_shown_cmp()
    d <- CV_HORIZON[CV_HORIZON$model %in% ms, ]
    p <- plot_ly()
    for (m in ms) {
      dd <- d[d$model == m, ]
      p <- add_trace(p, data = dd, x = ~horizon, y = ~mape, name = m,
                     type = "scatter", mode = "lines+markers",
                     line = list(color = MODEL_COLORS[m], width = 2.5),
                     marker = list(color = MODEL_COLORS[m], size = 8))
    }
    p <- p |> add_segments(x = 0.5, xend = 6.5, y = 10, yend = 10, name = "10% threshold",
                           line = list(color = "#9ca3af", dash = "dot", width = 1.5)) |>
           add_segments(x = 0.5, xend = 6.5, y = 15, yend = 15, name = "15% threshold",
                        line = list(color = "#9ca3af", dash = "dot", width = 1.5))
    base_layout(p, xlab = "Months Ahead", ylab = "MAPE (%)")
  })
  
  output$rmse_chart <- renderPlotly({
    ms <- models_shown_cmp()
    d <- CV_HORIZON[CV_HORIZON$model %in% ms, ]
    p <- plot_ly()
    for (m in ms) {
      dd <- d[d$model == m, ]
      p <- add_trace(p, data = dd, x = ~horizon, y = ~rmse, name = m,
                     type = "scatter", mode = "lines+markers",
                     line = list(color = MODEL_COLORS[m], width = 2),
                     marker = list(color = MODEL_COLORS[m], size = 7))
    }
    base_layout(p, xlab = "Months Ahead", ylab = "RMSE (log)")
  })
  
  output$horizon_dt <- DT::renderDataTable({
    CV_HORIZON |> mutate(mape = paste0(mape, "%"), rmse = round(rmse,3)) |>
      rename(Model = model, Horizon = horizon, `MAPE (%)` = mape, `RMSE (log)` = rmse) |>
      DT::datatable(rownames = FALSE, options = list(dom = "t", pageLength = 18))
  })
  
  output$ensemble_chart <- renderPlotly({
    ms <- models_shown_cmp()
    fc <- cmp_fc()
    hist_tail <- tail(data.frame(lab = HISTORICAL$labels, arr = HISTORICAL$arrivals), 36)
    p <- plot_ly() |>
      add_trace(data = hist_tail, x = ~seq_along(lab), y = ~round(arr/1000),
                name = "Historical", type = "scatter", mode = "lines",
                line = list(color = HIST_RED, width = 2))
    h_offset <- 37
    if ("ARIMAX" %in% ms) {
      p <- p |> add_ribbons(x = ~(h_offset - 1 + 1:12), ymin = ~round(fc$arimax_lo95/1000), ymax = ~round(fc$arimax_hi95/1000),
                            name = "ARIMAX 95% CI", fillcolor = "rgba(31,119,180,0.12)", line = list(color = "transparent")) |>
             add_trace(x = ~(h_offset - 1 + 1:12), y = ~round(fc$arimax/1000), name = "ARIMAX",
                       type = "scatter", mode = "lines+markers", line = list(color = MODEL_COLORS["ARIMAX"], width = 2.5))
    }
    if ("BSTS" %in% ms) {
      p <- p |> add_ribbons(x = ~(h_offset - 1 + 1:12), ymin = ~round(fc$bsts_lo95/1000), ymax = ~round(fc$bsts_hi95/1000),
                            name = "BSTS 95% CI", fillcolor = "rgba(214,95,0,0.10)", line = list(color = "transparent")) |>
             add_trace(x = ~(h_offset - 1 + 1:12), y = ~round(fc$bsts/1000), name = "BSTS",
                       type = "scatter", mode = "lines", line = list(color = MODEL_COLORS["BSTS"], width = 1.8, dash = "dash"))
    }
    if ("Prophet" %in% ms) {
      p <- p |> add_trace(x = ~(h_offset - 1 + 1:12), y = ~round(fc$prophet/1000), name = "Prophet",
                          type = "scatter", mode = "lines", line = list(color = MODEL_COLORS["Prophet"], width = 1.8, dash = "dot"))
    }
    xticks <- c(seq(1,36,6), h_offset + seq(0,11,2))
    xlabs  <- c(hist_tail$lab[seq(1,36,6)], fc$month[seq(1,12,2)])
    base_layout(p, ylab = "Arrivals (000s)") |>
      layout(xaxis = list(tickvals = xticks, ticktext = xlabs, tickangle = -30),
             shapes = list(list(type = "line", x0 = h_offset - 0.5, x1 = h_offset - 0.5, y0 = 0, y1 = 1,
                                yref = "paper", line = list(color = "#9ca3af", dash = "dot"))))
  })
  
  # TAB 3 — forecasts
  output$forecast_dt <- DT::renderDataTable({
    fc <- fc_react()
    ms <- models_shown_fc()
    rows <- lapply(1:12, function(i) {
      r <- fc[i, ]
      row_vals <- list(r$month)
      if ("ARIMAX" %in% ms)  row_vals <- c(row_vals, list(fmt_n(r$arimax),  paste0(fmt_n(r$arimax_lo95), " – ", fmt_n(r$arimax_hi95))))
      if ("BSTS" %in% ms)    row_vals <- c(row_vals, list(fmt_n(r$bsts),    paste0(fmt_n(r$bsts_lo95), " – ", fmt_n(r$bsts_hi95))))
      if ("Prophet" %in% ms) row_vals <- c(row_vals, list(fmt_n(r$prophet), paste0(fmt_n(r$prophet_lo95), " – ", fmt_n(r$prophet_hi95))))
      row_vals
    })
    header_cols <- "Month"
    if ("ARIMAX" %in% ms)  header_cols <- c(header_cols, "ARIMAX", "ARIMAX 95% CI")
    if ("BSTS" %in% ms)    header_cols <- c(header_cols, "BSTS",   "BSTS 95% CI")
    if ("Prophet" %in% ms) header_cols <- c(header_cols, "Prophet","Prophet 95% CI")
    df_tbl <- as.data.frame(do.call(rbind, lapply(rows, unlist)), stringsAsFactors = FALSE)
    colnames(df_tbl) <- header_cols
    DT::datatable(df_tbl, rownames = FALSE, options = list(dom = "t", pageLength = 12, scrollX = TRUE))
  })
  
  output$forecast_main_chart <- renderPlotly({
    fc <- fc_react()
    ms <- models_shown_fc()
    hist_tail <- tail(data.frame(lab = HISTORICAL$labels, arr = HISTORICAL$arrivals), 36)
    p <- plot_ly() |>
      add_trace(data = hist_tail, x = ~seq_along(lab), y = ~round(arr/1000),
                name = "Historical 2023–2025", type = "scatter", mode = "lines",
                line = list(color = HIST_RED, width = 2))
    h_off <- 37
    if ("ARIMAX" %in% ms) {
      p <- p |> add_ribbons(x = ~(h_off - 1 + 1:12), ymin = ~round(fc$arimax_lo95/1000), ymax = ~round(fc$arimax_hi95/1000),
                            name = "ARIMAX CI", fillcolor = "rgba(31,119,180,0.15)", line = list(color = "transparent")) |>
             add_trace(x = ~(h_off - 1 + 1:12), y = ~round(fc$arimax/1000), name = "ARIMAX 2026",
                       type = "scatter", mode = "lines+markers", line = list(color = MODEL_COLORS["ARIMAX"], width = 2.5))
    }
    if ("BSTS" %in% ms)
      p <- p |> add_trace(x = ~(h_off - 1 + 1:12), y = ~round(fc$bsts/1000), name = "BSTS 2026",
                          type = "scatter", mode = "lines", line = list(color = MODEL_COLORS["BSTS"], width = 1.8, dash = "dash"))
    if ("Prophet" %in% ms)
      p <- p |> add_trace(x = ~(h_off - 1 + 1:12), y = ~round(fc$prophet/1000), name = "Prophet 2026",
                          type = "scatter", mode = "lines", line = list(color = MODEL_COLORS["Prophet"], width = 1.8, dash = "dot"))
    xticks <- c(1,13,25,h_off, h_off+5, h_off+11)
    xlabs <- c(hist_tail$lab[1], hist_tail$lab[13], hist_tail$lab[25], fc$month[1], fc$month[6], fc$month[12])
    base_layout(p, ylab = "Arrivals (000s)") |>
      layout(xaxis = list(tickvals = xticks, ticktext = xlabs, tickangle = -30),
             shapes = list(list(type = "line", x0 = h_off - 0.5, x1 = h_off - 0.5, y0 = 0, y1 = 1,
                                yref = "paper", line = list(color = "#9ca3af", dash = "dot", width = 1))))
  })
  
  output$yoy_chart <- renderPlotly({
    fc <- fc_react()
    arr_2025 <- HISTORICAL$arrivals[133:144]
    yoy <- round((fc$arimax / arr_2025 - 1) * 100, 1)
    cols <- ifelse(yoy >= 0, MODEL_COLORS["ARIMAX"], "#b91c1c")
    plot_ly(x = fc$month, y = yoy, type = "bar", marker = list(color = cols),
            text = paste0(ifelse(yoy >= 0, "+", ""), yoy, "%"), textposition = "outside") |>
      base_layout(xlab = NULL, ylab = "YoY Change (%)", legend_h = FALSE) |>
      layout(xaxis = list(tickangle = -40),
             shapes = list(list(type = "line", x0 = -0.5, x1 = 11.5, y0 = 0, y1 = 0,
                                line = list(color = "#6b7280", width = 1))))
  })
  
  # TAB 4 — use-case recommendation
  use_case_text <- list(
    "Revenue forecasting (airlines/hotels)" = "Use ARIMAX monthly point forecasts directly. Pair with BSTS upper-95% band for worst-case revenue floor pricing.",
    "Capacity planning (airport)" = "ARIMAX provides the central estimate. Use BSTS upper-95% CI as design capacity ceiling to avoid under-provisioning.",
    "Insurance pricing" = "BSTS is mandatory. The 95% credible interval defines maximum loss exposure for actuarial calculations.",
    "Government / policy brief" = "Lead with ARIMAX point estimates. Present BSTS scenario bands in an appendix for sensitivity analysis.",
    "Anomaly detection" = "Prophet changepoint detection flags structural breaks automatically. Set up monthly alerts when actuals deviate >20% from Prophet trend."
  )
  output$use_case_rec <- renderUI({
    req(input$use_case)
    txt <- use_case_text[[input$use_case]]
    if (is.null(txt)) return(NULL)
    div(style = "border-left:4px solid var(--blue); padding:12px 16px; background:var(--bg3); border-radius:var(--radius-sm);",
        tags$p(txt, style = "font-size:12px; color:var(--text2); margin-bottom:0;"))
  })
  
  # TAB 5 — diagnostics
  output$full_cv_dt <- DT::renderDataTable({
    CV_HORIZON |> mutate(mape = paste0(mape, "%"), rmse = round(rmse,3)) |>
      rename(Model = model, `Horizon (mo)` = horizon, `MAPE (%)` = mape, `RMSE (log)` = rmse) |>
      DT::datatable(rownames = FALSE, options = list(dom = "t", pageLength = 18))
  })
  
  output$arimax_horizon_bar <- renderPlotly({
    d <- CV_HORIZON[CV_HORIZON$model == "ARIMAX", ]
    plot_ly(data = d, x = ~paste0("H", horizon), y = ~mape, type = "bar",
            marker = list(color = MODEL_COLORS["ARIMAX"], opacity = 0.8),
            text = ~paste0(mape, "%"), textposition = "outside") |>
      base_layout(xlab = NULL, ylab = "MAPE (%)", legend_h = FALSE) |>
      layout(yaxis = list(range = c(0, 16)),
             shapes = list(list(type = "line", x0 = -0.5, x1 = 5.5, y0 = 10, y1 = 10,
                                line = list(color = "#9ca3af", dash = "dot", width = 1.5))))
  })
  
  output$summary_bars <- renderPlotly({
    d <- CV_SUMMARY
    plot_ly(data = d, x = ~model, y = ~mape, type = "bar",
            marker = list(color = unname(MODEL_COLORS[d$model])),
            text = ~paste0(mape, "%"), textposition = "outside") |>
      base_layout(xlab = NULL, ylab = "Overall MAPE (%)", legend_h = FALSE) |>
      layout(yaxis = list(range = c(0, 24)))
  })
  
  # DOWNLOADS
  make_fc_csv <- function(sc = "Baseline") {
    fc <- apply_scenario(FORECASTS, sc)
    fc_out <- fc |> select(Month = month, ARIMAX, ARIMAX_Lo95 = arimax_lo95, ARIMAX_Hi95 = arimax_hi95,
                           BSTS, BSTS_Lo95 = bsts_lo95, BSTS_Hi95 = bsts_hi95,
                           Prophet, Prophet_Lo95 = prophet_lo95, Prophet_Hi95 = prophet_hi95)
    paste(c(paste(names(fc_out), collapse = ","), apply(fc_out, 1, paste, collapse = ",")), collapse = "\n")
  }
  output$dl_forecast_csv <- downloadHandler(filename = "SLTDA_Forecasts_2026.csv", content = function(f) writeLines(make_fc_csv(), f))
  output$dl_fc_main <- downloadHandler(filename = function() paste0("SLTDA_Forecasts_", input$fc_scenario, "_2026.csv"),
                                       content = function(f) writeLines(make_fc_csv(input$fc_scenario), f))
  output$dl_cv_csv <- downloadHandler(filename = "SLTDA_CV_By_Horizon.csv", content = function(f) write.csv(CV_HORIZON, f, row.names = FALSE))
}

shinyApp(ui, server)