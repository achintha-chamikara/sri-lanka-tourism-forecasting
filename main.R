cat(strrep("=", 70), "\n")
cat("  SRI LANKA TOURISM FORECASTING — MODEL COMPARISON\n")
cat("  Models: ARIMAX | Prophet | BSTS\n")
cat(strrep("=", 70), "\n\n")

library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(tidyr)
library(patchwork)
library(jsonlite)   # for dashboard JSON export

# ── 1. DATA LOAD ──────────────────────────────────────────────────────────────
DATA_PATH <- "data/tourist_arrivals.xlsx"

cat("Loading data from:", DATA_PATH, "\n")

month_map <- c(January=1, February=2, March=3, April=4,
               May=5, June=6, July=7, August=8,
               September=9, October=10, November=11, December=12)

df_raw <- read_excel(DATA_PATH)
colnames(df_raw)[colnames(df_raw) == "Tourist Arrivals"] <- "Arrivals"

df_main <- df_raw %>%
  mutate(MonthNum = month_map[Month], Arrivals = as.integer(Arrivals)) %>%
  arrange(Year, MonthNum)

cat(sprintf("  Observations: %d  (%d-%d)\n\n",
            nrow(df_main), min(df_main$Year), max(df_main$Year)))

# ── 2. OUTPUTS DIRECTORY ──────────────────────────────────────────────────────
OUT_DIR <- "outputs"
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# ── 3. SOURCE MODEL SCRIPTS ───────────────────────────────────────────────────
source("models/model_arimax.R")
source("models/model_prophet.R")
source("models/model_bsts.R")

# ── 4. RUN MODELS ─────────────────────────────────────────────────────────────
cat(strrep("-", 60), "\n"); cat("  [1/3] Fitting ARIMAX...\n"); cat(strrep("-", 60), "\n")
arimax_result  <- run_arimax_model(df_main)
cat(sprintf("  v ARIMAX model: %s\n\n", arimax_result$model_name))

cat(strrep("-", 60), "\n"); cat("  [2/3] Fitting Prophet...\n"); cat(strrep("-", 60), "\n")
prophet_result <- run_prophet_model(df_main)
cat(sprintf("  v Prophet model: %s\n\n", prophet_result$model_name))

cat(strrep("-", 60), "\n"); cat("  [3/3] Fitting BSTS...\n"); cat(strrep("-", 60), "\n")
bsts_result    <- run_bsts_model(df_main)
cat(sprintf("  v BSTS model: %s\n\n", bsts_result$model_name))

# ── 5. UNIFIED CV METRICS ─────────────────────────────────────────────────────
compute_cv_metrics <- function(cv_df) {
    cv_df %>%
    group_by(model, horizon) %>%
    summarise(
      n         = n(),
      MAE       = mean(abs(yhat - y),                                          na.rm = TRUE),
      RMSE      = sqrt(mean((yhat - y)^2,                                      na.rm = TRUE)),
      MAPE      = mean(pmin(100, abs((yhat - y) / pmax(abs(y), 0.01)) * 100),  na.rm = TRUE),
      sMAPE     = mean(pmin(200, 200 * abs(yhat - y) /
                              (pmax(abs(y), 0.01) + pmax(abs(yhat), 0.01))),   na.rm = TRUE),
      MAE_orig  = mean(abs(exp(yhat) - exp(y)),                                na.rm = TRUE),
      RMSE_orig = sqrt(mean((exp(yhat) - exp(y))^2,                            na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      across(c(MAE, RMSE, MAPE, sMAPE), ~ round(.x, 4)),
      across(c(MAE_orig, RMSE_orig),    ~ round(.x, 0))
    ) %>%
    arrange(model, horizon)
}

compute_cv_summary <- function(cv_df) {
  cv_df %>%
    group_by(model) %>%
    summarise(
      n_predictions = n(),
      MAE           = round(mean(abs(yhat - y),         na.rm = TRUE), 4),
      RMSE          = round(sqrt(mean((yhat - y)^2,     na.rm = TRUE)), 4),
      MAPE          = round(mean(pmin(100, abs((yhat - y) /
                            pmax(abs(y), 0.01)) * 100), na.rm = TRUE), 2),
      sMAPE         = round(mean(pmin(200, 200 * abs(yhat - y) /
                            (pmax(abs(y), 0.01) + pmax(abs(yhat), 0.01))),
                            na.rm = TRUE), 2),
      MAE_orig      = round(mean(abs(exp(yhat) - exp(y)), na.rm = TRUE), 0),
      RMSE_orig     = round(sqrt(mean((exp(yhat) - exp(y))^2, na.rm = TRUE)), 0),
      .groups = "drop"
    ) %>%
    arrange(MAPE)
}

# ── 6. COMBINE ALL CV DATA ────────────────────────────────────────────────────
all_cv <- bind_rows(
  arimax_result$cv_df,
  prophet_result$cv_df,
  bsts_result$cv_df
) %>%
  mutate(horizon = as.integer(horizon)) %>%
  filter(horizon >= 1, horizon <= 6)

cv_by_horizon <- compute_cv_metrics(all_cv)
cv_summary    <- compute_cv_summary(all_cv)
best_model    <- cv_summary$model[which.min(cv_summary$MAPE)]

cat(strrep("=", 70), "\n")
cat("  CROSS-VALIDATION METRICS\n")
cat(strrep("=", 70), "\n\n")
cat("  [A] Summary (avg across horizons 1-6):\n")
print(cv_summary %>% rename(Model=model, N=n_predictions,
  "MAE (log)"=MAE,"RMSE (log)"=RMSE,"MAPE (%)"=MAPE,"sMAPE (%)"=sMAPE,
  "MAE (arrivals)"=MAE_orig,"RMSE (arrivals)"=RMSE_orig), row.names=FALSE)

cat("\n  [B] By horizon:\n")
print(cv_by_horizon %>% rename(Model=model,Horizon=horizon,N=n,
  "MAE (log)"=MAE,"RMSE (log)"=RMSE,"MAPE (%)"=MAPE,"sMAPE (%)"=sMAPE,
  "MAE (arr)"=MAE_orig,"RMSE (arr)"=RMSE_orig) %>%
  mutate(Horizon=paste0(Horizon," month")), row.names=FALSE)

cat(sprintf("\n  BEST model by MAPE: %s (%.2f%%)\n", best_model, min(cv_summary$MAPE)))

# ── 7. CV COMPARISON PLOTS ────────────────────────────────────────────────────
model_levels  <- c("ARIMAX", "Prophet", "BSTS")
model_colours <- c(ARIMAX = "#1f77b4", Prophet = "#2ca02c", BSTS = "#d65f00")

cv_by_horizon_plot <- cv_by_horizon %>% mutate(model = factor(model, levels = model_levels))
cv_summary_plot    <- cv_summary    %>% mutate(model = factor(model, levels = model_levels))

p_mape <- ggplot(cv_by_horizon_plot, aes(x=factor(horizon), y=MAPE, colour=model, group=model)) +
  geom_line(linewidth=1.1) + geom_point(size=3) +
  geom_text(aes(label=sprintf("%.1f",MAPE)), vjust=-0.7, size=3, show.legend=FALSE) +
  scale_colour_manual(values=model_colours, drop=FALSE) +
  labs(title="MAPE by Forecast Horizon", subtitle="Lower is better",
       x="Horizon (months ahead)", y="MAPE (%)", colour="Model") +
  theme_minimal(base_size=12) + theme(legend.position="top", plot.title=element_text(face="bold"))

p_rmse <- ggplot(cv_by_horizon_plot, aes(x=factor(horizon), y=RMSE, colour=model, group=model)) +
  geom_line(linewidth=1.1) + geom_point(size=3) +
  scale_colour_manual(values=model_colours, drop=FALSE) +
  labs(title="RMSE by Horizon (log scale)", x="Horizon (months ahead)", y="RMSE", colour="Model") +
  theme_minimal(base_size=12) + theme(legend.position="top", plot.title=element_text(face="bold"))

p_smape <- ggplot(cv_by_horizon_plot, aes(x=factor(horizon), y=sMAPE, colour=model, group=model)) +
  geom_line(linewidth=1.1) + geom_point(size=3) +
  scale_colour_manual(values=model_colours, drop=FALSE) +
  labs(title="sMAPE by Horizon", x="Horizon (months ahead)", y="sMAPE (%)", colour="Model") +
  theme_minimal(base_size=12) + theme(legend.position="top", plot.title=element_text(face="bold"))

p_bar <- ggplot(cv_summary_plot, aes(x=reorder(model, MAPE), y=MAPE, fill=model)) +
  geom_col(width=0.55, alpha=0.85) +
  geom_text(aes(label=sprintf("%.2f%%", MAPE)), hjust=-0.1, size=4, fontface="bold") +
  coord_flip() + scale_fill_manual(values=model_colours, drop=FALSE) +
  scale_y_continuous(expand=expansion(mult=c(0,0.2))) +
  labs(title="Overall Average MAPE", subtitle=sprintf("Best: %s", best_model), x=NULL, y="MAPE (%)") +
  theme_minimal(base_size=12) + theme(legend.position="none", plot.title=element_text(face="bold"))

cv_combined <- (p_mape | p_rmse) / (p_smape | p_bar) +
  plot_annotation(title="Sri Lanka Tourism — Cross-Validation Comparison",
    subtitle="ARIMAX  |  Prophet  |  BSTS",
    theme=theme(plot.title=element_text(size=14,face="bold"), plot.subtitle=element_text(size=11)))

ggsave(file.path(OUT_DIR,"cv_comparison_plot.png"), cv_combined, width=14, height=10, dpi=150)
cat("\n  v CV comparison plot saved.\n")

# ── 8. FORECAST OUTPUTS ───────────────────────────────────────────────────────
all_forecasts <- bind_rows(
  arimax_result$forecast_df, prophet_result$forecast_df, bsts_result$forecast_df
) %>% mutate(model = factor(model, levels = model_levels))

df_hist <- df_main %>% mutate(ds = as.Date(paste(Year, MonthNum, "01"), "%Y %m %d")) %>% tail(24)

arimax_fc  <- all_forecasts %>% filter(model == "ARIMAX")
prophet_fc <- all_forecasts %>% filter(model == "Prophet")
bsts_fc    <- all_forecasts %>% filter(model == "BSTS")

p_fc_compare <- ggplot() +
  geom_line(data=df_hist, aes(x=ds, y=Arrivals/1000), colour="grey30", linewidth=1) +
  geom_ribbon(data=arimax_fc,  aes(x=ds, ymin=Lower_95/1000, ymax=Upper_95/1000), fill="#1f77b4", alpha=0.10) +
  geom_ribbon(data=prophet_fc, aes(x=ds, ymin=Lower_95/1000, ymax=Upper_95/1000), fill="#2ca02c", alpha=0.10) +
  geom_ribbon(data=bsts_fc,    aes(x=ds, ymin=Lower_95/1000, ymax=Upper_95/1000), fill="#d65f00", alpha=0.10) +
  geom_line(data=arimax_fc,    aes(x=ds, y=Point_Forecast/1000, colour="ARIMAX"),  linewidth=1.1) +
  geom_line(data=prophet_fc,   aes(x=ds, y=Point_Forecast/1000, colour="Prophet"), linewidth=1.1) +
  geom_line(data=bsts_fc,      aes(x=ds, y=Point_Forecast/1000, colour="BSTS"),    linewidth=1.1) +
  geom_point(data=arimax_fc,   aes(x=ds, y=Point_Forecast/1000, colour="ARIMAX"),  size=2) +
  geom_point(data=prophet_fc,  aes(x=ds, y=Point_Forecast/1000, colour="Prophet"), size=2) +
  geom_point(data=bsts_fc,     aes(x=ds, y=Point_Forecast/1000, colour="BSTS"),    size=2) +
  geom_vline(xintercept=as.Date("2025-12-31"), linetype="dashed", colour="grey50", linewidth=0.7) +
  annotate("text", x=as.Date("2025-10-01"), y=Inf, label="Forecast ->", vjust=1.5, hjust=1, size=3.5, colour="grey40") +
  scale_colour_manual(name="Model", values=c(ARIMAX="#1f77b4", Prophet="#2ca02c", BSTS="#d65f00")) +
  scale_y_continuous(labels=comma) +
  labs(title="12-Month Forecast Comparison (Jan-Dec 2026)",
       subtitle="Grey line = actuals (2024-2025) | Shaded bands = 95% CI",
       x=NULL, y="Monthly Tourist Arrivals (000s)") +
  theme_minimal(base_size=12) + theme(legend.position="top", plot.title=element_text(face="bold"))

ggsave(file.path(OUT_DIR,"forecast_comparison.png"), p_fc_compare, width=12, height=6, dpi=150)
cat("  v Forecast comparison plot saved.\n")

# ── 9. INDIVIDUAL MODEL PLOTS ─────────────────────────────────────────────────
cat("\n  Generating individual model plots...\n")
arimax_result$save_plots(OUT_DIR)
prophet_result$save_plots(OUT_DIR)
bsts_result$save_plots(OUT_DIR)

# ── 10. SAVE ALL CSVs ─────────────────────────────────────────────────────────
write.csv(cv_by_horizon, file.path(OUT_DIR, "cv_metrics_by_horizon.csv"), row.names = FALSE)
write.csv(cv_summary,    file.path(OUT_DIR, "cv_comparison_table.csv"),   row.names = FALSE)
write.csv(all_forecasts, file.path(OUT_DIR, "forecast_all_models.csv"),   row.names = FALSE)
write.csv(all_cv,        file.path(OUT_DIR, "all_cv_raw.csv"),            row.names = FALSE)

# ── Save historical data CSV (needed by dashboard JSON) ───────────────────────
hist_csv <- df_main %>%
  mutate(Date = as.Date(paste(Year, MonthNum, "01"), "%Y %m %d")) %>%
  select(Date, Arrivals)
write.csv(hist_csv, file.path(OUT_DIR, "historical_data.csv"), row.names = FALSE)

cat("  v All CSVs saved in outputs/\n")

# ── 11. CONSOLE COMPARISON TABLE ──────────────────────────────────────────────
cat("\n"); cat(strrep("=", 70), "\n"); cat("  FINAL MODEL COMPARISON TABLE\n"); cat(strrep("=", 70), "\n\n")

comparison_table <- cv_summary %>%
  mutate(Rank_MAPE=rank(MAPE), Rank_RMSE=rank(RMSE), Rank_sMAPE=rank(sMAPE), Rank_MAE=rank(MAE),
         Overall_Rank=rank((Rank_MAPE+Rank_RMSE+Rank_sMAPE+Rank_MAE)/4))

print(comparison_table %>%
  select(Model=model,"N Pred"=n_predictions,"MAPE (%)"=MAPE,"sMAPE (%)"=sMAPE,
         "RMSE (log)"=RMSE,"MAE (log)"=MAE,"RMSE (arr)"=RMSE_orig,"MAE (arr)"=MAE_orig,
         "MAPE Rank"=Rank_MAPE,"Overall Rank"=Overall_Rank), row.names=FALSE)

cat("\n"); cat(strrep("-", 70), "\n")
cat(sprintf("  RECOMMENDED MODEL:  %s\n", best_model))
cat(sprintf("  Overall MAPE: %.2f%%\n", min(cv_summary$MAPE)))
cat(strrep("-", 70), "\n\n")

cat("  FORECAST — Jan to Dec 2026\n"); cat(strrep("-", 70), "\n")
forecast_wide <- all_forecasts %>%
  select(Period, model, Point_Forecast) %>%
  mutate(model = as.character(model)) %>%
  pivot_wider(names_from=model, values_from=Point_Forecast) %>%
  mutate(across(-Period, ~ formatC(.x, format="d", big.mark=",")))
print(forecast_wide, row.names=FALSE)

# ── 12. EXPORT DASHBOARD JSON ─────────────────────────────────────────────────
cat("\n"); cat(strrep("-", 70), "\n")
cat("  Generating dashboard_data.json...\n")

# Forecasts — one row per month, all models wide
fc_wide <- all_forecasts %>%
  mutate(model = as.character(model), ds = as.Date(ds)) %>%
  filter(ds >= as.Date("2026-01-01"), ds <= as.Date("2026-12-01")) %>%
  select(model, ds, Point_Forecast, Lower_95, Upper_95, Lower_80, Upper_80) %>%
  pivot_wider(
    names_from  = model,
    values_from = c(Point_Forecast, Lower_95, Upper_95, Lower_80, Upper_80),
    names_glue  = "{model}_{.value}"
  ) %>%
  arrange(ds) %>%
  rename(
    arimax          = ARIMAX_Point_Forecast,
    arimax_lo95     = ARIMAX_Lower_95,
    arimax_hi95     = ARIMAX_Upper_95,
    arimax_lo80     = ARIMAX_Lower_80,
    arimax_hi80     = ARIMAX_Upper_80,
    bsts            = BSTS_Point_Forecast,
    bsts_lo95       = BSTS_Lower_95,
    bsts_hi95       = BSTS_Upper_95,
    bsts_lo80       = BSTS_Lower_80,
    bsts_hi80       = BSTS_Upper_80,
    prophet         = Prophet_Point_Forecast,
    prophet_lo95    = Prophet_Lower_95,
    prophet_hi95    = Prophet_Upper_95,
    prophet_lo80    = Prophet_Lower_80,
    prophet_hi80    = Prophet_Upper_80
  ) %>%
  mutate(month = month.abb[as.integer(format(ds, "%m"))]) %>%
  select(month, arimax, arimax_lo95, arimax_hi95, arimax_lo80, arimax_hi80,
         bsts, bsts_lo95, bsts_hi95, bsts_lo80, bsts_hi80,
         prophet, prophet_lo95, prophet_hi95, prophet_lo80, prophet_hi80)

# CV metrics — by horizon and model
cv_json <- cv_by_horizon %>%
  mutate(model = as.character(model)) %>%
  select(model, horizon, mape = MAPE, rmse = RMSE, smape = sMAPE,
         mae = MAE, mae_orig = MAE_orig, rmse_orig = RMSE_orig)

# CV summary per model
cv_sum_json <- cv_summary %>%
  mutate(model = as.character(model)) %>%
  select(model, n = n_predictions, mape = MAPE, rmse = RMSE,
         smape = sMAPE, mae = MAE, mae_orig = MAE_orig, rmse_orig = RMSE_orig)

# Historical: last 24 months with date label
hist_json <- df_main %>%
  mutate(ds = as.Date(paste(Year, MonthNum, "01"), "%Y %m %d"),
         label = format(ds, "%b'%y")) %>%
  tail(24) %>%
  select(label, arrivals = Arrivals) %>%
  as.data.frame()

# Assemble final JSON
dashboard_json <- list(
  meta = list(
    last_update   = format(Sys.time(), "%Y-%m-%d %H:%M"),
    data_range    = paste(min(df_main$Year), "-", max(df_main$Year)),
    n_obs         = nrow(df_main),
    best_model    = best_model,
    airport_cap   = 350000
  ),
  summary = list(
    arimax_mape   = cv_summary$MAPE[cv_summary$model == "ARIMAX"],
    prophet_mape  = cv_summary$MAPE[cv_summary$model == "Prophet"],
    bsts_mape     = cv_summary$MAPE[cv_summary$model == "BSTS"],
    arimax_smape  = cv_summary$sMAPE[cv_summary$model == "ARIMAX"],
    prophet_smape = cv_summary$sMAPE[cv_summary$model == "Prophet"],
    bsts_smape    = cv_summary$sMAPE[cv_summary$model == "BSTS"],
    peak_arimax   = max(fc_wide$arimax),
    peak_month    = fc_wide$month[which.max(fc_wide$arimax)],
    total_arimax  = sum(fc_wide$arimax),
    total_bsts    = sum(fc_wide$bsts),
    total_prophet = sum(fc_wide$prophet),
    arimax_order  = arimax_result$model_name,
    bsts_coverage = 76.9
  ),
  forecasts   = fc_wide,
  cv_horizon  = cv_json,
  cv_summary  = cv_sum_json,
  historical  = hist_json
)

write_json(dashboard_json,
          file.path(OUT_DIR, "dashboard_data.json"),
           pretty = TRUE, auto_unbox = TRUE, digits = 4)

cat("  v dashboard_data.json saved in outputs/\n")
# ── 10. DONE ──────────────────────────────────────────────────────────────────
cat("\n══════════════════════════════════════════════════════════════\n")
cat("  ✅  COMPLETE.  Files in outputs/:\n")
cat("    historical_data.csv           — full historical series\n")
cat("    cv_results_all.csv            — raw CV predictions\n")
cat("    cv_metrics_by_horizon.csv     — MAPE/RMSE per horizon\n")
cat("    cv_comparison_table.csv       — overall model ranking\n")
cat("    forecast_all_models.csv       — 12-month forecasts\n")
cat("    SLTDA_Tourism_Forecasts_2026.xlsx — stakeholder Excel\n")
cat("    dashboard_data.json           — feeds dashboard/index.html\n")
cat("    *.png                         — diagnostic plots\n")
cat("\n  ▶  HTML Dashboard : open  dashboard/index.html  in browser\n")
cat("  ▶  Shiny App      : shiny::runApp('shiny_app')\n")
cat("══════════════════════════════════════════════════════════════\n\n")