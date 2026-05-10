# =============================================================================
#  MODEL: Prophet — Sri Lanka Monthly Tourist Arrivals
#  File  : models/model_prophet.R
#  Fix   : timezone warning in filter(), colour scale mismatch
# =============================================================================

run_prophet_model <- function(df_input) {

  library(prophet)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)

  # ── 1. PREPARE DATA ─────────────────────────────────────────
  df <- df_input %>%
    mutate(
      ds = as.Date(paste(Year, MonthNum, "01"), "%Y %m %d"),
      y  = ifelse(Arrivals == 0, 0.5, as.numeric(Arrivals)),
      y  = log(y)
    ) %>%
    select(ds, y) %>%
    mutate(
      easter_shock   = as.integer(ds >= as.Date("2019-04-01") & ds <= as.Date("2019-06-30")),
      covid_lockdown = as.integer(ds >= as.Date("2020-03-01") & ds <= as.Date("2020-12-31")),
      covid_recovery = as.integer(ds >= as.Date("2021-01-01") & ds <= as.Date("2021-12-31")),
      cap   = log(350000),
      floor = log(0.5)
    )

  # ── 2. HOLIDAYS ─────────────────────────────────────────────
  sinhala_new_year <- data.frame(
    holiday = "sinhala_tamil_new_year",
    ds = as.Date(paste0(2014:2026, "-04-14")),
    lower_window = 0L, upper_window = 1L
  )
  vesak_poson <- data.frame(
    holiday = rep(c("vesak", "poson"), each = length(2014:2026)),
    ds = c(as.Date(paste0(2014:2026, "-05-23")),
           as.Date(paste0(2014:2026, "-06-10"))),
    lower_window = 0L, upper_window = 1L
  )
  holidays_full <- bind_rows(sinhala_new_year, vesak_poson)

  # ── 3. BUILD & FIT MODEL ─────────────────────────────────────
  m <- prophet(
    growth = "logistic",
    holidays = holidays_full,
    seasonality.mode = "additive",
    changepoint.prior.scale = 0.3,
    holidays.prior.scale = 20,
    yearly.seasonality = FALSE,
    weekly.seasonality = FALSE,
    daily.seasonality = FALSE
  )
  m <- add_seasonality(m, name = "yearly", period = 365.25, fourier.order = 6)
  m <- add_regressor(m, "easter_shock",   mode = "additive")
  m <- add_regressor(m, "covid_lockdown", mode = "additive")
  m <- add_regressor(m, "covid_recovery", mode = "additive")
  m <- fit.prophet(m, df)

  # ── 4. CROSS-VALIDATION (Prophet native) ─────────────────────
  df_cv_raw <- cross_validation(
    m,
    horizon = as.difftime(180, units = "days"),
    initial = as.difftime(1095, units = "days"),
    period  = as.difftime(180, units = "days")
  )

  # Standardise to same format as ARIMAX/BSTS:
  # Convert POSIXct ds/cutoff → Date to avoid timezone warnings
  cv_df <- df_cv_raw %>%
    mutate(
      ds_date     = as.Date(ds),
      cutoff_date = as.Date(cutoff),
      horizon_days = as.numeric(ds_date - cutoff_date),
      horizon      = as.integer(ceiling(horizon_days / 30.44))
    ) %>%
    filter(horizon >= 1, horizon <= 6) %>%
    group_by(cutoff_date, horizon) %>%
    slice_max(order_by = ds_date, n = 1) %>%
    ungroup() %>%
    transmute(
      model  = "Prophet",
      horizon = horizon,
      cutoff  = as.character(cutoff_date),
      y       = y,
      yhat    = yhat
    )

  # ── 5. 12-MONTH FORECAST (2026) ──────────────────────────────
  future <- make_future_dataframe(m, periods = 12, freq = "month") %>%
    mutate(ds_date = as.Date(ds)) %>%
    left_join(
      df %>% mutate(ds_date = as.Date(ds)) %>%
        select(ds_date, easter_shock, covid_lockdown, covid_recovery),
      by = "ds_date"
    ) %>%
    mutate(
      easter_shock   = replace_na(easter_shock,   0L),
      covid_lockdown = replace_na(covid_lockdown, 0L),
      covid_recovery = replace_na(covid_recovery, 0L),
      cap   = log(350000),
      floor = log(0.5)
    ) %>%
    select(-ds_date)

  fc_raw <- predict(m, future)

  # Fix: convert ds to Date before filter to avoid timezone warning
  forecast_df <- fc_raw %>%
    mutate(ds_date = as.Date(ds)) %>%
    filter(ds_date >= as.Date("2026-01-01")) %>%
    slice_head(n = 12) %>%
    transmute(
      model          = "Prophet",
      Period         = format(ds_date, "%Y-%m"),
      ds             = ds_date,
      Point_Forecast = round(exp(yhat)),
      Lower_95       = round(exp(yhat_lower)),
      Upper_95       = round(exp(yhat_upper)),
      Lower_80       = round(exp(yhat_lower)),
      Upper_80       = round(exp(yhat_upper))
    )

  # ── 6. DIAGNOSTIC PLOTS ──────────────────────────────────────
  save_prophet_plots <- function(m_obj, fc_raw_obj, fc_df, df_orig, out_dir) {
    library(ggplot2)
    library(scales)

    # Convert all ds to plain Date to avoid timezone issues
    df_dates <- as.Date(df_orig$ds)

    fitted_df <- fc_raw_obj %>%
      mutate(ds_date = as.Date(ds)) %>%
      filter(ds_date %in% df_dates) %>%
      select(ds_date, yhat, yhat_lower, yhat_upper) %>%
      left_join(
        data.frame(ds_date = df_dates, y = df_orig$y),
        by = "ds_date"
      ) %>%
      mutate(across(c(yhat, yhat_lower, yhat_upper, y), exp))

    p1 <- ggplot(fitted_df, aes(x = ds_date)) +
      geom_ribbon(aes(ymin = yhat_lower / 1000, ymax = yhat_upper / 1000),
                  fill = "#a6cee3", alpha = 0.4) +
      geom_line(aes(y = y    / 1000, colour = "Actual"), linewidth = 0.8) +
      geom_line(aes(y = yhat / 1000, colour = "Fitted"),
                linewidth = 0.8, linetype = "dashed") +
      scale_colour_manual(values = c(Actual = "#2166ac", Fitted = "#d6604d")) +
      scale_y_continuous(labels = comma) +
      labs(title = "Prophet — Actual vs Fitted",
           x = NULL, y = "Arrivals (000s)", colour = NULL) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "top", plot.title = element_text(face = "bold"))

    hist_tail <- tail(fitted_df, 24)

    p2 <- ggplot() +
      geom_line(data = hist_tail,
                aes(x = ds_date, y = y / 1000),
                colour = "#2166ac", linewidth = 0.9) +
      geom_ribbon(data = fc_df,
                  aes(x = ds, ymin = Lower_95 / 1000, ymax = Upper_95 / 1000),
                  fill = "#1f78b4", alpha = 0.15) +
      geom_line(data = fc_df,
                aes(x = ds, y = Point_Forecast / 1000),
                colour = "#1f78b4", linewidth = 1) +
      scale_y_continuous(labels = comma) +
      labs(title = "Prophet — 12-Month Forecast (2026)",
           x = NULL, y = "Arrivals (000s)") +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(face = "bold"))

    ggsave(file.path(out_dir, "prophet_actual_vs_fitted.png"), p1,
           width = 9, height = 4, dpi = 150)
    ggsave(file.path(out_dir, "prophet_forecast_2026.png"),    p2,
           width = 9, height = 4, dpi = 150)

    # Component plot — wrapped in tryCatch to suppress aes_string deprecation crash
    png(file.path(out_dir, "prophet_components.png"),
        width = 1200, height = 900, res = 130)
    tryCatch(
      suppressWarnings(print(prophet_plot_components(m_obj, fc_raw_obj))),
      error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Component plot unavailable:\n", conditionMessage(e)),
             cex = 0.9)
      }
    )
    dev.off()

    message("Prophet plots saved.")
  }

  list(
    model_name  = "Prophet (Logistic, 350k cap)",
    fit         = m,
    cv_df       = cv_df,
    forecast_df = forecast_df,
    save_plots  = function(out_dir) save_prophet_plots(m, fc_raw, forecast_df, df, out_dir)
  )
}
