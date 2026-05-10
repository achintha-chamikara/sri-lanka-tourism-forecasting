# =============================================================================
#  MODEL: ARIMAX — Sri Lanka Monthly Tourist Arrivals
#  File  : models/model_arimax.R
#  Output: arimax_cv_results (list), arimax_forecast_df (data.frame)
# =============================================================================

run_arimax_model <- function(df_input) {

  library(forecast)
  library(tseries)

  set.seed(2025)
  options(scipen = 999)

  # ── 1. PREPARE DATA ─────────────────────────────────────────
  raw <- df_input
  n   <- nrow(raw)

  # Interpolate zeros
  zero_idx <- which(raw$Arrivals == 0)
  if (length(zero_idx) > 0) {
    pre_val  <- raw$Arrivals[min(zero_idx) - 1]
    post_val <- raw$Arrivals[max(zero_idx) + 1]
    m_z      <- length(zero_idx)
    raw$Arrivals[zero_idx] <- round(
      seq(pre_val, post_val, length.out = m_z + 2)[2:(m_z + 1)]
    )
  }
  raw$logArr <- log(raw$Arrivals)
  y_ts <- ts(raw$logArr, start = c(2014, 1), frequency = 12)

  # ── 2. DUMMIES ──────────────────────────────────────────────
  idx_fn <- function(yr, mo) which(raw$Year == yr & raw$MonthNum == mo)

  pulse_attack  <- numeric(n); pulse_attack[idx_fn(2019, 4)] <- 1
  step_easter   <- as.integer(raw$Year > 2019 | (raw$Year == 2019 & raw$MonthNum >= 5))
  covid_period  <- as.integer((raw$Year == 2020 & raw$MonthNum >= 4) | raw$Year == 2021)
  step_econ2022 <- as.integer(raw$Year > 2022 | (raw$Year == 2022 & raw$MonthNum >= 4))

  xreg_base <- cbind(
    pulse_attack  = pulse_attack,
    step_easter   = step_easter,
    covid_period  = covid_period,
    step_econ2022 = step_econ2022
  )

  # ── 3. MODEL SELECTION ──────────────────────────────────────
  try_arima <- function(p, d, q, P, D, Q, xr, label) {
    fit <- tryCatch(
      Arima(y_ts, order = c(p, d, q), seasonal = c(P, D, Q),
            xreg = xr, method = "ML"),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      lb <- Box.test(residuals(fit), lag = 20, type = "Ljung-Box")
      return(list(fit = fit, lb = lb, aic = AIC(fit), label = label))
    }
    return(NULL)
  }

  candidates <- list(
    try_arima(0, 1, 1, 2, 1, 0, xreg_base, "ARIMA(0,1,1)(2,1,0)"),
    try_arima(1, 1, 1, 2, 1, 0, xreg_base, "ARIMA(1,1,1)(2,1,0)"),
    try_arima(0, 1, 1, 2, 1, 1, xreg_base, "ARIMA(0,1,1)(2,1,1)"),
    try_arima(1, 1, 1, 2, 1, 1, xreg_base, "ARIMA(1,1,1)(2,1,1)"),
    try_arima(1, 1, 2, 2, 1, 1, xreg_base, "ARIMA(1,1,2)(2,1,1)"),
    try_arima(2, 1, 2, 2, 1, 1, xreg_base, "ARIMA(2,1,2)(2,1,1)"),
    try_arima(0, 1, 2, 1, 1, 1, xreg_base, "ARIMA(0,1,2)(1,1,1)"),
    try_arima(1, 1, 1, 1, 1, 1, xreg_base, "ARIMA(1,1,1)(1,1,1)")
  )
  candidates <- candidates[!sapply(candidates, is.null)]

  good <- candidates[sapply(candidates, function(x) x$lb$p.value > 0.05)]
  if (length(good) > 0) {
    best <- good[[which.min(sapply(good, `[[`, "aic"))]]
  } else {
    best <- candidates[[which.min(sapply(candidates, `[[`, "aic"))]]
  }

  fit_final <- best$fit
  ord       <- arimaorder(fit_final)

  # ── 4. ROLLING WINDOW CV (window=60, h=1 to 6) ──────────────
  win        <- 60
  horizons   <- 1:6
  cv_records <- list()

  for (h in horizons) {
    roll_act <- c(); roll_fc <- c(); cutoffs <- c()

    for (i in seq_len(n - win - h + 1)) {
      tr  <- i:(i + win - 1)
      te  <- i + win + h - 1
      y_tr <- ts(raw$logArr[tr], frequency = 12,
                 start = c(raw$Year[tr[1]], raw$MonthNum[tr[1]]))
      xr_tr <- xreg_base[tr, , drop = FALSE]
      xr_fc <- xreg_base[(i + win):(i + win + h - 1), , drop = FALSE]

      fit_r <- tryCatch(
        Arima(y_tr, order = c(ord[1], ord[2], ord[3]),
              seasonal = c(ord[4], ord[5], ord[6]),
              xreg = xr_tr, method = "ML"),
        error = function(e) {
          tryCatch(
            Arima(y_tr, order = c(0, 1, 1), seasonal = c(1, 1, 0),
                  xreg = xr_tr, method = "ML"),
            error = function(e2) NULL
          )
        }
      )

      if (!is.null(fit_r)) {
        fc_r <- forecast(fit_r, h = h, xreg = xr_fc)
        yhat_h <- as.numeric(fc_r$mean)[h]
        yhat_lo <- as.numeric(fc_r$lower[h, "95%"])
        yhat_hi <- as.numeric(fc_r$upper[h, "95%"])
        roll_fc  <- c(roll_fc, yhat_h)
        roll_act <- c(roll_act, raw$logArr[te])
        cutoffs  <- c(cutoffs, as.character(
          as.Date(paste(raw$Year[i + win - 1], raw$MonthNum[i + win - 1], "01"), "%Y %m %d")
        ))
      }
    }

    if (length(roll_act) > 0) {
      cv_records[[h]] <- data.frame(
        model   = "ARIMAX",
        horizon = h,
        cutoff  = cutoffs,
        y       = roll_act,
        yhat    = roll_fc,
        stringsAsFactors = FALSE
      )
    }
  }

  cv_df <- do.call(rbind, cv_records)

  # ── 5. 12-MONTH FORECAST (2026) ─────────────────────────────
  xreg_fut <- matrix(0, nrow = 12, ncol = 4,
                     dimnames = list(NULL, colnames(xreg_base)))
  xreg_fut[, "step_easter"]   <- 1
  xreg_fut[, "step_econ2022"] <- 1

  fc_norm <- forecast(fit_final, h = 12, xreg = xreg_fut)

  forecast_df <- data.frame(
    model          = "ARIMAX",
    Period         = format(seq(as.Date("2026-01-01"), by = "month", length.out = 12), "%Y-%m"),
    ds             = seq(as.Date("2026-01-01"), by = "month", length.out = 12),
    Point_Forecast = round(exp(as.numeric(fc_norm$mean))),
    Lower_95       = round(exp(as.numeric(fc_norm$lower[, "95%"]))),
    Upper_95       = round(exp(as.numeric(fc_norm$upper[, "95%"]))),
    Lower_80       = round(exp(as.numeric(fc_norm$lower[, "80%"]))),
    Upper_80       = round(exp(as.numeric(fc_norm$upper[, "80%"])))
  )

  # ── 6. DIAGNOSTIC PLOTS ─────────────────────────────────────
  save_arimax_plots <- function(raw_data, fit, fc_df, out_dir) {
    library(ggplot2)
    library(dplyr)

    resid_f <- residuals(fit)
    n_r     <- length(resid_f)

    # Actual vs Fitted
    actual_df <- data.frame(
      t    = seq_along(raw_data$logArr),
      y    = exp(raw_data$logArr),
      yhat = exp(as.numeric(fitted(fit)))
    )
    p1 <- ggplot(actual_df, aes(x = t)) +
      geom_line(aes(y = y / 1000, colour = "Actual"), linewidth = 0.8) +
      geom_line(aes(y = yhat / 1000, colour = "Fitted"), linewidth = 0.8, linetype = "dashed") +
      scale_colour_manual(values = c(Actual = "#2166ac", Fitted = "#d6604d")) +
      labs(title = sprintf("ARIMAX — Actual vs Fitted"),
           x = "Month Index", y = "Arrivals (000s)", colour = NULL) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "top", plot.title = element_text(face = "bold"))

    # Residuals
    resid_df <- data.frame(t = seq_len(n_r), r = as.numeric(resid_f))
    p2 <- ggplot(resid_df, aes(x = t, y = r)) +
      geom_line(colour = "#5aae61", linewidth = 0.7) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
      geom_hline(yintercept = c(-2, 2) * sd(resid_f), linetype = "dotted", colour = "#e31a1c") +
      labs(title = "ARIMAX — Residuals", x = "Month Index", y = "Residual") +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(face = "bold"))

    # Forecast
    hist_tail <- tail(raw_data, 24)
    hist_tail$ds <- as.Date(paste(hist_tail$Year, hist_tail$MonthNum, "01"), "%Y %m %d")

    p3 <- ggplot() +
      geom_line(data = hist_tail,
                aes(x = ds, y = Arrivals / 1000), colour = "#2166ac", linewidth = 0.9) +
      geom_ribbon(data = fc_df,
                  aes(x = ds, ymin = Lower_95 / 1000, ymax = Upper_95 / 1000),
                  fill = "#e31a1c", alpha = 0.15) +
      geom_ribbon(data = fc_df,
                  aes(x = ds, ymin = Lower_80 / 1000, ymax = Upper_80 / 1000),
                  fill = "#e31a1c", alpha = 0.25) +
      geom_line(data = fc_df,
                aes(x = ds, y = Point_Forecast / 1000), colour = "#e31a1c", linewidth = 1) +
      labs(title = "ARIMAX — 12-Month Forecast (2026)",
           x = NULL, y = "Arrivals (000s)") +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(face = "bold"))

    ggsave(file.path(out_dir, "arimax_actual_vs_fitted.png"), p1, width = 9, height = 4, dpi = 150)
    ggsave(file.path(out_dir, "arimax_residuals.png"),        p2, width = 9, height = 4, dpi = 150)
    ggsave(file.path(out_dir, "arimax_forecast_2026.png"),    p3, width = 9, height = 4, dpi = 150)
    message("ARIMAX plots saved.")
  }

  list(
    model_name   = sprintf("ARIMAX(%d,%d,%d)(%d,%d,%d)[12]",
                           ord[1], ord[2], ord[3], ord[4], ord[5], ord[6]),
    fit          = fit_final,
    cv_df        = cv_df,
    forecast_df  = forecast_df,
    raw          = raw,
    save_plots   = function(out_dir) save_arimax_plots(raw, fit_final, forecast_df, out_dir)
  )
}
