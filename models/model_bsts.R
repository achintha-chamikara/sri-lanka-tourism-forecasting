# =============================================================================
#  MODEL: BSTS — Sri Lanka Monthly Tourist Arrivals
#  File  : models/model_bsts.R
#  Fix   : colMeans array dimension error in save_bsts_plots()
# =============================================================================

run_bsts_model <- function(df_input) {

  library(bsts)
  library(dplyr)
  library(lubridate)
  library(ggplot2)

  # ── 1. PREPARE DATA ─────────────────────────────────────────
  df <- df_input %>%
    mutate(
      ds          = as.Date(paste(Year, MonthNum, "01"), "%Y %m %d"),
      y_raw       = ifelse(Arrivals == 0L, 1L, as.integer(Arrivals)),
      y           = log(y_raw),
      easter_bomb = as.integer(ds >= "2019-04-01" & ds <= "2019-06-30"),
      covid_lock  = as.integer(ds >= "2020-04-01" & ds <= "2020-12-31"),
      econ_crisis = as.integer(ds >= "2022-04-01" & ds <= "2022-12-31")
    ) %>%
    arrange(ds)

  y_vec   <- df$y
  n_total <- nrow(df)
  xreg    <- as.matrix(df[, c("easter_bomb", "covid_lock", "econ_crisis")])

  # ── 2. FIT FULL BSTS MODEL ──────────────────────────────────
  ss <- list()
  ss <- AddLocalLinearTrend(ss, y_vec)
  ss <- AddSeasonal(ss, y_vec, nseasons = 12)

  fit_bsts <- bsts(
    y_vec ~ xreg,
    state.specification = ss,
    niter = 1000,
    seed  = 42,
    ping  = 0
  )

  # ── 3. PROPHET-STYLE ROLLING CV ─────────────────────────────
  initial_n   <- 36
  cv_period_n <- 6
  cv_horizon  <- 6

  cutoff_starts <- seq(initial_n, n_total - cv_horizon, by = cv_period_n)
  cv_records    <- list()
  idx           <- 1L

  for (cut in cutoff_starts) {
    y_tr  <- y_vec[1:cut]
    xr_tr <- xreg[1:cut, , drop = FALSE]

    ss_cv <- list()
    ss_cv <- AddLocalLinearTrend(ss_cv, y_tr)
    ss_cv <- AddSeasonal(ss_cv, y_tr, nseasons = 12)

    fit_cv <- tryCatch(
      bsts(y_tr ~ xr_tr,
           state.specification = ss_cv,
           niter = 500, seed = 42, ping = 0),
      error = function(e) NULL
    )

    if (!is.null(fit_cv)) {
      for (h in 1:cv_horizon) {
        te <- cut + h
        if (te > n_total) break

        xr_new  <- xreg[(cut + 1):te, , drop = FALSE]
        fc_bsts <- tryCatch(
          predict(fit_cv, horizon = h, newdata = xr_new, burn = 100),
          error = function(e) NULL
        )
        if (is.null(fc_bsts)) next

        yhat       <- as.numeric(fc_bsts$mean)[h]
        dist_h     <- fc_bsts$distribution[, h]
        yhat_lower <- as.numeric(quantile(dist_h, 0.025))
        yhat_upper <- as.numeric(quantile(dist_h, 0.975))

        cv_records[[idx]] <- data.frame(
          model      = "BSTS",
          horizon    = h,
          cutoff     = as.character(df$ds[cut]),
          y          = y_vec[te],
          yhat       = yhat,
          yhat_lower = yhat_lower,
          yhat_upper = yhat_upper,
          stringsAsFactors = FALSE
        )
        idx <- idx + 1L
      }
    }
  }

  cv_df <- do.call(rbind, cv_records)

  # ── 4. 12-MONTH FORECAST (2026) ─────────────────────────────
  xreg_fut <- matrix(0, nrow = 12, ncol = 3,
                     dimnames = list(NULL, c("easter_bomb", "covid_lock", "econ_crisis")))

  fc_full <- predict(fit_bsts, horizon = 12, newdata = xreg_fut, burn = 200)

  # fc_full$distribution is [draws x 12]
  fc_mean <- colMeans(fc_full$distribution)
  fc_lo95 <- apply(fc_full$distribution, 2, quantile, 0.025)
  fc_hi95 <- apply(fc_full$distribution, 2, quantile, 0.975)
  fc_lo80 <- apply(fc_full$distribution, 2, quantile, 0.10)
  fc_hi80 <- apply(fc_full$distribution, 2, quantile, 0.90)

  forecast_df <- data.frame(
    model          = "BSTS",
    Period         = format(seq(as.Date("2026-01-01"), by = "month", length.out = 12), "%Y-%m"),
    ds             = seq(as.Date("2026-01-01"), by = "month", length.out = 12),
    Point_Forecast = round(exp(fc_mean)),
    Lower_95       = round(exp(fc_lo95)),
    Upper_95       = round(exp(fc_hi95)),
    Lower_80       = round(exp(fc_lo80)),
    Upper_80       = round(exp(fc_hi80))
  )

  # ── 5. DIAGNOSTIC PLOTS ─────────────────────────────────────
  save_bsts_plots <- function(df_data, fit, fc_obj, fc_df, out_dir) {
    library(ggplot2)
    library(scales)

    burn  <- min(200, floor(fit$niter * 0.2))

    # Best approach: use one.step.prediction.errors (always a matrix [iter x time])
    err_mat <- fit$one.step.prediction.errors
    if (!is.null(err_mat) && is.matrix(err_mat) && nrow(err_mat) > burn) {
      post_err   <- err_mat[(burn + 1):nrow(err_mat), , drop = FALSE]
      mean_err   <- colMeans(post_err)
      fitted_log <- as.numeric(fit$original.series) - mean_err
    } else {
      # Fallback: state contributions, safely handled
      fitted_mat <- fit$state.contributions
      fitted_log <- tryCatch({
        # dims: [iter x time x components]
        post_c      <- fitted_mat[(burn + 1):dim(fitted_mat)[1], , , drop = FALSE]
        contrib_sum <- apply(post_c, c(1, 2), sum)   # [post_iter x time]
        colMeans(contrib_sum)
      }, error = function(e) as.numeric(fit$original.series))
    }

    fit_df <- data.frame(
      ds   = df_data$ds,
      y    = exp(df_data$y),
      yhat = exp(fitted_log)
    )

    p1 <- ggplot(fit_df, aes(x = ds)) +
      geom_line(aes(y = y    / 1000, colour = "Actual"), linewidth = 0.8) +
      geom_line(aes(y = yhat / 1000, colour = "Fitted"),
                linewidth = 0.8, linetype = "dashed") +
      scale_colour_manual(values = c(Actual = "#2166ac", Fitted = "#7B1FA2")) +
      scale_y_continuous(labels = comma) +
      labs(title = "BSTS — Actual vs Fitted",
           x = NULL, y = "Arrivals (000s)", colour = NULL) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "top", plot.title = element_text(face = "bold"))

    hist_tail <- tail(fit_df, 24)

    p2 <- ggplot() +
      geom_line(data = hist_tail,
                aes(x = ds, y = y / 1000),
                colour = "#2166ac", linewidth = 0.9) +
      geom_ribbon(data = fc_df,
                  aes(x = ds, ymin = Lower_95 / 1000, ymax = Upper_95 / 1000),
                  fill = "#7B1FA2", alpha = 0.15) +
      geom_ribbon(data = fc_df,
                  aes(x = ds, ymin = Lower_80 / 1000, ymax = Upper_80 / 1000),
                  fill = "#7B1FA2", alpha = 0.25) +
      geom_line(data = fc_df,
                aes(x = ds, y = Point_Forecast / 1000),
                colour = "#7B1FA2", linewidth = 1) +
      scale_y_continuous(labels = comma) +
      labs(title = "BSTS — 12-Month Forecast (2026)",
           x = NULL, y = "Arrivals (000s)") +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(face = "bold"))

    ggsave(file.path(out_dir, "bsts_actual_vs_fitted.png"), p1,
           width = 9, height = 4, dpi = 150)
    ggsave(file.path(out_dir, "bsts_forecast_2026.png"),    p2,
           width = 9, height = 4, dpi = 150)
    message("BSTS plots saved.")
  }

  list(
    model_name  = "BSTS (Local Linear Trend + Seasonal)",
    fit         = fit_bsts,
    cv_df       = cv_df,
    forecast_df = forecast_df,
    df          = df,
    save_plots  = function(out_dir) save_bsts_plots(df, fit_bsts, fc_full, forecast_df, out_dir)
  )
}
