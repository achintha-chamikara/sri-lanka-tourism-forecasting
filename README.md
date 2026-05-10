# 🇱🇰 Sri Lanka Tourism Arrivals Forecasting Dashboard

**ARIMAX · BSTS · Prophet**  
Cross‑validated monthly tourism forecasts for Sri Lanka (2014–2025 data → 2026 forecasts)

---

## 📊 Project Overview

This repository provides an end‑to‑end forecasting system for **monthly tourist arrivals to Sri Lanka**. Three state‑of‑the‑art time series models are trained, validated, and compared:

- **ARIMAX** – with exogenous shock dummies (Easter 2019, COVID‑19, economic crisis)
- **Prophet** – logistic growth with holiday effects (Sinhala New Year, Vesak)
- **BSTS** – Bayesian structural time series (local linear trend + seasonality)

The pipeline generates:
- Rolling cross‑validation metrics (MAPE, RMSE, sMAPE, CI coverage)
- 12‑month point forecasts with 80% and 95% intervals
- Interactive dashboards (static HTML and Shiny)
- Excel and CSV outputs for stakeholders

**Best model (lowest MAPE): ARIMAX (8.69%)** – recommended for day‑to‑day operational decisions.

---

## 📁 Repository Structure

---

## 🚀 Getting Started

### 1. Clone the repository

```bash
[git clone https://github.com/your-username/sri-lanka-tourism-forecast.git](https://github.com/achintha-chamikara/sri-lanka-tourism-forecasting.git)
