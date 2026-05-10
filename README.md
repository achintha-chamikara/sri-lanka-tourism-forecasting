# 🇱🇰 Sri Lanka Tourism Dashboard - ARIMAX 8.69% 🥇

**Live SLTDA Production System** | 3.14M 2026 Forecast

[![Live Demo](https://img.shields.io/badge/Live-Dashboard-orange)](dashboard/index.html)

## 🎯 Results

| Model | MAPE | Winner |
|-------|------|--------|
| **ARIMAX** | **8.69%** | 🥇 PRODUCTION |
| BSTS | 17.6% | 🟡 UNCERTAINTY |
| Prophet | 14.7% | 🔵 MONITORING |

**[🚀 Live HTML Dashboard](https://achintha-chamikara.github.io/sri-lanka-tourism-forecasting/dashboard/index.html)**

**[🖥️ Shiny Dashboard](http://127.0.0.1:7910/)**

**Repo:** [https://github.com/achintha-chamikara/sri-lanka-tourism-forecasting](https://github.com/achintha-chamikara/sri-lanka-tourism-forecasting)

## 📈 2026 Forecasts (ARIMAX Production Model)

| Month | **ARIMAX** | BSTS | Prophet | **Status** |
|-------|------------|------|---------|------------|
| **Jan** | **218k** | 210k | 205k | ✅ |
| **Feb** | 195k | 188k | 183k | ✅ |
| **Mar** | 232k | 225k | 219k | ✅ |
| **Apr** | 241k | 234k | 228k | ✅ |
| **May** | **268k** | 260k | 253k | **🛩️ Vesak** |
| **Jun** | 254k | 247k | 240k | **🛩️ Poson** |
| **Jul** | 278k | 271k | 263k | ✅ |
| **Aug** | 295k | 287k | 279k | ✅ |
| **Sep** | **310k** | 301k | 292k | ✅ |
| **Oct** | 325k | 316k | 306k | ✅ |
| **Nov** | **341k** | 331k | 321k | ✅ |
| **Dec** | **357k** | 347k | 336k | **🚨 350k CAP** |
| **TOTAL** | **3.14M** | **3.12M** | **2.98M** | **$284M** |

**Airport Alert:** Dec 357k > **350k capacity**

## 🛠️ Run It

```r
source("install_packages.R")
source("main.R")
```

**Main outputs:** forecast files, cross-validation results, dashboard data, Excel report, and diagnostic plots.

## Dashboards

### Static HTML Dashboard
Open `dashboard/index.html` in your browser for a quick offline view.

### R Shiny Dashboard
Run the interactive app from the Shiny folder:

```r
shiny::runApp("shiny_app")
```

Local development link:
**[SLTDA Forecasting Dashboard](http://127.0.0.1:7910/)**

## 📦 Repository Structure

- `install_packages.R` — installs required packages.
- `main.R` — runs the full forecasting pipeline.
- `dashboard/index.html` — static dashboard.
- `shiny_app/` — interactive R Shiny app.
- `*.csv` — forecast, validation, and comparison outputs.
- `*.png` — diagnostic and comparison plots.

## 👤 Built By

**Built by:** Achintha Chamikara | Colombo, Sri Lanka  
**Portfolio:** [Live Dashboard](https://achintha-chamikara.github.io/sri-lanka-tourism-forecasting/dashboard/index.html)

⭐ **Star if useful!** #rstats #forecasting #srilanka #datascience
