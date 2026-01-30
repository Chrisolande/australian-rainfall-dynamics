# Predicting the Unpredictable: Australian Rainfall Dynamics

This repository contains a full statistical analysis of daily rainfall across Australia using zero-inflated gamma mixed models. The work blends exploratory data analysis, feature engineering, and model diagnostics to explain both rainfall occurrence and intensity across 49 locations.

## Project Goals

- Quantify how rainfall occurrence and intensity differ across regions and seasons.
- Handle zero inflation by modeling dry day probability separately from rainfall intensity.
- Incorporate temporal persistence, meteorological drivers, and spatial heterogeneity.
- Provide a reproducible Quarto report with figures, model outputs, and validation checks.

## What Is Included

- **Quarto analysis**: `index.qmd` holds the full narrative, code, and figures.
- **Data sources**: Raw data and supplemental examples live in `data/`.
- **Pretrained models**: Fitted models are stored in `models/` as RDS objects.
- **Rendered site**: The static HTML output is in `docs/` and can be hosted as a site.
- **Notebooks**: `ausweather.ipynb` and `refactored.ipynb` capture exploratory and refactored workflows.

## Repository Layout

```
.
├── index.qmd          # Quarto report with full analysis
├── _quarto.yml        # Quarto site configuration
├── data/              # Raw datasets, including weatherAUS.csv
├── models/            # Pretrained model objects
├── docs/              # Rendered HTML site output
├── ausweather.ipynb   # Original exploratory notebook
└── refactored.ipynb   # Refined notebook workflow
```

## Data Overview

The primary dataset is `data/weatherAUS.csv`, which includes daily observations from 49 Australian locations. The analysis covers more than 140,000 observations and includes variables such as humidity, pressure, wind direction, cloud cover, and temperature. Key engineered features include:

- **Cyclical seasonality** using sine and cosine transforms for day of year.
- **Wind vector decomposition** to convert wind direction into North-South and East-West components.
- **Rain persistence features** such as wet or dry day streaks and lagged rainfall.
- **Interaction terms** that capture the combined effect of sunshine and humidity.

## Modeling Approach

The analysis uses a zero-inflated gamma generalized linear mixed model, which separates the process into:

1. **Zero inflation model** for dry day probability.
2. **Gamma intensity model** for rainfall amount conditional on rain.

The final model includes fixed effects for meteorological drivers and random effects for location. Model selection uses AIC comparisons, likelihood ratio tests, and diagnostic checks with DHARMa.

## Key Findings Summary

- Rainfall probability is strongly tied to humidity and prior day rain state.
- The joint effect of humidity and sunshine creates a nonlinear threshold for rain onset.
- Wind direction and pressure gradients influence rainfall intensity.
- Location level random effects capture meaningful geographic structure.

## Reproducing the Analysis

### Requirements

- R 4.x
- Quarto
- R packages listed in the `librarian::shelf` call in `index.qmd`

### Steps

1. Install dependencies in R:
   ```r
   librarian::shelf(
     tidyverse, tidymodels, glmmTMB, DHARMa, performance,
     missRanger, ranger, pROC, gtsummary, kableExtra, ggridges
   )
   ```
2. Render the report:
   ```bash
   quarto render index.qmd
   ```
3. Open `docs/index.html` in a browser to view the report.

## Validation and Diagnostics

The report includes diagnostics for:

- Residual dispersion and zero inflation using DHARMa.
- Temporal autocorrelation checks using Durbin-Watson statistics.
- Comparative model tests using likelihood ratio tests and AIC deltas.

## Notes on Outputs

- `docs/` contains the rendered site and is safe to deploy to GitHub Pages.
- Model objects in `models/` are provided for inference and reuse without refitting.

## Citation

If you use this work, please cite:

```
Olande, C. (2026). Predicting the Unpredictable: Australian Rainfall Dynamics.
A Zero-Inflated Gamma Mixed Model Approach.
```

## License

This project is licensed under the MIT License. See `LICENSE` for details.
