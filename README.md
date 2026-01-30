# Predicting the Unpredictable: Australian Rainfall Dynamics

This repository contains a full statistical analysis of daily rainfall across Australia using zero-inflated gamma mixed models. The work blends exploratory data analysis, feature engineering, and model diagnostics to explain both rainfall occurrence and intensity across 49 locations and more than 142,000 observations.

## Statistical Challenge

Australian rainfall is highly intermittent and non-Gaussian. The dataset includes roughly 64 percent dry days, strong right skew, and heavy tail events where daily rainfall exceeds 100 mm. These characteristics motivate a two-part model that separates the probability of rain from the magnitude of rain.

## Project Goals

- Quantify how rainfall occurrence and intensity differ across regions and seasons.
- Handle zero inflation by modeling dry day probability separately from rainfall intensity.
- Incorporate temporal persistence, meteorological drivers, and spatial heterogeneity.
- Provide a reproducible Quarto report with figures, model outputs, and validation checks.

## What Is Included

- **Quarto analysis**: `index.qmd` contains the full narrative, code, and figures.
- **Data sources**: `data/` contains raw data and supplemental examples.
- **Pretrained models**: `models/` contains fitted model objects in RDS format.
- **Rendered site**: `docs/` contains the static HTML output and can be hosted as a site.
- **Notebooks**: `ausweather.ipynb` and `refactored.ipynb` contain exploratory and refactored workflows.

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

The primary dataset is `data/weatherAUS.csv`, which includes daily observations from 49 Australian locations. The analysis covers more than 142,000 observations and includes variables such as humidity, pressure, wind direction, cloud cover, and temperature. Key engineered features include:

- **Cyclical seasonality** using sine and cosine transforms for day of year.
- **Wind vector decomposition** to convert wind direction into North-South and East-West components.
- **Rain persistence features** such as wet or dry day streaks and lagged rainfall.
- **Interaction terms** that capture the combined effect of sunshine and humidity.

Missingness is most pronounced in sunshine and evaporation, which motivates Random Forest-based imputation with predictive mean matching to preserve distributional structure.

## Modeling Approach

The analysis uses a zero-inflated gamma generalized linear mixed model, which separates the process into:

1. **Zero inflation model** for dry day probability.
2. **Gamma intensity model** for rainfall amount conditional on rain.

The final model includes fixed effects for meteorological drivers and random effects for location. Model selection uses AIC comparisons, likelihood ratio tests, and diagnostic checks using the DHARMa package.

## Key Findings Summary

- Rainfall probability is strongly tied to humidity and prior day rain state.
- The joint effect of humidity and sunshine creates a nonlinear threshold for rain onset, described as the rain corner effect.
- Wind direction and pressure gradients influence rainfall intensity, especially for southerly and westerly flows.
- Location-level random effects capture meaningful geographic structure, roughly 10 percent of total variance.

## Exploratory Insights

- Seasonal intensity peaks in winter months with June and July showing the highest share of rainfall days.
- Markov chain analysis shows dry persistence around 85 percent and wet persistence around 47 percent.
- Humidity and cloud cover correlate positively with rainfall, while sunshine and evaporation are negative drivers.

## Model Progression

| Model | Focus | AIC | Delta AIC |
| --- | --- | ---: | ---: |
| M0 | Null baseline | 461,670 | - |
| M1 | Moisture and pressure | 422,847 | -38,823 |
| M2 | Temporal persistence | 406,659 | -16,188 |
| M3 | Historical trends | 404,936 | -1,723 |
| M4 | Energy and interactions | 403,052 | -1,884 |
| M5 | Wind vectors | 402,453 | -599 |
| M6 | Mixed effects | 394,854 | -7,599 |

## Performance Metrics

- **AUC**: 0.83 for wet and dry discrimination.
- **MAE**: 2.71 mm for rainfall magnitude.
- **RMSE**: 7.48 mm, reflecting heavy tail events.
- **R2 marginal**: 0.345 for fixed effects.
- **R2 conditional**: 0.441 with location effects.

## Suitable Use Cases

- Short-range probabilistic forecasting.
- Agricultural planning and irrigation scheduling.
- Historical data imputation for non-extreme events.

## Unsuitable Use Cases

- Extreme value prediction such as 100-year flood estimation.
- Engineering design loads or catastrophic event forecasting.

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
