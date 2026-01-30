# Predicting the Unpredictable: Australian Rainfall Dynamics

A comprehensive statistical analysis and modeling framework for predicting daily rainfall across Australia using advanced Zero-Inflated Gamma Generalized Linear Mixed Models (GLMM).

## Overview

This project addresses the complex challenge of modeling Australian rainfall—a phenomenon characterized by extreme zero-inflation (~64% dry days), high skewness, and strong spatiotemporal dependencies. Using over 140,000 daily observations from 49 locations, we developed a hierarchical statistical framework that accurately models both rainfall occurrence and intensity.

## Key Findings

### The "Rain Corner" Effect
We identified and mathematically validated a critical non-linear interaction: rainfall occurs specifically when **high humidity coincides with low sunshine**. This "Rain Corner" phenomenon required custom interaction terms to capture properly.

### Persistence & Markov Chains
- If it rained yesterday, the odds of today being dry drop by **~76%**
- Dry spells exhibit exponential decay: each additional dry day reduces rain probability by **16.5%**
- The "wet" state is transient (47% persistence), while "dry" is stable (85% persistence)

### Wind Vector Dynamics
Decomposing wind into North-South and East-West components revealed:
- **Southerly winds** (from Southern Ocean) increase rainfall intensity
- **Westerly flows** (Roaring Forties) are major drivers of precipitation
- Prevailing air mass direction matters more than localized wind gusts

### Spatial Heterogeneity
Mixed-effects modeling captured ~10% of variance as purely geographic:
- Tropical cities (Darwin, Katherine): **+80% baseline rainfall**
- Arid interior (Nhil): **-52% baseline rainfall**
- One-size-fits-all models systematically fail across continental scales

## Model Performance

| Metric | Value | Interpretation |
|--------|-------|----------------|
| **AUC** | 0.832 | Strong discriminative power for wet/dry classification |
| **MAE** | 2.71 mm | Average prediction error within 2.7mm |
| **RMSE** | 7.48 mm | Accounts for extreme event variance |
| **Accuracy** | 75.4% | Overall classification correctness |
| **R² (Marginal)** | 0.345 | Fixed effects explain 34.5% of variance |
| **R² (Conditional)** | 0.441 | Total model explains 44.1% of variance |

## Statistical Framework

### Zero-Inflated Gamma GLMM

The model consists of two components:

1. **Zero-Inflation (Logistic)**: Models P(Dry Day)
   - Primary drivers: `humidity3pm`, `rain_yesterday`, `pressure_change`
   
2. **Conditional (Gamma)**: Models rainfall intensity when rain occurs
   - Primary drivers: `humidity`, `wind_vectors`, `instability_index`, `sunshine × humidity`

### Model Progression

| Model | Features Added | AIC | ΔAIC |
|-------|---------------|-----|------|
| M0 | Null (Intercept only) | 461,670 | - |
| M1 | Moisture & Pressure | 422,847 | -38,823 |
| M2 | Temporal & Persistence | 406,659 | -16,188 |
| M3 | Historical Trends | 404,936 | -1,723 |
| M4 | Energy & Interactions | 403,052 | -1,884 |
| M5 | Wind Vectors | 402,453 | -599 |
| M6 | Mixed Effects (Final) | 394,854 | -7,599 |

## Feature Engineering Highlights

### Cyclical Time Encoding
```r
day_sin = sin(2π × day_of_year / 365)
day_cos = cos(2π × day_of_year / 365)
```
Captures smooth seasonal transitions (Dec → Jan continuity)

### Wind Vector Decomposition
```r
gust_V_NS = wind_gust_speed × cos(θ)  # North-South
gust_U_EW = wind_gust_speed × sin(θ)  # East-West
```
Converts circular compass directions into continuous physical forces

### Interaction Terms
```r
sun_humid_interaction = (sunshine - mean) × (humidity - mean)
```
Captures the synergistic "Rain Corner" threshold effect

### Moving Averages (Temporal Context)
```r
rainfall_ma7 = lag(rollmean(rainfall, k=7), n=1)
```
Prevents data leakage while capturing weekly wetness regimes

## Installation & Requirements

### R Dependencies
```r
librarian::shelf(
  tidyverse, tidymodels, glmmTMB, DHARMa, 
  performance, missRanger, ranger, pROC,
  gtsummary, kableExtra, ggridges
)
```

### Data Requirements
- **Observations**: 142,199 daily records
- **Locations**: 49 cities across Australia
- **Time Period**: Multi-year coverage (exact dates in data)
- **Features**: 23 meteorological variables + engineered features

## Usage

### Quick Start
```r
# Load processed data
df_final <- read_csv("data/df_final.csv")

# Load pre-trained models
load("models/all_models_bundle.RData")

# Generate predictions
predictions <- predict(m6_mixed, type = "response")
prob_dry <- predict(m6_mixed, type = "zprob")
```

### Custom Model Training
```r
# Feature engineering
source("utils.R")
df_engineered <- engineer_features(df_final)

# Fit final model
m6_mixed <- glmmTMB(
  rainfall ~ humidity3pm + ... + 
    diag(1 + humidity3pm + rain_yesterday | location),
  ziformula = ~ humidity3pm + rain_yesterday + ...,
  family = ziGamma(link = "log"),
  data = df_engineered
)
```

## Validation & Diagnostics

### Model Assumptions
✅ **Distributional Fit**: Q-Q plot shows excellent Gamma alignment  
✅ **Homoscedasticity**: Residuals show uniform spread  
✅ **No Over-dispersion**: DHARMa test p = 0.192  
✅ **Zero-Inflation Calibration**: Predicted zeros match observed (ratio = 1.00)  
✅ **Temporal Independence**: Durbin-Watson = 2.04 (p = 0.24)

### Posterior Predictive Checks
Model-generated simulations closely replicate:
- Zero-inflation structure (64% dry days)
- Right-skewed intensity distribution
- Extreme event frequency (heavy tail)

## Limitations & Recommendations

### Current Scope

**✅ Suitable For:**
- 1-7 day probabilistic forecasting
- Agricultural planning & irrigation scheduling
- Historical data imputation (non-extreme events)
- Climate pattern analysis

**❌ NOT Suitable For:**
- Extreme Value Analysis (1-in-100-year floods)
- Engineering design loads (dam capacity, levee height)
- Catastrophic event prediction (>50mm outliers)

### Future Improvements

1. **Extreme Value Theory Integration**
   - Implement Generalized Pareto Distribution (GPD) for tail events
   - Develop separate EVT module for >95th percentile predictions

2. **Computational Optimization**
   - Implement INLA for faster Bayesian inference
   - Explore gradient boosting (XGBoost) for comparison

3. **Ensemble Methods**
   - Combine ZI-Gamma with Random Forest for robust predictions
   - Implement stacking for uncertainty quantification

## Citation

If you use this work, please cite:

```
Olande, C. (2026). Predicting the Unpredictable: Australian Rainfall Dynamics. 
A Zero-Inflated Gamma Mixed Model Approach. 
```

## License

This project is licensed under the MIT License - see LICENSE file for details.

## Acknowledgments

- Data source: Australian Bureau of Meteorology
- Statistical framework: glmmTMB, DHARMa packages
- Computational environment: R 4.x with Quarto

## Contact

For questions, suggestions, or collaboration:
- **Author**: Chris Olande
- **Date**: January 2026

---

**Technical Note**: This README reflects the state of analysis as of January 30, 2026. Model performance metrics are based on validation against held-out temporal data and cross-location predictions.
