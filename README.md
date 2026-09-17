# Australian Rainfall Dynamics

![R Version](https://img.shields.io/badge/R-4.5.x-blue?logo=R)
![Quarto](https://img.shields.io/badge/Quarto-1.5+-blue?logo=quarto)
![Methodology](https://img.shields.io/badge/Method-ZI--Gamma%20GLMM-red)
![License](https://img.shields.io/badge/License-MIT-green)

**[Read the full report](https://chrisolande.github.io/australian-rainfall-dynamics/)**

Daily rainfall in Australia violates the core assumptions of Ordinary Least Squares regression in three simultaneous and non-remediable ways: 64.05% of observations are structural zeros, the positive values follow a severely leptokurtic distribution (kurtosis = 181.146), and consecutive observations exhibit strong Markovian dependence. This repository contains a frequentist analysis that addresses all three through a **Zero-Inflated Gamma Generalised Linear Mixed Model (ZIG-GLMM)**, decomposing precipitation into two physically distinct regimes estimated under a joint likelihood.

## Statistical Problem

A Gaussian linear model fitted to this data generates predictions for negative rainfall, a physical impossibility,and achieves an AUC of approximately 0.61 when used for binary occurrence classification. These are not failures of tuning or feature engineering: they are consequences of applying a symmetric, unbounded distributional family to data that is bounded below at zero, heavily right-skewed, and 64% zero-valued.

The ZIG framework resolves this by treating the zero mass and the positive continuous mass as products of separate processes with separate linear predictors:

$$P(Y_i = 0) = \pi_i + (1 - \pi_i) \cdot f_\Gamma(0 \mid \mu_i,\, \phi)$$

$$P(Y_i = y) = (1 - \pi_i) \cdot f_\Gamma(y \mid \mu_i,\, \phi), \quad y > 0$$

where:

$$\text{logit}(\pi_i) = \mathbf{W}_i \boldsymbol{\gamma} \qquad \log(\mu_i) = \mathbf{X}_i \boldsymbol{\beta} + \mathbf{Z}_i \mathbf{b}$$

$\pi_i$ is the structural dry-day probability estimated by the logistic hurdle component, $\mu_i$ is the conditional rainfall intensity estimated by the Gamma component, and $\mathbf{Z}_i \mathbf{b}$ encodes location-specific random slopes. The predictor matrices $\mathbf{W}_i$ and $\mathbf{X}_i$ are estimated independently, allowing the drivers of occurrence to differ from the drivers of intensity. This is the structural distinction between the ZIG framework and a Tweedie model.

All models are fitted using `glmmTMB` via maximum likelihood with Rubin's Rules pooling across five multiply-imputed datasets.

## Key Findings

**Thermodynamic interaction threshold.** Rainfall is gated by a non-linear joint condition in the feature space: precipitation concentrates almost exclusively when afternoon humidity exceeds approximately 60% and daily sunshine falls below approximately five hours simultaneously, the Rain Corner. A centred multiplicative interaction term captures this structure with a VIF of 1.174, confirming that centring successfully removed the artificial collinearity between the interaction and its constituent main effects. The term is jointly significant at $F(2,\ 24.1) = 49.375$, $p < 0.001$.

**Markovian persistence.** Having rained the previous day is the single strongest predictor in the hurdle component across the entire model sequence. Dry states are highly self-reinforcing (85% dry-to-dry transition probability); wet states are transient (47% wet-to-wet). The temporal feature block produced the largest pooled $D_1$ $F$-statistic in the model sequence at $F(4,\ 71.3) = 802.158$, $p < 0.001$. A model that assumes daily conditional independence loses this signal entirely.

**Directional wind dynamics.** Decomposing compass bearings into orthogonal North-South ($V$) and East-West ($U$) vectors revealed that southerly and westerly morning flows are the primary directional drivers of intensity, consistent with the influence of Southern Ocean frontal systems and the mid-latitude westerlies. Morning wind direction is three to four times more informative than peak gust direction. The four wind vector parameters are jointly significant at $F(4,\ 272.8) = 133.422$, $p < 0.001$.

**Spatial heterogeneity.** After controlling for all dynamic meteorological predictors, location-specific random effects account for approximately 10% of total rainfall variance. Tropical Top End stations produce roughly 75 to 77% more rainfall than an average station at identical atmospheric conditions; arid interior stations produce approximately half as much. The addition of the mixed-effects structure yields $\Delta\text{AIC} = -6{,}915.32$ over the fixed-effects model.

## Model Progression

| Model | Specification | AIC | Delta AIC |
| :--- | :--- | ---: | ---: |
| M0 | Null baseline | 461,669.7 | |
| M1 | Moisture and pressure dynamics | 425,731.4 | -35,938.3 |
| M2 | Seasonality and day-to-day persistence | 412920.9| -12810.5|
| M3 | Accumulated weather history |408915.1 | -4005.8|
| M4 | Thermodynamic energy and Rain Corner interaction | 407848.8| -1066.3|
| M5 | Circular wind vectors | 407066.6| -782.2|
| M6 | Mixed effects with random slopes | 400151.2| -6,915.4|

Every extension is confirmed by pooled $D_1$ Wald test at $p < 0.001$. The moisture and pressure block (M1) yields the largest single improvement in the sequence at $\Delta\text{AIC} = -35{,}938.3$, confirming that atmospheric moisture dynamics constitute the fundamental predictive substrate.

## Validation

The final model (M6) was subjected to four independent validation procedures.

| Metric | Value | Interpretation |
| :--- | :--- | :--- |
| AUC (occurrence submodel) | 0.813 | Strong discriminative ability between dry and wet days |
| Youden-optimal threshold | 0.6246 | Higher than 0.5 reflecting the 64% dry-day prior |
| Brier Score | 0.1654 | Calibrated probabilistic accuracy |
| Brier Skill Score | 0.2819 | Improvement over naive climatological baseline |
| MAE (all observations) | 2.760 mm | Average absolute prediction error across all days |
| MAE (rain-days only) | 5.607 mm | Harder prediction problem on positive observations |
| RMSE (all observations) | 7.567 mm | Driven upward by under-prediction of extreme events |
| RMSE (rain-days only) | 12.263 mm | |
| DHARMa dispersion test | $p = 0.152$ | No evidence of over- or under-dispersion |
| Zero-inflation calibration | Ratio = 1.00, $p = 0.512$ | Model generates exactly the correct proportion of dry days |
| Durbin-Watson statistic | ~2.0 across most locations | No residual temporal autocorrelation |

## Report Structure

| Chapter | Content |
| :--- | :--- |
| 1: Introduction | Statistical pathology of Australian rainfall and the failure modes of standard regression |
| 2: Data Preparation | Two-stage hybrid imputation for 42-48% missingness in key meteorological variables |
| 3: Imputation Sensitivity | Convergence diagnostics, distributional fidelity, and Rubin's Rules variance decomposition |
| 4: Exploratory Data Analysis | Zero-inflation, distributional properties, Markovian persistence, and the Rain Corner |
| 5: Feature Engineering | Wind vectors, cyclical encoding, interaction terms, and temporal lag features |
| 6: Modelling | Progressive ZIG model construction from null baseline through spatial mixed effects |
| 7: Model Evaluation | ROC analysis, spatial random effects, DHARMa diagnostics, and autocorrelation testing |
| 8: Model Selection | Distributional family comparison, pooled $D_1$ Wald tests, and AIC model sequence |
| 9: Conclusion | Principal findings, limitations, and recommended extensions |

## Repository Structure

```
.
├── index.qmd                        # Landing page and executive overview
├── _quarto.yml                      # Site configuration
├── config.R                         # Shared project configuration
├── utils.R                          # Shared utility functions
├── air.toml                         # Air live-reload configuration
├── custom.scss                      # Light theme styles
├── custom-dark.scss                 # Dark theme styles
├── renv.lock                        # Reproducible package lockfile
├── LICENSE
├── README.md
│
├── chapters/                        # Quarto source documents
│   ├── 01-intro.qmd
│   ├── 02-data-prep.qmd
│   ├── 03-sensitivity.qmd
│   ├── 04-eda.qmd
│   ├── 05-features.qmd
│   ├── 06-modeling.qmd
│   ├── 07-evaluation.qmd
│   ├── 08-model-selection.qmd
│   └── 09-conclusion.qmd
│
├── chapter02-03/                    # Data cleaning and imputation scripts
│   ├── cleaning.R
│   ├── data_missingness.R
│   ├── imputation.R
│   └── sensitivity_analysis.R
│
├── chapter4/                        # EDA scripts
│   ├── correlation.R
│   ├── interactions.R
│   ├── pressure.R
│   ├── seasonality.R
│   ├── target.R
│   └── temporal.R
│
├── chapter5/                        # Feature engineering scripts
│   ├── feature_engineering.R
│   ├── ma_correlation.R
│   └── moving_averages.R
│
├── chapter6/                        # Modelling scripts
│   ├── main.R
│   ├── mitml_wrappers.R
│   ├── modeling_core.R
│   ├── modeling_pool.R
│   ├── modeling_runners.R
│   └── Reporting.R
│
├── chapter7/                        # Model evaluation scripts
│   ├── eval_autocorrelation.R
│   ├── eval_classification.R
│   ├── eval_diagnostics.R
│   └── eval_random_effects.R
│
├── chapter8/                        # Model selection scripts
│   ├── distribution_comparison.R
│   ├── model_progression.R
│   └── predictive_accuracy.R
│
├── data/
│   ├── weatherAUS.csv               # Source data (49 stations, 142,199 observations)
│   ├── df_clean.rds                 # Cleaned dataset
│   ├── df_final.csv                 # Feature-engineered dataset
│   ├── imp_mids.rds                 # Multiple imputation object (5 datasets)
│   ├── ppc_dat.rds                  # Posterior predictive check data
│   └── dw_results.csv               # Durbin-Watson test results by location
│
├── models/
│   ├── m0_null.rds                  # Null baseline
│   ├── m1_moisture.rds              # Moisture and pressure dynamics
│   ├── m2_temporal.rds              # Seasonality and persistence
│   ├── m3_history.rds               # Accumulated weather history
│   ├── m4_energy.rds                # Thermodynamic energy and Rain Corner interaction
│   ├── m5_wind.rds                  # Circular wind vectors
│   ├── m6_mixed.rds                 # Final mixed-effects model
│   ├── m_linear.rds                 # Gaussian comparison model
│   ├── m_lognormal.rds              # Log-normal comparison model
│   ├── m_tweedie.rds                # Tweedie comparison model
│   ├── check_data.rds               # Predictions from all comparison families
│   └── rainfall_preds.rds           # Final model predictions
│
└── docs/                            # Rendered HTML report (GitHub Pages)
    ├── index.html
    ├── chapters/
    ├── search.json
    └── site_libs/
```

## Reproduction

**Requirements:** R 4.5.x, Quarto CLI 1.5+

**Install dependencies:**

```r
librarian::shelf(
  aod,         broom,        broom.mixed,  car,
  caret,       cocor,        corrplot,     DHARMa,
  DT,          forcats,      furrr,        GGally,
  ggpubr,      ggridges,     glmmTMB,      glue,
  gridExtra,   gt,           gtsummary,    here,
  janitor,     kableExtra,   lmtest,       Metrics,
  mgcv,        mice,         missRanger,   mitml,
  moments,     multcompView, naniar,       parallel,
  patchwork,   performance,  pROC,         ranger,
  RhpcBLASctl, rstatix,      scales,       sjPlot,
  skimr,       splines,      tidymodels,   tidyverse,
  viridis,     yardstick,    zoo
)
```

Or restore the exact environment from the lockfile:

```r
renv::restore()
```

**Render the report:**

```bash
git clone https://github.com/Chrisolande/australian-rainfall-dynamics.git
cd australian-rainfall-dynamics
quarto render index.qmd
```

Pre-fitted model objects are stored individually under `models/`. The report renders without re-running the `glmmTMB` optimisations, which involve high-dimensional mixed-effects likelihoods fitted across five imputed datasets and are computationally intensive.

## Scope and Limitations

**Suitable applications:** Short-range probabilistic forecasting (1 to 7 days), regional climate characterisation, agricultural drought planning, and historical gap-filling for non-extreme observations.

**Not suitable for:** Extreme value analysis. The Gamma distribution has exponentially declining tails and systematically underestimates the probability and magnitude of rare events exceeding approximately the 95th percentile. Flood risk and infrastructure design applications require Extreme Value Theory methods, specifically a Generalised Pareto Distribution fitted to threshold exceedances. A natural extension is a composite model that grafts a GPD onto the ZIG body at the 95th percentile.

## Author

**Chris Olande** | Statistician and Programmer | Nairobi, Kenya

[LinkedIn](https://www.linkedin.com/in/chris-olande-6557a3238/)

## Citation

Olande, C. (2026). *Australian Rainfall Dynamics: A Zero-Inflated Gamma Mixed-Effects Model for Spatiotemporal Precipitation*. GitHub. https://github.com/Chrisolande/australian-rainfall-dynamics

**License:** MIT