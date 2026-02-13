# Australian Rainfall Dynamics
## A Zero-Inflated Gamma Mixed-Effects Model for Spatiotemporal Precipitation

![R Version](https://img.shields.io/badge/R-4.5.x-blue?logo=R)
![Quarto](https://img.shields.io/badge/Quarto-1.5+-blue?logo=quarto)
![Methodology](https://img.shields.io/badge/Method-ZI--Gamma%20GLMM-red)
![License](https://img.shields.io/badge/License-MIT-green)

---

Daily rainfall in Australia violates the core assumptions of Ordinary Least Squares regression in three simultaneous and non-remediable ways: 64% of observations are structural zeros, the positive values follow a severely leptokurtic distribution (kurtosis = 181), and consecutive observations exhibit strong Markovian dependence. This repository contains a frequentist analysis that addresses all three through a **Zero-Inflated Gamma Generalised Linear Mixed Model (ZIG-GLMM)**, decomposing precipitation into two physically distinct regimes estimated under a joint likelihood.

The full rendered report is available via GitHub Pages. Source code, fitted model objects, and data are documented below.

---

## Statistical Problem

A Gaussian linear model fitted to this data predicts negative rainfall for approximately 12% of observations and achieves an AUC of 0.61 when used for binary occurrence classification. These are not failures of tuning or feature selection: they are consequences of applying a symmetric, unbounded distributional family to data that is bounded below at zero, heavily right-skewed, and 64% zero-valued.

The ZIG framework resolves this by treating the zero mass and the positive continuous mass as products of separate processes with separate linear predictors:

$$P(Y_i = 0) = \pi_i + (1 - \pi_i) \cdot f_\Gamma(0 \mid \mu_i,\, \phi)$$

$$P(Y_i = y) = (1 - \pi_i) \cdot f_\Gamma(y \mid \mu_i,\, \phi), \quad y > 0$$

where:

$$\text{logit}(\pi_i) = \mathbf{W}_i \boldsymbol{\gamma} \qquad \log(\mu_i) = \mathbf{X}_i \boldsymbol{\beta} + \mathbf{Z}_i \mathbf{b}$$

$\pi_i$ is the structural dry-day probability estimated by the logistic hurdle component, $\mu_i$ is the conditional rainfall intensity estimated by the Gamma component, and $\mathbf{Z}_i \mathbf{b}$ encodes location-specific random slopes. The two predictor matrices $\mathbf{W}_i$ and $\mathbf{X}_i$ are estimated independently, allowing the drivers of occurrence to differ from the drivers of intensity.

---

## Key Findings

**Thermodynamic interaction threshold.** Rainfall is gated by a non-linear joint condition in the feature space: precipitation concentrates almost exclusively when afternoon humidity exceeds approximately 50% and daily sunshine falls below approximately five hours simultaneously. A centered multiplicative interaction term captures this structure with a VIF of 1.25, confirming that centring successfully removed the artificial collinearity between the interaction and its constituent main effects.

**Markovian persistence.** Having rained the previous day reduces the odds of today being structurally dry by 76%, making `rain_yesterday` the strongest single predictor in the hurdle component. Dry states are highly stable (85% dry-to-dry transition probability); wet states are transient (47% wet-to-wet). A model that assumes daily conditional independence loses this signal entirely.

**Directional wind dynamics.** Decomposing compass bearings into orthogonal North-South ($V$) and East-West ($U$) vectors revealed that southerly and westerly morning flows are the primary directional drivers of intensity, consistent with the influence of Southern Ocean frontal systems and the mid-latitude westerlies. Morning wind direction is three to four times more informative than peak gust direction.

**Spatial heterogeneity.** After controlling for all dynamic meteorological predictors, location-specific random effects account for approximately 10% of total rainfall variance. Tropical Top End stations produce roughly 1.7 to 1.8 times the rainfall of an average station at identical atmospheric conditions; arid interior stations produce approximately half as much. A pooled fixed-effects model averages over this variation, systematically mis-predicting both extremes.

---

## Model Progression

| Model | Specification | AIC | Delta AIC |
| :--- | :--- | ---: | ---: |
| M0 | Null baseline | 461,669.7 | |
| M1 | Moisture and pressure dynamics | 422,630 | -39,039.7 |
| M2 | Temporal persistence and seasonality | 406,826 | -15,804 |
| M3 | Accumulated weather history | 405,125 | -1,701 |
| M4 | Thermodynamic energy and interactions | 403,742 | -1,383 |
| M5 | Circular wind vectors | 403,141 | -601 |
| M6 | Mixed effects with random slopes | 396,638 | -6,503 |

Every extension is confirmed by Likelihood Ratio Test at $p < 0.001$ after Holm correction. Akaike weights place all probability on M6.

**Variance decomposition (Nakagawa's $R^2$):**
- Marginal $R^2$ (fixed effects only): 0.3498
- Conditional $R^2$ (fixed and random effects): 0.4464
- Geographic contribution: approximately 10% of total variance attributable to location after controlling for meteorological predictors

---

## Validation

The final model (M6) was subjected to four independent validation procedures.

| Test | Result | Interpretation |
| :--- | :--- | :--- |
| AUC (occurrence submodel) | 0.827 | Strong discriminative ability between dry and wet days |
| Classification accuracy | 75.28% | At the Youden-optimal threshold of 0.6314 |
| Mean absolute error | 2.736 mm | Average absolute prediction error across all days |
| DHARMa dispersion test | $p = 0.36$ | No evidence of over- or under-dispersion |
| Zero-inflation calibration | Ratio = 1.00, $p = 0.976$ | Model generates exactly the correct proportion of dry days |
| Durbin-Watson statistic | 2.0533, $p = 0.1197$ | No residual temporal autocorrelation |

---

## Repository Structure

```
.
├── index.qmd                  # Landing page and executive overview
├── 01-intro.qmd               # Statistical problem and modelling framework
├── 02-data-prep.qmd           # Hybrid imputation pipeline
├── 03-eda.qmd                 # Exploratory analysis and feature motivation
├── 04-features.qmd            # Feature engineering pipeline
├── 05-modeling.qmd            # Progressive model construction
├── 06-evaluation.qmd          # Model validation and diagnostics
├── 07-model-selection.qmd     # Distributional comparison and formal selection
├── 08-conclusion.qmd          # Findings, limitations, and recommendations
├── _quarto.yml                # Site configuration
├── utils.R                    # Shared utility functions
├── data/
│   ├── weatherAUS.csv         # Source data
│   ├── df_clean.rds           # Cleaned dataset
│   ├── df_final.csv           # Feature-engineered dataset
│   ├── df_engineered.csv      # Post-pipeline dataset with all derived features
│   └── df_scaled.csv          # Z-score standardised dataset for modelling
├── models/
│   └── all_models_bundle.RData  # Serialised glmmTMB objects (M0 through M6)
└── docs/                      # Rendered HTML report (GitHub Pages)
```

---

## Reproduction

**Requirements:** R 4.5.x, Quarto CLI 1.5+

**Install dependencies:**

```r
librarian::shelf(
  tidyverse, glmmTMB, DHARMa, missRanger,
  performance, pROC, zoo, ggridges, rstatix,
  moments, cocor, aod, multcompView, sjPlot,
  gtsummary, lmtest, splines, kableExtra
)
```

**Render the report:**

```bash
git clone https://github.com/Chrisolande/australian-rainfall-dynamics.git
cd australian-rainfall-dynamics
quarto render index.qmd
```

Pre-fitted model objects are bundled in `models/all_models_bundle.RData`. The report renders without re-running the `glmmTMB` optimisations, which involve high-dimensional mixed-effects likelihoods and are computationally intensive.

---

## Scope and Limitations

**Suitable applications:** Short-range probabilistic forecasting (1 to 7 days), regional climate characterisation, agricultural drought planning, and historical gap-filling for non-extreme observations.

**Not suitable for:** Extreme value analysis. The Gamma distribution has exponentially declining tails and systematically underestimates the probability and magnitude of rare catastrophic events (exceedances above approximately the 95th percentile). Flood risk and infrastructure design applications require Extreme Value Theory methods, specifically a Generalised Pareto Distribution fitted to threshold exceedances.

---

## Author

**Chris Olande** | Statistician and Programmer | Nairobi, Kenya

---

## Citation

Olande, C. (2026). *Australian Rainfall Dynamics: A Zero-Inflated Gamma Mixed-Effects Model for Spatiotemporal Precipitation*. GitHub. https://github.com/Chrisolande/australian-rainfall-dynamics

**License:** MIT
