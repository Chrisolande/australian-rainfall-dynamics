# # %%
source(here::here("config.R"))
source(here::here("utils.R"))
source(here::here("chapter5", "feature_engineering.R"))
source(here::here("chapter6", "modeling_core.R"))
source(here::here("chapter6", "modeling_pool.R"))
source(here::here("chapter6", "modeling_runners.R"))
source(here::here("chapter6", "mitml_wrappers.R"))
source(here::here("chapter6", "Reporting.R"))

# # %%
df_final <- read_csv(here::here("data", "df_final.csv"))
# m5_wind <- readRDS(here::here("models", "m5_wind.rds"))
# m4_energy <- readRDS(here::here("models", "m4_energy.rds"))
imp_mids <- readRDS("data/imp_mids.rds")

# %%
engineered_list <- complete(imp_mids, action = "all") %>% map(engineer_features)

# %%
model_configs <- list(
  list(name = "m1", cond = m1_cond, zi = zi_m1),
  list(name = "m2", cond = m2_cond, zi = zi_m2),
  list(name = "m3", cond = m3_cond, zi = zi_m3_to_m5),
  list(name = "m4", cond = m4_cond, zi = zi_m3_to_m5),
  list(name = "m5", cond = m5_cond, zi = zi_m3_to_m5)
)

control <- glmmTMB::glmmTMBControl(
  optimizer = nlminb,
  optCtrl = list(iter.max = 1200, eval.max = 1500, rel.tol = 1e-8)
)

for (cfg in model_configs) {
  cat(sprintf("Fitting %s...\n", cfg$name))

  fit <- fit_and_pool(
    cond_formula = cfg$cond,
    zi_formula = cfg$zi,
    datasets = engineered_list,
    control = control,
    dispformula = ~1,
    preflight_n = 0,
    fail_fast = TRUE,
    parallel = TRUE,
    workers = 4
  )

  saveRDS(fit, here::here("models", sprintf("%s.rds", cfg$name)))
  cat(sprintf("Saved %s.rds\n", cfg$name))

  rm(fit)
  gc()
}

# %%
m6_mixed <- fit_and_pool(
  cond_formula = m6_cond,
  zi_formula = zi_m6,
  datasets = engineered_list,
  control = glmmTMB::glmmTMBControl(
    optimizer = nlminb,
    optCtrl = list(iter.max = 1200, eval.max = 1500, rel.tol = 1e-8)
  ),
  dispformula = dispformula,
  preflight_n = 0,
  fail_fast = TRUE,
  parallel = TRUE,
  workers = 4
)
