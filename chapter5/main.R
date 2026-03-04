# %%
source(here::here("config.R"))
source(here::here("utils.R"))
source(here::here("chapter5", "feature_eng.R"))
source(here::here("chapter5", "modeling_core.R"))
source(here::here("chapter5", "modeling_pool.R"))
source(here::here("chapter5", "modeling_runners.R"))
source(here::here("chapter5", "mitml_wrappers.R"))
source(here::here("chapter5", "Reporting.R"))

# %%
df_final <- read_csv(here::here("data", "df_final.csv"))
# m5_wind <- readRDS(here::here("models", "m5_wind.rds"))
# m4_energy <- readRDS(here::here("models", "m4_energy.rds"))
imp_mids <- readRDS("data/imp_mids.rds")

# %%
engineered_list <- complete(imp_mids, action = "all") %>% map(engineer_features)


# %%
m0_null <- fit_and_pool(
  cond_formula = rainfall ~ 1,
  zi_formula = ~1,
  datasets = engineered_list,
  workers = 1L
)


saveRDS(m0_null, here::here("models", "m0_null.rds"))


# %%
m1_moisture <- fit_and_pool(
  cond_formula = m1_moisture,
  zi_formula = zi_m1,
  datasets = engineered_list,
  control = glmmTMB::glmmTMBControl(
    optimizer = nlminb,
    optCtrl = list(iter.max = 1200, eval.max = 1500, rel.tol = 1e-8)
  ),
  preflight_n = 0,
  fail_fast = TRUE,
  parallel = TRUE,
  workers = 4
)
saveRDS(m1_moisture, here::here("models", "m1_moisture.rds"))

# %%
m2_temporal <- fit_and_pool(
  cond_formula = m2_cond,
  zi_formula = zi_m2,
  datasets = engineered_list,
  control = glmmTMB::glmmTMBControl(
    optimizer = nlminb,
    optCtrl = list(iter.max = 1200, eval.max = 1500, rel.tol = 1e-8)
  ),
  preflight_n = 0,
  fail_fast = TRUE,
  parallel = TRUE,
  workers = 4
)

saveRDS(m2_temporal, here::here("models", "m2_temporal.rds"))

# %%
m2_history <- fit_and_pool(
  cond_formula = m3_cond,
  zi_formula = zi_m3_to_m5,
  datasets = engineered_list,
  control = glmmTMB::glmmTMBControl(
    optimizer = nlminb,
    optCtrl = list(iter.max = 1200, eval.max = 1500, rel.tol = 1e-8)
  ),
  preflight_n = 0,
  fail_fast = TRUE,
  parallel = TRUE,
  workers = 4
)
saveRDS(m3_history, here::here("models", "m3_history.rds"))

# %%
m4_energy <- fit_and_pool(
  cond_formula = m4_cond,
  zi_formula = zi_m3_to_m5,
  datasets = engineered_list,
  control = glmmTMB::glmmTMBControl(
    optimizer = nlminb,
    optCtrl = list(iter.max = 1200, eval.max = 1500, rel.tol = 1e-8)
  ),
  preflight_n = 0,
  fail_fast = TRUE,
  parallel = TRUE,
  workers = 4
)

saveRDS(m4_energy, here::here("models", "m4_energy.rds"))

# %%
m5_wind <- fit_and_pool(
  cond_formula = m5_cond,
  zi_formula = zi_m3_to_m5,
  datasets = engineered_list,
  control = glmmTMB::glmmTMBControl(
    optimizer = nlminb,
    optCtrl = list(iter.max = 1200, eval.max = 1500, rel.tol = 1e-8)
  ),
  preflight_n = 0,
  fail_fast = TRUE,
  parallel = TRUE,
  workers = 4
)

saveRDS(m5_wind, here::here("models", "m5_wind.rds"))


# %%
m6_mixed <- fit_and_pool(
  cond_formula = m6_cond,
  zi_formula = zi_m6,
  datasets = engineered_list,
  control = glmmTMB::glmmTMBControl(
    optimizer = nlminb,
    optCtrl = list(iter.max = 1200, eval.max = 1500, rel.tol = 1e-8)
  ),
  preflight_n = 0,
  fail_fast = TRUE,
  parallel = TRUE,
  workers = 4
)

