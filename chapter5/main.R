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
