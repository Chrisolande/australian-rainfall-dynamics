# %%
source(here::here("config.R"))
source(here::here("chapter5", "feature_eng.R"))
source(here::here("utils.R"))
source(here::here("chapter5", "modeling.R"))
source(here::here("chapter5", "mitml_wrappers.R"))
# %%
df_final <- read_csv(here::here("data", "df_final.csv"))
imp_mids <- readRDS("data/imp_mids.rds")

# %%
engineered_list <- complete(imp_mids, action = "all") %>% map(engineer_features)
# %%
m1_moisture <- fit_and_pool_v3(
  cond_formula = rainfall ~ 1 +
    humidity3pm +
    dewpoint_9am +
    dewpoint_change +
    pressure_change,
  zi_formula = ~ humidity3pm + dewpoint_9am,
  datasets = engineered_list
)


# %%
m2_temporal <- fit_and_pool_v3(
  cond_formula = rainfall ~ 1 +
    humidity3pm +
    dewpoint_9am +
    dewpoint_change +
    pressure_change +
    day_cos +
    day_sin,
  zi_formula = ~ humidity3pm +
    dewpoint_9am +
    rain_yesterday +
    cloud_development +
    pressure_change,
  datasets = engineered_list
)


# %%
comp_1v2 <- compare_models(
  m_large = m2_temporal,
  m_small = m1_moisture,
  names = c("m2_temporal", "m1_moisture")
)
