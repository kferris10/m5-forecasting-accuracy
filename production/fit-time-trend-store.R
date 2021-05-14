
# notes ------------------------------------------------------------------------

# calculating time trend coefficients for each store
# this takes < 10 minutes, requires at least 3 cores + all my memory

# setup ------------------------------------------------------------------------

library(feather)
library(Hmisc)
library(tidyverse)
library(multidplyr) # note that this package is experimental
source("production/helper-funs.R")
options(stringsAsFactors = F, digits = 3, mc.cores = 4)

# model specifications I'll be using
f_time <- formula(~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10)

# loading data
load("predictions/preds-global.RData")
train_raw <- read_feather("data/data-train-wide.feather")
train_raw %>% select(1:20) %>% glimpse()

# grouping by store for dplyr
train_by_store <- train_raw %>%
  mutate(store_id2 = store_id) %>%
  group_by(store_id = store_id2) %>%
  nest() 


# fitting by store -------------------------------------------------------------


# setting up the cluster (for parallelization)
cluster <- new_cluster(getOption("mc.cores"))
cluster_library(cluster, c("dplyr", "tidyr", "purrr"))
cluster_assign(cluster, 
               prep_time_data = prep_time_data, 
               calc_glm_coefs = calc_glm_coefs, 
               pp_global = pp_global, 
               preds_global = preds_global, 
               extract_se = extract_se, 
               f_time = f_time)

# setting up the data for analysis
store_dat <- train_by_store %>% 
  # ungroup() %>% slice(1:4) %>% group_by(store_id) %>% 
  partition(cluster) %>% 
  mutate(time_mod_data = map(data, prep_time_data))
gc()

# fitting models and extracting coefficients
store_coefs <- store_dat %>% 
  mutate(
    m_non0_time = map(time_mod_data, calc_glm_coefs, offset = lp_non0_base, 
                      f = update(f_time, I(sales != 0) ~ .), is_pois = F), 
    m_sales_time = map(time_mod_data, calc_glm_coefs, offset = lp_sales_base, 
                       f = update(f_time, sales ~ .), is_pois = T)
  )  %>% 
  collect() %>% 
  mutate(coefs_all = map2(m_non0_time, m_sales_time, full_join, by = "term", suffix = c("_non0", "_sales"))) %>% 
  ungroup() %>% 
  select(store_id, coefs_all) %>% 
  unnest(coefs_all)

# applying RTTM to coefficients
store_coefs_regr <- store_coefs %>% 
  group_by(term) %>% 
  mutate(sd_between_non0 = sqrt(wtd.var(est_raw_non0, 1 / se_non0^2)), 
         sd_between_sales = sqrt(wtd.var(est_raw_sales, 1 / se_sales^2)), 
         est_regr_non0 = apply_rttm(est_raw_non0, se_non0, sd_between_non0), 
         est_regr_sales = apply_rttm(est_raw_sales, se_sales, sd_between_sales)) %>% 
  ungroup() %>% 
  select(store_id, term, est_regr_non0, est_regr_sales) %>% 
  pivot_wider(names_from = term, values_from = c(est_regr_non0, est_regr_sales)) %>% 
  rename_with(str_replace_all, pattern = "est_regr_", replacement = "") %>% 
  mutate(across(where(is.numeric), round, digits = 4))

# generating predictions -------------------------------------------------------

store_coefs_nested <- store_coefs_regr %>% 
  group_by(store_id) %>% 
  nest(non0 = starts_with("non0_"), sales = starts_with("sales_"))
store_preds <- expand_grid(store_id = unique(train_raw$store_id), day = 1:1969) %>% 
  left_join(pp_global, by = "day") %>% 
  group_by(store_id) %>% 
  nest(day = day, pp = starts_with("X")) %>% 
  left_join(store_coefs_nested, by = "store_id") %>% 
  mutate(
    lpred_non0_store = map2(pp, non0, ~ as.numeric(cbind(1, as.matrix(.x)) %*% as.numeric(.y))), 
    lpred_sales_store = map2(pp, sales, ~ as.numeric(cbind(1, as.matrix(.x)) %*% as.numeric(.y)))
  ) %>% 
  select(store_id, day, lpred_non0_store, lpred_sales_store) %>% 
  unnest(c(day, lpred_non0_store, lpred_sales_store)) %>% 
  pivot_wider(names_from = day, values_from = c(lpred_non0_store, lpred_sales_store))
preds_store_time <- train_raw %>% 
  select(id, item_id, store_id) %>% 
  left_join(store_preds, by = c("store_id"))

# checks -----------------------------------------------------------------------

preds_store_time %>% 
  filter(item_id == "FOODS_3_262") %>% 
  pivot_longer(starts_with("lpred_"), names_prefix = "lpred_", names_to = "day", values_to = "sales")  %>% 
  mutate(type = ifelse(substr(day, 1, 4) == "non0", "non0", "sales"), 
         day = str_replace_all(day, "non0_store_", ""), 
         day = str_replace_all(day, "sales_store_", ""), 
         day = as.numeric(day)) %>% 
  pivot_wider(names_from = type, values_from = sales) %>% 
  left_join(preds_global, by = c("day")) %>% 
  mutate(pred_sales = arm::invlogit(-non0 + lp_non0_base) * exp(sales + lp_sales_base)) %>% 
  sample_frac(.2) %>%
  qplot(day, pred_sales, data = ., colour = store_id, geom = "line") + 
  geom_vline(aes(xintercept = 1941)) + 
  ylim(c(0, 5))

# saving -----------------------------------------------------------------------

save(store_coefs_regr, file = "fitted-models/models-time-store.RData")
save(preds_store_time, file = "predictions/preds-time-store.RData")


