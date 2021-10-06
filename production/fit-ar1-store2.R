
# notes ------------------------------------------------------------------------

# calculating time trend coefficients for each store
# this takes ~20 minutes

# setup ------------------------------------------------------------------------

library(feather)
library(Hmisc)
library(tidyverse)
library(broom)
library(progress)
source("production/helper-funs.R")
options(stringsAsFactors = F, digits = 3, mc.cores = 4)

# loading data
cal <- read_feather("data/data-calendar-clean.feather")
train_raw <- read_feather("data/data-train-wide.feather")
load("predictions/preds-global.RData")
load("predictions/preds-time-all.RData")
load("predictions/preds-month-all.RData")
load("predictions/preds-weekday-all.RData")
load("predictions/preds-snap-all.RData")
train_raw %>% select(1:20) %>% glimpse()

# setting up the baseline time predictions
preds_base <- bind_cols(
  select(preds_time, ends_with("id")), 
  select(preds_time, -ends_with("id")) + 
    select(preds_month, -ends_with("id")) + 
    select(preds_weekday, -ends_with("id")) + 
    select(preds_snap, -ends_with("id"))
)
rm(preds_time, preds_month, preds_weekday, preds_snap)
gc()

# fitting by store -------------------------------------------------------------

store_ar1_coefs <- tibble(data.frame())
pb <- progress_bar$new(total = length(unique(train_raw$store_id)))
for(i in unique(train_raw$store_id)) {
  pb$tick()
  
  dat_i <- train_raw %>% filter(store_id == i)
  off_dat_i <- preds_base %>% filter(store_id == i)
  
  # fitting the models
  mod_data_i <- prep_time_data(dat_i, 
                                    off_data = off_dat_i, 
                                    off_data_non0_prefix = "lpred_non0_time_", 
                                    off_data_sales_prefix = "lpred_sales_time_") %>% 
    group_by(store_id, item_id) %>% 
    mutate(sales_l1 = lag(sales, order_by = day), 
           lp_non0_l1 = lag(lp_non0_base, order_by = day), 
           lp_sales_l1 = lag(lp_sales_base, order_by = day)) %>% 
    ungroup() %>% 
    filter(!is.na(sales_l1))
  m_non0_i <- mod_data_i %>% 
    filter(between(lp_non0_base, -7, 7)) %>% 
    glm(I(sales != 0) ~ I(sales_l1 != 0) * lp_non0_l1, 
        data = ., 
        offset = lp_non0_base)
  m_sales_i <- mod_data_i %>% 
    filter(sales > 0, sales_l1 > 0, between(lp_sales_base, -8, 8)) %>% 
    glm(sales ~ I(log(sales_l1) - lp_sales_l1), 
        data = .,
        family = poisson(), 
        offset = lp_sales_base)
  
  # combining the results into a nice dataset
  results_i <- tidy(m_non0_i) %>% 
    left_join(tidy(m_sales_i), by = "term", suffix = c("_non0", "_sales")) %>% 
    mutate(store_id = i)
  
  store_ar1_coefs <- bind_rows(store_ar1_coefs, results_i)
}

# summary of results
store_ar1_coefs %>% 
  group_by(term) %>% 
  summarise(mu_non0 = weighted.mean(estimate_non0, 1 / std.error_non0^2, na.rm = T), 
            sd_between_non0 = sqrt(wtd.var(estimate_non0, 1 / std.error_non0^2)), 
            sd_within_non0 = sqrt(mean(std.error_non0^2, na.rm = T)), 
            mu_sales = weighted.mean(estimate_sales, 1 / std.error_sales^2, na.rm = T), 
            sd_between_sales = sqrt(wtd.var(estimate_sales, 1 / std.error_sales^2)), 
            sd_within_sales = sqrt(mean(std.error_sales^2, na.rm = T))) %>% 
  mutate(across(where(is.numeric), round, digits = 2))

# applying RTTM to coefficients
store_ar1_coefs_regr <- train_raw %>% 
  # annoying data manipulation to ensure missing coefs get a row with 0 effect
  select(store_id) %>%  distinct() %>% 
  full_join(expand_grid(store_id = unique(train_raw$store_id), term = unique(store_ar1_coefs$term)), by = "store_id") %>% 
  left_join(store_ar1_coefs, by = c("store_id", "term")) %>% 
  # applying RTTM
  group_by(term) %>% 
  mutate(mu_non0 = weighted.mean(estimate_non0, 1 / std.error_non0^2, na.rm = T), 
         sd_between_non0 = sqrt(wtd.var(estimate_non0, 1 / std.error_non0^2)), 
         mu_sales = weighted.mean(estimate_sales, 1 / std.error_sales^2, na.rm = T), 
         sd_between_sales = sqrt(wtd.var(estimate_sales, 1 / std.error_sales^2))) %>% 
  mutate(est_regr_non0 = coalesce(apply_rttm(estimate_non0, std.error_non0, sd_between_non0, mu_non0), 0), 
         est_regr_sales = coalesce(apply_rttm(estimate_sales, std.error_sales, sd_between_sales, mu_sales), 0)) %>% 
  ungroup() %>% 
  # organizing into nice dataset
  select(store_id, term, est_regr_non0, est_regr_sales) %>% 
  pivot_wider(names_from = term, values_from = c(est_regr_non0, est_regr_sales)) %>% 
  rename_with(str_replace_all, pattern = "est_regr_", replacement = "") %>% 
  mutate(across(where(is.numeric), round, digits = 4))

# generating predictions -------------------------------------------------------

# checks -----------------------------------------------------------------------

# saving -----------------------------------------------------------------------

save(store_ar1_coefs_regr, file = "fitted-models/models-ar1-store.RData")
save(preds_store_ar1, file = "predictions/preds-ar1-store.RData")


