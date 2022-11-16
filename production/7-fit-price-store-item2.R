
# notes ------------------------------------------------------------------------

# calculating price coefficients for each item
# this takes ~20 minutes

# setup ------------------------------------------------------------------------

library(feather)
library(Hmisc)
library(tidyverse)
library(broom)
library(progress)
source("production/0-helper-funs.R")
options(stringsAsFactors = F, digits = 3, mc.cores = 3)

# loading data
cal <- read_feather("data/data-calendar-clean.feather")
prices <- read_feather("data/data-price-clean.feather")
load("predictions/preds-global.RData")
load("predictions/preds-time-month-wday-snap-event.RData")
load("predictions/preds-price-item.RData")
train_raw <- read_feather("data/data-train-wide.feather")
train_raw %>% select(1:20) %>% glimpse()

# setting up the baseline time predictions
preds_base <- bind_cols(
  select(preds_time_month_wday_snap_event, ends_with("id")), 
  select(preds_time_month_wday_snap_event, -ends_with("id")) + 
    select(preds_item_price, -ends_with("id"))
)
rm(preds_time_month_wday_snap_event, preds_item_price)
gc()

# fitting by item -------------------------------------------------------------

store_item_price_coefs <- tibble(data.frame())
pb <- progress_bar$new(total = nrow(train_raw))
for(i in 1:nrow(train_raw)) {
  pb$tick()
  
  store_i <- train_raw$store_id[i]
  item_i <- train_raw$item_id[i]
  
  dat_i <- train_raw %>% filter(store_id == store_i, item_id == item_i)
  off_dat_i <- preds_base %>% filter(store_id == store_i, item_id == item_i)
  prices_i <- prices %>% 
    filter(store_id == store_i, item_id == item_i) %>% 
    pivot_longer(starts_with("X"), 
                 names_to = "day", 
                 names_prefix = "X", 
                 names_transform = list(day = as.integer), 
                 values_to = "price_z", 
                 values_drop_na = T)
  
  # fitting the models
  mod_data_i <- prep_time_data(dat_i, 
                               off_data = off_dat_i, 
                               off_data_non0_prefix = "lpred_non0_base_", 
                               off_data_sales_prefix = "lpred_sales_base_") %>% 
    inner_join(prices_i, by = c("id", "store_id", "item_id", "day"))
  # non0 model
  non0_mod_data <- mod_data_i %>% 
    filter(between(lp_non0_base, -7, 7)) %>% 
    filter(sum(sales == 0) > 0, 
           sum(sales != 0) > 0) %>% 
    filter(n_distinct(price_z) > 1)
  cf_non0 <- if(nrow(non0_mod_data) > 2) {
    m_non0_time_i <- glm(I(sales != 0) ~ price_z, 
                         non0_mod_data, 
                         family = binomial(), 
                         offset = lp_non0_base)
    tidy(m_non0_time_i)
  } else {
    data.frame(term = character(), 
               estimate = numeric(), 
               std.error = numeric(), 
               statistic = numeric(), 
               p.value = numeric())
  }
  # sales modes
  sales_mod_data <- mod_data_i %>% 
    filter(sales > 0, 
           between(lp_sales_base, -7, 7)) %>% 
    filter(n_distinct(price_z) > 1)
  cf_sales <- if(nrow(sales_mod_data) > 2) {
    m_sales_time_i <- glm(sales ~ price_z, 
                          sales_mod_data, 
                          family = poisson(), 
                          offset = lp_sales_base)
    tidy(m_sales_time_i)
  } else {
    data.frame(term = character(), 
               estimate = numeric(), 
               std.error = numeric(), 
               statistic = numeric(), 
               p.value = numeric())
  }
  
  # combining the results into a nice dataset
  results_i <- cf_non0 %>% 
    left_join(cf_sales, by = "term", suffix = c("_non0", "_sales")) %>% 
    mutate(item_id = item_i, 
           store_id = store_i)
  
  
  store_item_price_coefs <- bind_rows(store_item_price_coefs, results_i)
  
  # manual memory cleaning is annoying...
  rm(dat_i, off_dat_i, non0_mod_data, sales_mod_data)
  gc()
}

# summary of results
store_item_price_coefs %>% 
  filter(between(estimate_non0, -1e5, 1e5)) %>% 
  group_by(term) %>% 
  summarise(mu_non0 = weighted.mean(estimate_non0, 1 / std.error_non0^2, na.rm = T), 
            sd_between_non0 = sqrt(wtd.var(estimate_non0, 1 / std.error_non0^2)), 
            sd_within_non0 = sqrt(mean(std.error_non0^2, na.rm = T)), 
            sd_within_non0_med = median(std.error_non0, na.rm = T), 
            mu_sales = weighted.mean(estimate_sales, 1 / std.error_sales^2, na.rm = T), 
            sd_between_sales = sqrt(wtd.var(estimate_sales, 1 / std.error_sales^2)), 
            sd_within_sales = sqrt(mean(std.error_sales^2, na.rm = T)), 
            sd_within_sales_med = median(std.error_sales, na.rm = T)) %>% 
  mutate(across(where(is.numeric), round, digits = 2))

# applying RTTM to coefficients
store_item_price_coefs_regr <- train_raw %>% 
  select(id, item_id, store_id) %>% 
  full_join(filter(store_item_price_coefs, term == "price_z"), by = c("item_id", "store_id")) %>% 
  mutate(term = coalesce(term, "price_z")) %>% 
  # fixing a couple wonky results
  mutate(estimate_non0 = ifelse(between(estimate_non0, -1e5, 1e5), estimate_non0, 0)) %>% 
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
  select(store_id, item_id, term, est_regr_non0, est_regr_sales) %>% 
  pivot_wider(names_from = term, values_from = c(est_regr_non0, est_regr_sales)) %>% 
  rename_with(str_replace_all, pattern = "est_regr_", replacement = "") %>% 
  mutate(across(where(is.numeric), round, digits = 4))

# generating predictions -------------------------------------------------------


# real mediocre code here
store_item_price_preds_non0 <- matrix(NA, nrow = 30490, ncol = 1969)
store_item_price_preds_sales <- matrix(NA, nrow = 30490, ncol = 1969)
prices_num <- prices %>% select(-ends_with("id"))
for(j in 1:ncol(store_item_price_preds_non0)) {
  store_item_price_preds_non0[, j] <- coalesce(prices_num[[j]] * store_item_price_coefs_regr$non0_price_z, 0)
  store_item_price_preds_sales[, j] <- coalesce(prices_num[[j]] * store_item_price_coefs_regr$sales_price_z, 0)
}

# combining into a clean dataframe
preds_store_item_price <- train_raw %>% 
  select(id, item_id, store_id) %>%  
  bind_cols(rename_with(data.frame(store_item_price_preds_non0), ~ paste0("lpred_non0_item_price_", .))) %>%  
  bind_cols(rename_with(data.frame(store_item_price_preds_sales), ~ paste0("lpred_sales_item_price_", .))) %>% 
  rename_with(~str_replace_all(., "X", ""))

# saving -----------------------------------------------------------------------

# saving the full price effect
load("predictions/preds-price-item.RData")

preds_price <- bind_cols(
  select(train_raw, 1:3), 
    select(preds_item_price, starts_with("lpred")) + 
    select(preds_store_item_price, starts_with("lpred"))
) %>% 
  rename_with(str_replace_all, pattern = "_store_item_", replacement = "_") %>% 
  rename_with(str_replace_all, pattern = "_store_", replacement = "_") %>% 
  rename_with(str_replace_all, pattern = "_item_", replacement = "_")

save(store_item_price_coefs_regr, file = "fitted-models/models-price-store-item.RData")
save(preds_store_item_price, file = "predictions/preds-price-store-item.RData")
save(preds_price, file = "predictions/preds-price-all.RData")


