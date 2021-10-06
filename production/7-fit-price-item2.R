
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
options(stringsAsFactors = F, digits = 3, mc.cores = 4)

# loading data
cal <- read_feather("data/data-calendar-clean.feather")
prices <- read_feather("data/data-price-clean.feather")
load("predictions/preds-global.RData")
load("predictions/preds-time-month-wday-snap.RData")
train_raw <- read_feather("data/data-train-wide.feather")
train_raw %>% select(1:20) %>% glimpse()


# fitting by item -------------------------------------------------------------

item_price_coefs <- tibble(data.frame())
pb <- progress_bar$new(total = length(unique(train_raw$item_id)))
for(i in unique(train_raw$item_id)) {
  pb$tick()
  
  dat_i <- train_raw %>% filter(item_id == i)
  off_dat_i <- preds_time_month_wday_snap %>% filter(item_id == i)
  prices_i <- prices %>% 
    filter(item_id == i) %>% 
    pivot_longer(starts_with("X"), 
                 names_to = "day", 
                 names_prefix = "X", 
                 names_transform = list(day = as.integer), 
                 values_to = "price_z", 
                 values_drop_na = T)
  
  # fitting the models
  time_mod_data_i <- prep_time_data(dat_i, 
                                    off_data = off_dat_i, 
                                    off_data_non0_prefix = "lpred_non0_base_", 
                                    off_data_sales_prefix = "lpred_sales_base_") %>% 
    inner_join(prices_i, by = c("id", "store_id", "item_id", "day"))
  m_non0_time_i <- time_mod_data_i %>% 
    filter(between(lp_non0_base, -7, 7)) %>% 
    filter(sum(sales == 0) > 1, 
           sum(sales != 0) > 1) %>% 
    glm(I(sales != 0) ~ price_z, 
        data = ., 
        family = binomial(), 
        offset = lp_non0_base)
  m_sales_time_i <- time_mod_data_i %>% 
    filter(sales > 0, between(lp_sales_base, -8, 8)) %>% 
    filter(n() >= 2) %>% 
    glm(sales ~ price_z, 
        data = ., 
        family = poisson(), 
        offset = lp_sales_base)
  
  # combining the results into a nice dataset
  results_i <- tidy(m_non0_time_i) %>% 
    left_join(tidy(m_sales_time_i), by = "term", suffix = c("_non0", "_sales")) %>% 
    mutate(item_id = i)
  
  item_price_coefs <- bind_rows(item_price_coefs, results_i)
  
  # manual memory cleaning is annoying...
  rm(dat_i, off_dat_i, non0_mod_data, sales_mod_data, m_non0_time_i, m_sales_time_i)
  gc()
}

# summary of results
item_price_coefs %>% 
  group_by(term) %>% 
  summarise(mu_non0 = weighted.mean(estimate_non0, 1 / std.error_non0^2, na.rm = T), 
            sd_between_non0 = sqrt(wtd.var(estimate_non0, 1 / std.error_non0^2)), 
            sd_within_non0 = sqrt(mean(std.error_non0^2, na.rm = T)), 
            mu_sales = weighted.mean(estimate_sales, 1 / std.error_sales^2, na.rm = T), 
            sd_between_sales = sqrt(wtd.var(estimate_sales, 1 / std.error_sales^2)), 
            sd_within_sales = sqrt(mean(std.error_sales^2, na.rm = T))) %>% 
  mutate(across(where(is.numeric), round, digits = 2))

# applying RTTM to coefficients
item_price_coefs_regr <- train_raw %>% 
  # annoying data manipulation to ensure missing coefs get a row with 0 effect
  select(item_id) %>%  distinct() %>% 
  full_join(expand_grid(item_id = unique(train_raw$item_id), term = unique(item_price_coefs$term)), by = "item_id") %>% 
  left_join(item_price_coefs, by = c("item_id", "term")) %>% 
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
  select(item_id, term, est_regr_non0, est_regr_sales) %>% 
  pivot_wider(names_from = term, values_from = c(est_regr_non0, est_regr_sales)) %>% 
  rename_with(str_replace_all, pattern = "est_regr_", replacement = "") %>% 
  mutate(across(where(is.numeric), round, digits = 4))

# generating predictions -------------------------------------------------------


# real mediocre code here
item_price_coefs_full <- train_raw %>% 
  select(id, item_id, store_id) %>% 
  left_join(item_price_coefs_regr, by = c("item_id")) 
item_price_preds_non0 <- matrix(NA, nrow = 30490, ncol = 1969)
item_price_preds_sales <- matrix(NA, nrow = 30490, ncol = 1969)
prices_num <- prices %>% select(-ends_with("id"))
for(j in 1:ncol(item_price_preds_non0)) {
  item_price_preds_non0[, j] <- coalesce(prices_num[[j]] * item_price_coefs_full$non0_price_z, 0)
  item_price_preds_sales[, j] <- coalesce(prices_num[[j]] * item_price_coefs_full$sales_price_z, 0)
}

# combining into a clean dataframe
preds_item_price <- train_raw %>% 
  select(id, item_id, store_id) %>%  
  bind_cols(rename_with(data.frame(item_price_preds_non0), ~ paste0("lpred_non0_item_price_", .))) %>%  
  bind_cols(rename_with(data.frame(item_price_preds_sales), ~ paste0("lpred_sales_item_price_", .))) %>% 
  rename_with(~str_replace_all(., "X", ""))

# saving -----------------------------------------------------------------------

save(item_price_coefs_regr, file = "fitted-models/models-price-item.RData")
save(preds_item_price, file = "predictions/preds-price-item.RData")


