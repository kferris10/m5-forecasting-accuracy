
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
load("predictions/preds-global.RData")
load("predictions/preds-time-all.RData")
load("predictions/preds-month-all.RData")
load("predictions/preds-weekday-all.RData")
train_raw <- read_feather("data/data-train-wide.feather")
train_raw %>% select(1:20) %>% glimpse()

# setting up the baseline time predictions
preds_base <- bind_cols(
  select(preds_time, ends_with("id")), 
  select(preds_time, -ends_with("id")) + 
    select(preds_month, -ends_with("id")) + 
    select(preds_weekday, -ends_with("id"))
)

# fitting by store -------------------------------------------------------------

store_snap_coefs <- tibble(data.frame())
pb <- progress_bar$new(total = length(unique(train_raw$store_id)))
for(i in unique(train_raw$store_id)) {
  pb$tick()
  
  dat_i <- train_raw %>% filter(store_id == i)
  off_dat_i <- preds_time %>% filter(store_id == i)
  
  # fitting the models
  time_mod_data_i <- prep_time_data(dat_i, 
                                    off_data = off_dat_i, 
                                    off_data_non0_prefix = "lpred_non0_time_", 
                                    off_data_sales_prefix = "lpred_sales_time_")
  m_non0_time_i <- time_mod_data_i %>% 
    filter(between(lp_non0_base, -7, 7)) %>% 
    glm(update(f_snap, I(sales != 0) ~ .), 
        data = ., 
        family = binomial(), 
        offset = lp_non0_base)
  m_sales_time_i <- time_mod_data_i %>% 
    filter(sales > 0, between(lp_sales_base, -8, 8)) %>% 
    glm(update(f_snap, sales ~ .), 
                        data = ., 
                        family = poisson(), 
                        offset = lp_sales_base)
  
  # combining the results into a nice dataset
  results_i <- tidy(m_non0_time_i) %>% 
    left_join(tidy(m_sales_time_i), by = "term", suffix = c("_non0", "_sales")) %>% 
    mutate(store_id = i)
  
  store_snap_coefs <- bind_rows(store_snap_coefs, results_i)
}

# summary of results
store_snap_coefs %>% 
  group_by(term) %>% 
  summarise(mu_non0 = weighted.mean(estimate_non0, 1 / std.error_non0^2, na.rm = T), 
            sd_between_non0 = sqrt(wtd.var(estimate_non0, 1 / std.error_non0^2)), 
            sd_within_non0 = sqrt(mean(std.error_non0^2, na.rm = T)), 
            mu_sales = weighted.mean(estimate_sales, 1 / std.error_sales^2, na.rm = T), 
            sd_between_sales = sqrt(wtd.var(estimate_sales, 1 / std.error_sales^2)), 
            sd_within_sales = sqrt(mean(std.error_sales^2, na.rm = T))) %>% 
  mutate(across(where(is.numeric), round, digits = 2))

# applying RTTM to coefficients
store_snap_coefs_regr <- train_raw %>% 
  # annoying data manipulation to ensure missing coefs get a row with 0 effect
  select(store_id) %>%  distinct() %>% 
  full_join(expand_grid(store_id = unique(train_raw$store_id), term = unique(store_snap_coefs$term)), by = "store_id") %>% 
  left_join(store_snap_coefs, by = c("store_id", "term")) %>% 
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

state_snaps <- cal %>%
  select(day, snap_CA, snap_TX, snap_WI) %>% 
  pivot_longer(-day, names_to = "state", names_prefix = "snap_", values_to = "is_snap") 
store_snap_preds <- store_snap_coefs_regr %>% 
  pivot_longer(cols = -store_id, 
               names_to = c(".value", "is_snap"), 
               names_sep = "_factor\\(is_snap\\)", 
               names_transform = list(is_snap = as.integer)) %>% 
  mutate(state = substr(store_id, 1, 2)) %>% 
  left_join(state_snaps, by = c("state", "is_snap")) %>% 
  arrange(store_id, day) %>% 
  select(store_id, day, non0, sales) %>% 
  rename(lpred_non0_store_snap = non0, 
         lpred_sales_store_snap = sales) %>% 
  pivot_wider(names_from = day, values_from = c(lpred_non0_store_snap, lpred_sales_store_snap))
preds_store_snap <- train_raw %>% 
  select(id, item_id, store_id) %>%  
  left_join(store_snap_preds, by = c("store_id"))

# checks -----------------------------------------------------------------------

# for the most part the is_snap effects lead to higher sales
store_snap_coefs_regr

# saving -----------------------------------------------------------------------

save(store_snap_coefs_regr, file = "fitted-models/models-snap-store.RData")
save(preds_store_snap, file = "predictions/preds-snap-store.RData")


