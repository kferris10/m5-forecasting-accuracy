
# notes ------------------------------------------------------------------------

# calculating time trend coefficients for each store
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
train_raw <- read_feather("data/data-train-wide.feather")
load("predictions/preds-global.RData")
load("predictions/preds-time-all.RData")
load("predictions/preds-month-all.RData")

# setting up the baseline time predictions
preds_base <- bind_cols(
  select(preds_time, ends_with("id")), 
  select(preds_time, -ends_with("id")) + 
    select(preds_month, -ends_with("id"))
)

# fitting by store -------------------------------------------------------------

store_weekday_coefs <- tibble(data.frame())
pb <- progress_bar$new(total = length(unique(train_raw$store_id)))
for(i in unique(train_raw$store_id)) {
  pb$tick()
  gc()
  
  dat_i <- train_raw %>% filter(store_id == i)
  off_dat_i <- preds_base %>% filter(store_id == i)
  
  # fitting the models
  time_mod_data_i <- prep_time_data(dat_i, 
                                    off_data = off_dat_i, 
                                    off_data_non0_prefix = "lpred_non0_time_", 
                                    off_data_sales_prefix = "lpred_sales_time_")
  m_non0_time_i <- time_mod_data_i %>% 
    filter(between(lp_non0_base, -7, 7)) %>% 
    glm(update(f_weekday, I(sales != 0) ~ .), 
        data = ., 
        family = binomial(), 
        offset = lp_non0_base)
  m_sales_time_i <- time_mod_data_i %>% 
    filter(sales > 0, between(lp_sales_base, -8, 8)) %>% 
    glm(update(f_weekday, sales ~ .), 
                        data = ., 
                        family = poisson(), 
                        offset = lp_sales_base)
  
  # combining the results into a nice dataset
  results_i <- tidy(m_non0_time_i) %>% 
    left_join(tidy(m_sales_time_i), by = "term", suffix = c("_non0", "_sales")) %>% 
    mutate(store_id = i)
  
  store_weekday_coefs <- bind_rows(store_weekday_coefs, results_i)
}

# summary of results
store_weekday_coefs %>% 
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
store_weekday_coefs_regr <- train_raw %>% 
  # annoying data manipulation to ensure missing coefs get a row with 0 effect
  select(store_id) %>%  distinct() %>% 
  full_join(expand_grid(store_id = unique(train_raw$store_id), term = unique(store_weekday_coefs$term)), by = "store_id") %>% 
  left_join(store_weekday_coefs, by = c("store_id", "term")) %>% 
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

store_weekday_coefs_nested <- store_weekday_coefs_regr %>% 
  group_by(store_id) %>% 
  nest(non0 = starts_with("non0_"), sales = starts_with("sales_"))
store_weekday_preds <- store_weekday_coefs_regr %>% 
  pivot_longer(cols = -store_id, 
               names_to = c(".value", "weekday"), 
               names_sep = "_weekday") %>% 
  left_join(cal, by = "weekday") %>% 
  arrange(store_id, day) %>% 
  select(store_id, day, non0, sales) %>% 
  rename(lpred_non0_store_weekday = non0, 
         lpred_sales_store_weekday = sales) %>% 
  pivot_wider(names_from = day, values_from = c(lpred_non0_store_weekday, lpred_sales_store_weekday))
preds_store_weekday <- train_raw %>% 
  select(id, item_id, store_id) %>%  
  left_join(store_weekday_preds, by = c("store_id"))

# checks -----------------------------------------------------------------------

store_weekday_coefs_regr %>% 
  pivot_longer(c(starts_with("non0"), starts_with("sales")), 
               names_to = c("type", "weekday"), 
               names_sep = "_weekday", 
               values_to = "effect") %>% 
  mutate(weekday = factor(weekday, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>% 
  qplot(as.numeric(weekday), effect, data = ., geom = "line", colour = type) + 
  facet_wrap(~store_id, scales = "free_y")

# saving -----------------------------------------------------------------------

save(store_weekday_coefs_regr, file = "fitted-models/models-weekday-store.RData")
save(preds_store_weekday, file = "predictions/preds-weekday-store.RData")


