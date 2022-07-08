
# notes ------------------------------------------------------------------------

# calculating time trend coefficients for each store
# this takes ~20 minutes

# setup ------------------------------------------------------------------------

library(feather)
library(Hmisc)
library(tidyverse)
library(broom)
library(progress)
source("production/o-helper-funs.R")
options(stringsAsFactors = F, digits = 3, mc.cores = 4)

# loading data
cal <- read_csv("data/calendar.csv") %>% 
  mutate(day = as.numeric(str_replace_all(d, "d_", "")))
load("predictions/preds-global.RData")
load("predictions/preds-time-all.RData")
train_raw <- read_feather("data/data-train-wide.feather")
train_raw %>% select(1:20) %>% glimpse()

# fitting by store -------------------------------------------------------------

store_month_coefs <- tibble(data.frame())

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
    glm(update(f_month, I(sales != 0) ~ .), 
        data = ., 
        family = binomial(), 
        offset = lp_non0_base)
  m_sales_time_i <- time_mod_data_i %>% 
    filter(sales > 0, between(lp_sales_base, -8, 8)) %>%
    glm(update(f_month, sales ~ .), 
        data = ., 
        family = poisson(), 
        offset = lp_sales_base)
  
  # combining the results into a nice dataset
  results_i <- tidy(m_non0_time_i) %>% 
    left_join(tidy(m_sales_time_i), by = "term", suffix = c("_non0", "_sales")) %>% 
    mutate(store_id = i)
  
  store_month_coefs <- bind_rows(store_month_coefs, results_i)
}

# summary of results
store_month_coefs %>% 
  group_by(term) %>% 
  summarise(mu_non0 = weighted.mean(estimate_non0, 1 / std.error_non0^2, na.rm = T), 
            sd_between_non0 = sqrt(wtd.var(estimate_non0, 1 / std.error_non0^2)), 
            sd_within_non0 = sqrt(mean(std.error_non0^2, na.rm = T)), 
            mu_sales = weighted.mean(estimate_sales, 1 / std.error_sales^2, na.rm = T), 
            sd_between_sales = sqrt(wtd.var(estimate_sales, 1 / std.error_sales^2)), 
            sd_within_sales = sqrt(mean(std.error_sales^2, na.rm = T))) %>% 
  mutate(across(where(is.numeric), round, digits = 2))

# applying RTTM to coefficients
store_month_coefs_regr <- store_month_coefs %>% 
  group_by(term) %>% 
  mutate(sd_between_non0 = sqrt(wtd.var(estimate_non0, 1 / std.error_non0^2)), 
         sd_between_sales = sqrt(wtd.var(estimate_sales, 1 / std.error_sales^2)), 
         est_regr_non0 = apply_rttm(estimate_non0, std.error_non0, sd_between_non0), 
         est_regr_sales = apply_rttm(estimate_sales, std.error_sales, sd_between_sales)) %>% 
  ungroup() %>% 
  select(store_id, term, est_regr_non0, est_regr_sales) %>% 
  pivot_wider(names_from = term, values_from = c(est_regr_non0, est_regr_sales)) %>% 
  rename_with(str_replace_all, pattern = "est_regr_", replacement = "") %>% 
  mutate(across(where(is.numeric), round, digits = 4))

# generating predictions -------------------------------------------------------

store_month_coefs_nested <- store_month_coefs_regr %>% 
  group_by(store_id) %>% 
  nest(non0 = starts_with("non0_"), sales = starts_with("sales_"))
store_month_preds <- store_month_coefs_regr %>% 
  pivot_longer(cols = -store_id, 
               names_to = c(".value", "month"), 
               names_sep = "_factor\\(month\\)", 
               names_transform = list(month = as.integer)) %>% 
  left_join(cal %>% mutate(day = as.numeric(str_replace_all(d, "d_", ""))), by = "month") %>% 
  arrange(store_id, day) %>% 
  select(store_id, day, non0, sales) %>% 
  rename(lpred_non0_store_month = non0, 
         lpred_sales_store_month = sales) %>% 
  pivot_wider(names_from = day, values_from = c(lpred_non0_store_month, lpred_sales_store_month))
preds_store_month <- train_raw %>% 
  select(id, item_id, store_id) %>%  
  left_join(store_month_preds, by = c("store_id"))

# checks -----------------------------------------------------------------------

bind_cols(
  select(preds_store_month, ends_with("id")), 
  select(preds_store_month, starts_with("lpred")) + 
    select(preds_time, starts_with("lpred"))
  )%>% 
  filter(item_id == "FOODS_3_252") %>% 
  pivot_longer(starts_with("lpred_"), 
               names_prefix = "lpred_", 
               names_to = "day", 
               values_to = "sales")  %>% 
  mutate(type = ifelse(substr(day, 1, 4) == "non0", "non0", "sales"), 
         day = str_replace_all(day, "non0_store_month_", ""), 
         day = str_replace_all(day, "sales_store_month_", ""), 
         day = as.numeric(day)) %>% 
  pivot_wider(names_from = type, values_from = sales) %>% 
  left_join(preds_global, by = c("day")) %>% 
  mutate(pred_sales = arm::invlogit(non0 + lp_non0_base) * exp(sales + lp_sales_base)) %>% 
  sample_frac(.2) %>%
  qplot(day, pred_sales, data = ., colour = store_id, geom = "line") + 
  geom_vline(aes(xintercept = 1941))

# saving -----------------------------------------------------------------------

save(store_month_coefs_regr, file = "fitted-models/models-month-store.RData")
save(preds_store_month, file = "predictions/preds-month-store.RData")


