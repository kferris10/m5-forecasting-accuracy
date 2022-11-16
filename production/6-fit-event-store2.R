
# notes ------------------------------------------------------------------------

# calculating event coefficients for each store
# this takes ~1 hour

# setup ------------------------------------------------------------------------

library(feather)
library(Hmisc)
library(tidyverse)
library(broom)
library(progress)
source("production/0-helper-funs.R")
options(stringsAsFactors = F, digits = 3, mc.cores = 3)

# loading data
load("predictions/preds-global.RData")
cal <- read_feather("data/data-calendar-clean.feather")
train_raw <- read_feather("data/data-train-wide.feather")
train_raw %>% select(1:20) %>% glimpse()

# setting up the baseline predictions
load("predictions/preds-time-month-wday-snap.RData")

# fitting by store -------------------------------------------------------------

store_event_coefs <- tibble(data.frame())
pb <- progress_bar$new(total = length(unique(train_raw$store_id)))
for(i in unique(train_raw$store_id)) {
  pb$tick()
  
  dat_i <- train_raw %>% filter(store_id == i)
  off_dat_i <- preds_time_month_wday_snap %>% filter(store_id == i)
  
  # ith dataset
  mod_data_i <- prep_time_data(dat_i, 
                                    off_data = off_dat_i, 
                                    off_data_non0_prefix = "lpred_non0_base_", 
                                    off_data_sales_prefix = "lpred_sales_base_")
  
  # non0 model
  non0_mod_data <- mod_data_i %>% 
    filter(between(lp_non0_base, -7, 7)) %>% 
    filter(sum(sales == 0) > 0, 
           sum(sales != 0) > 0)
  cf_non0 <- if(nrow(non0_mod_data) > 100) {
    m_non0_time_i <- glm(update(f_event, I(sales != 0) ~ .), 
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
  sales_mod_data <- mod_data_i %>% filter(sales > 0, between(lp_sales_base, -7, 7))
  cf_sales <- if(nrow(sales_mod_data) > 100) {
    m_sales_time_i <- glm(update(f_event, sales ~ .), 
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
    mutate(store_id = i)
  
  store_event_coefs <- bind_rows(store_event_coefs, results_i)
  
  # manual memory cleaning is annoying...
  rm(dat_i, off_dat_i, non0_mod_data, sales_mod_data, m_non0_time_i, m_sales_time_i)
  gc()
}

# summary of results
store_event_coefs %>% 
  group_by(term) %>% 
  summarise(
    mu_non0 = weighted.mean(estimate_non0, 1 / std.error_non0^2, na.rm = T), 
    sd_between_non0 = sqrt(wtd.var(estimate_non0, 1 / std.error_non0^2)), 
    sd_within_non0 = sqrt(mean(std.error_non0^2, na.rm = T)), 
    sd_within_non0_med = median(std.error_non0, na.rm = T), 
    mu_sales = weighted.mean(estimate_sales, 1 / std.error_sales^2, na.rm = T), 
    sd_between_sales = sqrt(wtd.var(estimate_sales, 1 / std.error_sales^2)), 
    sd_within_sales = sqrt(mean(std.error_sales^2, na.rm = T)), 
    sd_within_sales_med = median(std.error_sales, na.rm = T)
  ) %>% 
  mutate(across(where(is.numeric), round, digits = 2))

# applying RTTM to coefficients
store_event_coefs_regr <- train_raw %>% 
  # annoying data manipulation to ensure missing coefs get a row with 0 effect
  select(store_id) %>%  distinct() %>% 
  full_join(expand_grid(store_id = unique(train_raw$store_id), term = unique(store_event_coefs$term)), by = "store_id") %>% 
  left_join(store_event_coefs, by = c("store_id", "term")) %>% 
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

store_event_coefs_long <- store_event_coefs_regr %>% 
  pivot_longer(-store_id, names_to = "variable", values_to = "coef") %>% 
  mutate(type = ifelse(substr(variable, 1, 4) == "non0", "non0", "sales"), 
         event = ifelse(type == "non0", 
                        substr(variable, 6, nchar(variable)), 
                        substr(variable, 7, nchar(variable)))) %>% 
  select(store_id, type, event, coef)
cal_event_long <- cal %>% 
  select(day, mem:fday_m2) %>% 
  pivot_longer(-day, names_to = "event", values_to = "ind")
store_event_preds <- store_event_coefs_long %>% 
  left_join(cal_event_long, by = c("event")) %>% 
  group_by(store_id, type, day) %>% 
  summarise(effect = sum(coef * ind), .groups = "drop") %>% 
  mutate(type = paste("lpred", type, "store_event", sep = "_")) %>% 
  pivot_wider(names_from = c(type, day), values_from = effect)
preds_store_event <- train_raw %>% 
  select(id, item_id, store_id) %>%  
  left_join(store_event_preds, by = c("store_id"))

# checks -----------------------------------------------------------------------

store_event_coefs_long %>% 
  qplot(as.numeric(factor(event)), coef, data = ., geom = "line", colour = type) + 
  facet_wrap(~store_id)

# saving -----------------------------------------------------------------------

save(store_event_coefs_regr, file = "fitted-models/models-event-store.RData")
save(preds_store_event, file = "predictions/preds-event-store.RData")


