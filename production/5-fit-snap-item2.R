
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
options(stringsAsFactors = F, digits = 3, mc.cores = 3)

# loading data
cal <- read_feather("data/data-calendar-clean.feather")
load("predictions/preds-global.RData")
load("predictions/preds-time-all.RData")
load("predictions/preds-month-all.RData")
load("predictions/preds-weekday-all.RData")
load("predictions/preds-snap-store.RData")
train_raw <- read_feather("data/data-train-wide.feather")
train_raw %>% select(1:20) %>% glimpse()

# setting up the baseline time predictions
preds_base <- bind_cols(
  select(preds_time, ends_with("id")), 
  select(preds_time, -ends_with("id")) + 
    select(preds_month, -ends_with("id")) + 
    select(preds_weekday, -ends_with("id")) + 
    select(preds_store_snap, -ends_with("id"))
)
rm(preds_time, preds_month, preds_weekday, preds_store_snap)
gc()

# fitting by store -------------------------------------------------------------

#setup parallel backend to use many processors
# cl <- makeCluster(getOption("mc.cores"))
# registerDoParallel(cl)
# item_month_coefs <- foreach(i=unique(train_raw$item_id), .combine=rbind) %dopar% {
#   library(tidyverse)
#   library(broom)
  
item_snap_coefs <- tibble(data.frame())
pb <- progress_bar$new(total = length(unique(train_raw$item_id)))
for(i in unique(train_raw$item_id)) {
  pb$tick()
  
  dat_i <- train_raw %>% filter(item_id == i)
  off_dat_i <- preds_base %>% filter(item_id == i)
  
  # fitting the models
  mod_data_i <- prep_time_data(dat_i, 
                               off_data = off_dat_i, 
                               off_data_non0_prefix = "lpred_non0_time_", 
                               off_data_sales_prefix = "lpred_sales_time_")
  # non0 model
  non0_mod_data <- mod_data_i %>% filter(between(lp_non0_base, -7, 7))
  cf_non0 <- if(length(unique(non0_mod_data$is_snap)) > 1) {
    m_non0_time_i <- glm(update(f_snap, I(sales != 0) ~ .), 
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
  sales_mod_data <- mod_data_i %>% filter(sales > 0, between(lp_sales_base, -8, 8))
  cf_sales <- if(length(unique(non0_mod_data$is_snap)) > 1) {
    m_sales_time_i <- glm(update(f_snap, sales ~ .), 
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
    mutate(item_id = i)
  
  item_snap_coefs <- bind_rows(item_snap_coefs, results_i)
}

# results_i
# }
# # closing the clusters
# stopCluster(cl)
# stopImplicitCluster()
# gc()


# summary of results
item_snap_coefs %>% 
  group_by(term) %>% 
  summarise(mu_non0 = weighted.mean(estimate_non0, 1 / std.error_non0^2, na.rm = T), 
            sd_between_non0 = sqrt(wtd.var(estimate_non0, 1 / std.error_non0^2)), 
            sd_within_non0 = sqrt(mean(std.error_non0^2, na.rm = T)), 
            mu_sales = weighted.mean(estimate_sales, 1 / std.error_sales^2, na.rm = T), 
            sd_between_sales = sqrt(wtd.var(estimate_sales, 1 / std.error_sales^2)), 
            sd_within_sales = sqrt(mean(std.error_sales^2, na.rm = T))) %>% 
  mutate(across(where(is.numeric), round, digits = 2))

# applying RTTM to coefficients
item_snap_coefs_regr <- train_raw %>% 
  # annoying data manipulation to ensure missing coefs get a row with 0 effect
  select(item_id) %>%  distinct() %>% 
  full_join(expand_grid(item_id = unique(train_raw$item_id), term = unique(item_snap_coefs$term)), by = "item_id") %>% 
  left_join(item_snap_coefs, by = c("item_id", "term")) %>% 
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

ca_item_snap_preds <- expand_grid(state = "CA", 
                                  item_id = item_snap_coefs_regr$item_id, 
                                  day = unique(cal$day)) %>% 
  left_join(cal, by = "day") %>% 
  left_join(item_snap_coefs_regr, by = "item_id") %>% 
  mutate(lpred_non0_item_snap = ifelse(snap_CA == 1, `non0_factor(is_snap)1`, `non0_factor(is_snap)0`), 
         lpred_sales_item_snap = ifelse(snap_CA == 1, `sales_factor(is_snap)1`, `sales_factor(is_snap)0`)) %>% 
  select(state, item_id, day, lpred_non0_item_snap, lpred_sales_item_snap) %>% 
  pivot_wider(names_from = day, values_from = c(lpred_non0_item_snap, lpred_sales_item_snap))
wi_item_snap_preds <- expand_grid(state = "WI", 
                                  item_id = item_snap_coefs_regr$item_id, 
                                  day = unique(cal$day)) %>% 
  left_join(cal, by = "day") %>% 
  left_join(item_snap_coefs_regr, by = "item_id") %>% 
  mutate(lpred_non0_item_snap = ifelse(snap_WI == 1, `non0_factor(is_snap)1`, `non0_factor(is_snap)0`), 
         lpred_sales_item_snap = ifelse(snap_WI == 1, `sales_factor(is_snap)1`, `sales_factor(is_snap)0`)) %>% 
  select(state, item_id, day, lpred_non0_item_snap, lpred_sales_item_snap) %>% 
  pivot_wider(names_from = day, values_from = c(lpred_non0_item_snap, lpred_sales_item_snap))
tx_item_snap_preds <- expand_grid(state = "TX", 
                                  item_id = item_snap_coefs_regr$item_id, 
                                  day = unique(cal$day)) %>% 
  left_join(cal, by = "day") %>% 
  left_join(item_snap_coefs_regr, by = "item_id") %>% 
  mutate(lpred_non0_item_snap = ifelse(snap_TX == 1, `non0_factor(is_snap)1`, `non0_factor(is_snap)0`), 
         lpred_sales_item_snap = ifelse(snap_TX == 1, `sales_factor(is_snap)1`, `sales_factor(is_snap)0`)) %>% 
  select(state, item_id, day, lpred_non0_item_snap, lpred_sales_item_snap) %>% 
  pivot_wider(names_from = day, values_from = c(lpred_non0_item_snap, lpred_sales_item_snap))

item_snap_preds <- bind_rows(ca_item_snap_preds, wi_item_snap_preds, tx_item_snap_preds)

preds_item_snap <- train_raw %>% 
  select(id, item_id, store_id) %>%  
  mutate(state = substr(store_id, 1, 2)) %>% 
  left_join(item_snap_preds, by = c("item_id", "state")) %>% 
  select(ends_with("id"), starts_with("lpred"))

# saving -----------------------------------------------------------------------

save(item_snap_coefs_regr, file = "fitted-models/models-snap-item.RData")
save(preds_item_snap, file = "predictions/preds-snap-item.RData")


