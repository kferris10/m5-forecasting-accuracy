
# notes ------------------------------------------------------------------------

# calculating monthly coefficients for each store/item
# takes ~ 4 hours

# setup ------------------------------------------------------------------------

library(feather)
library(Hmisc)
library(tidyverse)
library(broom)
library(progress)
library(foreach)
library(iterators)
library(doParallel)
library(tcltk)
source("production/helper-funs.R")
options(stringsAsFactors = F, digits = 3, mc.cores = 2)

# model specifications I'll be using
f_month <- formula(~ 0 + factor(month))

# loading data
cal <- read_feather("data/data-calendar-clean.feather")
train_raw <- read_feather("data/data-train-wide.feather")
load("predictions/preds-global.RData")
load("predictions/preds-time-all.RData")
load("predictions/preds-month-all.RData")
load("predictions/preds-weekday-all.RData")
load("predictions/preds-snap-store.RData")
load("predictions/preds-snap-item.RData")
train_raw %>% select(1:20) %>% glimpse()

# setting up the baseline time predictions
preds_base <- bind_cols(
  select(preds_time, ends_with("id")), 
  select(preds_time, -ends_with("id")) + 
    select(preds_month, -ends_with("id")) + 
    select(preds_weekday, -ends_with("id")) + 
    select(preds_store_snap, -ends_with("id")) + 
    select(preds_item_snap, -ends_with("id"))
)
rm(preds_time, preds_month, preds_weekday, preds_item_snap, preds_store_snap)
gc()

# fitting by item -------------------------------------------------------------

# setup parallel backend to use many processors
# cl <- makeCluster(getOption("mc.cores"))
# registerDoParallel(cl)
# n <- nrow(train_raw)
# clusterExport(cl, c("n"))
# store_item_snap_coefs <- foreach(i=icount(n), .packages = c("tidyverse", "broom", "tcltk"), .combine=rbind) %dopar% {
#   if(!exists("pb")) pb <- tkProgressBar("Parallel task", min=1, max=n)
#   setTkProgressBar(pb, i)
  
# if I need to test one at a time
store_item_snap_coefs <- tibble(data.frame())
pb <- progress_bar$new(total = nrow(train_raw))
for(i in 1:nrow(train_raw)) {
  pb$tick()
  
  dat_i <- train_raw %>% 
    filter(item_id == train_raw$item_id[i], store_id == train_raw$store_id[i])
  off_dat_i <- preds_base %>% 
    filter(item_id == train_raw$item_id[i], store_id == train_raw$store_id[i])
  
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
  sales_mod_data <- mod_data_i %>% filter(sales > 0, between(lp_sales_base, -5, 5))
  cf_sales <- if(length(unique(sales_mod_data$is_snap)) > 1) {
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
    mutate(item_id = train_raw$item_id[i], store_id = train_raw$store_id[i])


  store_item_snap_coefs <- bind_rows(store_item_snap_coefs, results_i)
}

#   results_i
# }
# # closing the clusters
# stopCluster(cl)
# stopImplicitCluster()
# gc()

# summary of results
store_item_snap_coefs %>% 
  group_by(term) %>% 
  summarise(mu_non0 = weighted.mean(estimate_non0, 1 / std.error_non0^2, na.rm = T), 
            sd_between_non0 = sqrt(wtd.var(estimate_non0, 1 / std.error_non0^2)), 
            sd_within_non0 = sqrt(mean(std.error_non0^2, na.rm = T)), 
            mu_sales = weighted.mean(estimate_sales, 1 / std.error_sales^2, na.rm = T), 
            sd_between_sales = sqrt(wtd.var(estimate_sales, 1 / std.error_sales^2)), 
            sd_within_sales = sqrt(mean(std.error_sales^2, na.rm = T))) %>% 
  mutate(across(where(is.numeric), round, digits = 2))

# applying RTTM to coefficients
store_item_snap_coefs_regr <- train_raw %>% 
  # annoying data manipulation to ensure missing coefs get a row with 0 effect
  select(store_id, item_id) %>% 
  full_join(expand_grid(store_id = unique(train_raw$store_id), term = unique(store_item_snap_coefs$term)), by = "store_id") %>% 
  left_join(store_item_snap_coefs, by = c("store_id", "item_id", "term")) %>% 
  # fixing one wonky point
  mutate(estimate_non0 = ifelse(abs(estimate_non0) > 1e5, 0, estimate_non0)) %>% 
  # applying RTTM
  group_by(term) %>% 
  mutate(mu_non0 = weighted.mean(estimate_non0, 1 / std.error_non0^2, na.rm = T), 
         sd_between_non0 = sqrt(wtd.var(estimate_non0, 1 / std.error_non0^2)), 
         mu_sales = weighted.mean(estimate_sales, 1 / std.error_sales^2, na.rm = T), 
         sd_between_sales = sqrt(wtd.var(estimate_sales, 1 / std.error_sales^2))) %>% 
  mutate(est_regr_non0 = coalesce(apply_rttm(estimate_non0, std.error_non0, sd_between_non0, mu_non0), 0), 
         est_regr_sales = coalesce(apply_rttm(estimate_sales, std.error_sales, sd_between_sales, mu_sales), 0)) %>% 
  ungroup() %>% 
  # organize into a nicer format
  select(store_id, item_id, term, est_regr_non0, est_regr_sales) %>% 
  pivot_wider(names_from = term, values_from = c(est_regr_non0, est_regr_sales)) %>% 
  rename_with(str_replace_all, pattern = "est_regr_", replacement = "") %>% 
  mutate(across(where(is.numeric), round, digits = 4))

# generating predictions -------------------------------------------------------
# predicting by state for simplicity

# CA predictions
ca_store_item_snap_preds <- expand_grid(store_id = c("CA_1", "CA_2", "CA_3", "CA_4"), 
                                  item_id = unique(store_item_snap_coefs_regr$item_id), 
                                  day = unique(cal$day)) %>% 
  left_join(cal, by = "day") %>% 
  left_join(store_item_snap_coefs_regr, by = c("item_id", "store_id")) %>% 
  mutate(lpred_non0_store_item_snap = ifelse(snap_CA == 1, `non0_factor(is_snap)1`, `non0_factor(is_snap)0`), 
         lpred_sales_store_item_snap = ifelse(snap_CA == 1, `sales_factor(is_snap)1`, `sales_factor(is_snap)0`)) %>% 
  select(state, store_id, item_id, day, lpred_non0_store_item_snap, lpred_sales_store_item_snap) %>% 
  pivot_wider(names_from = day, values_from = c(lpred_non0_store_item_snap, lpred_sales_store_item_snap))
# WI predictions
wi_store_item_snap_preds <- expand_grid(store_id = c("WI_1", "WI_2", "WI_3"), 
                                  item_id = unique(store_item_snap_coefs_regr$item_id), 
                                  day = unique(cal$day)) %>% 
  left_join(cal, by = "day") %>% 
  left_join(store_item_snap_coefs_regr, by = c("item_id", "store_id")) %>% 
  mutate(lpred_non0_store_item_snap = ifelse(snap_WI == 1, `non0_factor(is_snap)1`, `non0_factor(is_snap)0`), 
         lpred_sales_store_item_snap = ifelse(snap_WI == 1, `sales_factor(is_snap)1`, `sales_factor(is_snap)0`)) %>% 
  select(state, store_id, item_id, day, lpred_non0_store_item_snap, lpred_sales_store_item_snap) %>% 
  pivot_wider(names_from = day, values_from = c(lpred_non0_store_item_snap, lpred_sales_store_item_snap))
# TX predictions
tx_store_item_snap_preds <- expand_grid(store_id = c("TX_1", "TX_2", "TX_3"), 
                                  item_id = unique(store_item_snap_coefs_regr$item_id), 
                                  day = unique(cal$day)) %>% 
  left_join(cal, by = "day") %>% 
  left_join(store_item_snap_coefs_regr, by = c("item_id", "store_id")) %>% 
  mutate(lpred_non0_store_item_snap = ifelse(snap_TX == 1, `non0_factor(is_snap)1`, `non0_factor(is_snap)0`), 
         lpred_sales_store_item_snap = ifelse(snap_TX == 1, `sales_factor(is_snap)1`, `sales_factor(is_snap)0`)) %>% 
  select(state, store_id, item_id, day, lpred_non0_store_item_snap, lpred_sales_store_item_snap) %>% 
  pivot_wider(names_from = day, values_from = c(lpred_non0_store_item_snap, lpred_sales_store_item_snap))

store_item_snap_preds <- bind_rows(ca_store_item_snap_preds, wi_store_item_snap_preds, tx_store_item_snap_preds) 

preds_store_item_snap <- train_raw %>% 
  select(id, item_id, store_id) %>%  
  left_join(store_item_snap_preds, by = c("item_id", "store_id"))

# saving -----------------------------------------------------------------------

# saving the full snap effect
load("predictions/preds-snap-store.RData")
load("predictions/preds-snap-item.RData")
preds_snap <- bind_cols(
  select(train_raw, 1:3), 
  select(preds_store_snap, starts_with("lpred")) + 
    select(preds_item_snap, starts_with("lpred")) + 
    select(preds_store_item_snap, starts_with("lpred"))
) %>% 
  rename_with(str_replace_all, pattern = "_store_item_", replacement = "_") %>% 
  rename_with(str_replace_all, pattern = "_store_", replacement = "_") %>% 
  rename_with(str_replace_all, pattern = "_item_", replacement = "_")


save(store_item_snap_coefs_regr, file = "fitted-models/models-snap-store-item.RData")
save(preds_store_item_snap, file = "predictions/preds-snap-store-item.RData")
save(preds_snap, file = "predictions/preds-snap-all.RData")



