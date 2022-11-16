
# notes ------------------------------------------------------------------------

# calculating event coefficients for each store/item
# takes ~ 5 hour

# setup ------------------------------------------------------------------------

library(feather)
library(Hmisc)
library(tidyverse)
library(broom)
library(progress)
# library(foreach)
# library(iterators)
# library(doParallel)
# library(tcltk)
source("production/0-helper-funs.R")
options(stringsAsFactors = F, digits = 3, mc.cores = 2)

# loading data
load("predictions/preds-global.RData")
cal <- read_feather("data/data-calendar-clean.feather")
train_raw <- read_feather("data/data-train-wide.feather")
train_raw %>% select(1:20) %>% glimpse()

# setting up the baseline predictions
load("predictions/preds-time-month-wday-snap.RData")
load("predictions/preds-event-store.RData")
load("predictions/preds-event-item.RData")

# setting up the baseline time predictions
preds_base <- bind_cols(
  select(preds_time_month_wday_snap, ends_with("id")), 
  select(preds_time_month_wday_snap, -ends_with("id")) + 
    select(preds_store_event, -ends_with("id")) + 
    select(preds_item_event, -ends_with("id"))
)
rm(preds_time_month_wday_snap, preds_store_event, preds_item_event)
gc()

# fitting by item -------------------------------------------------------------

#setup parallel backend to use many processors
# cl <- makeCluster(getOption("mc.cores"))
# registerDoParallel(cl)
# n <- nrow(train_raw)
# clusterExport(cl, c("n"))
# store_item_event_coefs <- foreach(i=icount(n), .packages = c("tidyverse", "broom", "tcltk"), .combine=rbind) %dopar% {
#   if(!exists("pb")) pb <- tkProgressBar("Parallel task", min=1, max=n)
#   setTkProgressBar(pb, i)  

# if I need to test one at a time
store_item_event_coefs <- tibble(data.frame())
pb <- progress_bar$new(total = nrow(train_raw))
for(i in 1:nrow(train_raw)) {
  pb$tick()
  
  store_i <- train_raw$store_id[i]
  item_i <- train_raw$item_id[i]
  
  dat_i <- train_raw %>% filter(item_id == item_i, store_id == store_id)
  off_dat_i <- preds_base %>% filter(item_id == item_i, store_id == store_id)
  
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
  cf_non0 <- if(nrow(non0_mod_data) > 45) {
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
  cf_sales <- if(nrow(sales_mod_data) > 45) {
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
    mutate(item_id = item_i, 
           store_id = store_i)
  
  # manual memory cleaning is annoying...
  rm(dat_i, off_dat_i, mod_data_i, non0_mod_data, sales_mod_data, m_non0_time_i, m_sales_time_i)
  gc()
  
  store_item_event_coefs <- bind_rows(store_item_event_coefs, results_i)
}
  
#   results_i
# }
# 
# # closing the clusters
# stopCluster(cl)
# stopImplicitCluster()
# gc()

# summary of results
store_item_event_coefs %>% 
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
store_item_event_coefs_regr <- expand_grid(store_id = unique(train_raw$store_id), 
                                             item_id = unique(train_raw$item_id), 
                                             term = unique(store_item_event_coefs$term)) %>%
  left_join(store_item_event_coefs, by = c("store_id", "item_id", "term")) %>% 
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

cal_event_long <- cal %>% 
  select(day, mem:fday_m2) %>% 
  pivot_longer(-day, names_to = "event", values_to = "ind")

# looping through to generate predictions for each item
# memory crash otherwise
store_item_event_preds <- tibble(data.frame())
pb <- progress_bar$new(total = nrow(store_item_event_coefs_regr))
for(i in 1:nrow(store_item_event_coefs_regr)) {
  pb$tick()
  
  store_item_event_coefs_long_i <- store_item_event_coefs_regr %>% 
    slice(i) %>% 
    pivot_longer(-c(store_id, item_id), names_to = "variable", values_to = "coef") %>% 
    mutate(type = ifelse(substr(variable, 1, 4) == "non0", "non0", "sales"), 
           event = ifelse(type == "non0", 
                          substr(variable, 6, nchar(variable)), 
                          substr(variable, 7, nchar(variable)))) %>% 
    select(store_id, item_id, type, event, coef)
  
  store_item_event_preds_i <- store_item_event_coefs_long_i %>% 
    left_join(cal_event_long, by = c("event")) %>% 
    group_by(store_id, item_id, type, day) %>% 
    summarise(effect = sum(coef * ind), .groups = "drop") %>% 
    mutate(type = paste("lpred", type, "item_event", sep = "_")) %>% 
    pivot_wider(names_from = c(type, day), values_from = effect)
  
  store_item_event_preds <- bind_rows(store_item_event_preds, store_item_event_preds_i)
}

preds_store_item_event <- train_raw %>% 
  select(id, item_id, store_id) %>%  
  left_join(store_item_event_preds, by = c("store_id", "item_id"))


# checks -----------------------------------------------------------------------

store_item_event_coefs_regr %>% 
  pivot_longer(-c(item_id, store_id), names_to = "variable", values_to = "coef") %>% 
  mutate(type = ifelse(substr(variable, 1, 4) == "non0", "non0", "sales"), 
         event = ifelse(type == "non0", 
                        substr(variable, 6, nchar(variable)), 
                        substr(variable, 7, nchar(variable)))) %>% 
  filter(item_id %in% c(
    "FOODS_3_252", "FOODS_2_222", "HOBBIES_1_001", 
    sample(unique(train_raw$item_id), 3)
  ), 
  store_id %in% c("CA_1", "TX_1")) %>% 
  qplot(as.numeric(factor(event)), coef, data = ., geom = "line", colour = type) + 
  facet_wrap(store_id~item_id)

# predictions from store-item effects
preds_new <- preds_store_item_event %>% 
  filter(store_id %in% c("TX_1", "CA_1"), 
         item_id %in% c(sample(train_raw$item_id, 3), "FOODS_3_555", "HOUSEHOLD_1_441")) %>% 
  pivot_longer(cols = starts_with("lpred"), 
               names_prefix = "lpred_", 
               names_to = c("type", "day"), 
               names_sep = "_item_event_", 
               names_transform = list(day = as.integer), 
               values_to = "effect")

# seems reasonable
qplot(day, effect, data = preds_new, colour = type, geom = "line") + 
  facet_grid(store_id ~ item_id)

# saving -----------------------------------------------------------------------

# saving the full event effect
load("predictions/preds-event-store.RData")
load("predictions/preds-event-item.RData")

preds_event <- bind_cols(
  select(train_raw, 1:3), 
  select(preds_store_event, starts_with("lpred")) + 
    select(preds_item_event, starts_with("lpred")) + 
    select(preds_store_item_event, starts_with("lpred"))
) %>% 
  rename_with(str_replace_all, pattern = "_store_item_", replacement = "_") %>% 
  rename_with(str_replace_all, pattern = "_store_", replacement = "_") %>% 
  rename_with(str_replace_all, pattern = "_item_", replacement = "_")


save(store_item_event_coefs_regr, file = "fitted-models/models-event-store-item.RData")
save(preds_store_item_event, file = "predictions/preds-event-store-item.RData")
save(preds_event, file = "predictions/preds-event-all.RData")



