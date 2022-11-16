
# notes ------------------------------------------------------------------------

# calculating event coefficients for each item
# this takes ~30 minutes

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
options(stringsAsFactors = F, digits = 3, mc.cores = 3)

# loading data
load("predictions/preds-global.RData")
cal <- read_feather("data/data-calendar-clean.feather")
train_raw <- read_feather("data/data-train-wide.feather")
train_raw %>% select(1:20) %>% glimpse()

# setting up the baseline predictions
load("predictions/preds-time-month-wday-snap.RData")
load("predictions/preds-event-store.RData")

# setting up the baseline time predictions
preds_base <- bind_cols(
  select(preds_time_month_wday_snap, ends_with("id")), 
  select(preds_time_month_wday_snap, -ends_with("id")) + 
    select(preds_store_event, -ends_with("id"))
)
rm(preds_time_month_wday_snap, preds_store_event)
gc()

# fitting by item -------------------------------------------------------------

#setup parallel backend to use many processors
# item_ids <- unique(train_raw$item_id)
# cl <- makeCluster(getOption("mc.cores"))
# registerDoParallel(cl)
# n <- length(item_ids)
# clusterExport(cl, c("n"))
# item_event_coefs <- foreach(i=icount(n), .packages = c("tidyverse", "broom", "tcltk"), .combine=rbind) %dopar% {
#   if(!exists("pb")) pb <- tkProgressBar("Parallel task", min=1, max=n)
#   setTkProgressBar(pb, i)  
  
item_event_coefs <- tibble(data.frame())
pb <- progress_bar$new(total = length(unique(train_raw$item_id)))
for(i in unique(train_raw$item_id)) {
  pb$tick()
  item_i <- i
  
  dat_i <- train_raw %>% filter(item_id == item_i)
  off_dat_i <- preds_base %>% filter(item_id == item_i)
  
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
    mutate(item_id = item_i)
  
  # manual memory cleaning is annoying...
  rm(dat_i, off_dat_i, mod_data_i, non0_mod_data, sales_mod_data, m_non0_time_i, m_sales_time_i)
  gc()
  
  item_event_coefs <- bind_rows(item_event_coefs, results_i)
}

#   results_i
# }
# 
# # closing the clusters
# stopCluster(cl)
# stopImplicitCluster()
# gc()

# summary of results
item_event_coefs %>% 
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
item_event_coefs_regr <- train_raw %>% 
  # annoying data manipulation to ensure missing coefs get a row with 0 effect
  select(item_id) %>%  distinct() %>% 
  full_join(expand_grid(item_id = unique(train_raw$item_id), term = unique(item_event_coefs$term)), by = "item_id") %>% 
  left_join(item_event_coefs, by = c("item_id", "term")) %>% 
  # applying RTTM
  group_by(term) %>% 
  mutate(mu_non0 = weighted.mean(estimate_non0, 1 / std.error_non0^2, na.rm = T), 
         sd_between_non0 = sqrt(wtd.var(estimate_non0, 1 / std.error_non0^2)), 
         mu_sales = weighted.mean(estimate_sales, 1 / std.error_sales^2, na.rm = T), 
         sd_between_sales = sqrt(wtd.var(estimate_sales, 1 / std.error_sales^2))) %>% 
  mutate(est_regr_non0 = coalesce(apply_rttm(estimate_non0, std.error_non0, sd_between_non0, mu_non0), 0), 
         est_regr_sales = coalesce(apply_rttm(estimate_sales, std.error_sales, sd_between_sales, mu_sales), 0)) %>% 
  ungroup() %>% 
  select(item_id, term, est_regr_non0, est_regr_sales) %>% 
  pivot_wider(names_from = term, values_from = c(est_regr_non0, est_regr_sales)) %>% 
  rename_with(str_replace_all, pattern = "est_regr_", replacement = "") %>% 
  mutate(across(where(is.numeric), round, digits = 4))

# generating predictions -------------------------------------------------------

cal_event_long <- cal %>% 
  select(day, mem:fday_m2) %>% 
  pivot_longer(-day, names_to = "event", values_to = "ind")

# looping through to generate predictions for each item
# memory crash otherwise
item_event_preds <- tibble(data.frame())
for(i in 1:nrow(item_event_coefs_regr)) {
  if(i %% 200 == 1) print(i)
  item_event_coefs_long_i <- item_event_coefs_regr %>% 
    slice(i) %>% 
    pivot_longer(-item_id, names_to = "variable", values_to = "coef") %>% 
    mutate(type = ifelse(substr(variable, 1, 4) == "non0", "non0", "sales"), 
           event = ifelse(type == "non0", 
                          substr(variable, 6, nchar(variable)), 
                          substr(variable, 7, nchar(variable)))) %>% 
    select(item_id, type, event, coef)
  
  item_event_preds_i <- item_event_coefs_long_i %>% 
    left_join(cal_event_long, by = c("event")) %>% 
    group_by(item_id, type, day) %>% 
    summarise(effect = sum(coef * ind), .groups = "drop") %>% 
    mutate(type = paste("lpred", type, "item_event", sep = "_")) %>% 
    pivot_wider(names_from = c(type, day), values_from = effect)
  
  item_event_preds <- bind_rows(item_event_preds, item_event_preds_i)
}

preds_item_event <- train_raw %>% 
  select(id, item_id, store_id) %>%  
  left_join(item_event_preds, by = c("item_id"))

# checks -----------------------------------------------------------------------

item_event_coefs_regr %>% 
  pivot_longer(-item_id, names_to = "variable", values_to = "coef") %>% 
  mutate(type = ifelse(substr(variable, 1, 4) == "non0", "non0", "sales"), 
         event = ifelse(type == "non0", 
                        substr(variable, 6, nchar(variable)), 
                        substr(variable, 7, nchar(variable)))) %>% 
  filter(item_id %in% c(
    "FOODS_3_252", "FOODS_2_222", "HOBBIES_1_001", 
    sample(unique(train_raw$item_id), 3)
  )) %>% 
  qplot(as.numeric(factor(event)), coef, data = ., geom = "line", colour = type) + 
  facet_wrap(~item_id, scales = "free_y")

# saving -----------------------------------------------------------------------

save(item_event_coefs_regr, file = "fitted-models/models-event-item.RData")
save(preds_item_event, file = "predictions/preds-event-item.RData")


