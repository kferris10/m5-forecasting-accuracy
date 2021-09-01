
# notes ------------------------------------------------------------------------

# calculating monthly coefficients for each item
# this takes ~10 minutes

# setup ------------------------------------------------------------------------

library(feather)
library(Hmisc)
library(tidyverse)
library(broom)
library(foreach)
library(iterators)
library(doParallel)
library(tcltk)
source("production/helper-funs.R")
options(stringsAsFactors = F, digits = 3, mc.cores = 3)

# loading data
cal <- read_feather("data/data-calendar-clean.feather")
load("predictions/preds-global.RData")
load("predictions/preds-time-all.RData")
load("predictions/preds-month-all.RData")
load("predictions/preds-weekday-store.RData")
train_raw <- read_feather("data/data-train-wide.feather")
train_raw %>% select(1:20) %>% glimpse()

# setting up the baseline time predictions
preds_base <- bind_cols(
  select(preds_time, ends_with("id")), 
  select(preds_time, -ends_with("id")) + 
    select(preds_month, -ends_with("id")) + 
    select(preds_store_weekday, -ends_with("id"))
)
rm(preds_time, preds_month, preds_store_weekday)
gc()

# fitting by item -------------------------------------------------------------

#setup parallel backend to use many processors
item_ids <- unique(train_raw$item_id)
cl <- makeCluster(getOption("mc.cores"))
registerDoParallel(cl)
n <- length(item_ids)
clusterExport(cl, c("n"))
item_weekday_coefs <- foreach(i=icount(n), .packages = c("tidyverse", "broom", "tcltk"), .combine=rbind) %dopar% {
  if(!exists("pb")) pb <- tkProgressBar("Parallel task", min=1, max=n)
  setTkProgressBar(pb, i)  
  
# item_coefs <- tibble(data.frame())
# pb <- progress_bar$new(total = length(unique(train_raw$item_id)))
# for(i in unique(train_raw$item_id)) {
#   pb$tick()
  item_i <- item_ids[i]
  
  dat_i <- train_raw %>% filter(item_id == item_i)
  off_dat_i <- preds_base %>% filter(item_id == item_i)
  
  # fitting the models
  mod_data_i <- prep_time_data(dat_i, 
                               off_data = off_dat_i, 
                               off_data_non0_prefix = "lpred_non0_time_", 
                               off_data_sales_prefix = "lpred_sales_time_")
  # non0 model
  non0_mod_data <- mod_data_i %>% filter(between(lp_non0_base, -7, 7))
  cf_non0 <- if(length(unique(non0_mod_data$weekday)) > 1) {
    m_non0_time_i <- glm(update(f_weekday, I(sales != 0) ~ .), 
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
  cf_sales <- if(length(unique(non0_mod_data$weekday)) > 1) {
    m_sales_time_i <- glm(update(f_weekday, sales ~ .), 
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
  
#   item_coefs <- bind_rows(item_coefs, results_i)
# }
  
  results_i
}

# closing the clusters
stopCluster(cl)
stopImplicitCluster()
gc()

# summary of results
item_weekday_coefs %>% 
  group_by(term) %>% 
  summarise(mu_non0 = weighted.mean(estimate_non0, 1 / std.error_non0^2, na.rm = T), 
            sd_between_non0 = sqrt(wtd.var(estimate_non0, 1 / std.error_non0^2)), 
            sd_within_non0 = sqrt(mean(std.error_non0^2, na.rm = T)), 
            mu_sales = weighted.mean(estimate_sales, 1 / std.error_sales^2, na.rm = T), 
            sd_between_sales = sqrt(wtd.var(estimate_sales, 1 / std.error_sales^2)), 
            sd_within_sales = sqrt(mean(std.error_sales^2, na.rm = T))) %>% 
  mutate(across(where(is.numeric), round, digits = 2))

# applying RTTM to coefficients
item_weekday_coefs_regr <- train_raw %>% 
  # annoying data manipulation to ensure missing coefs get a row with 0 effect
  select(item_id) %>%  distinct() %>% 
  full_join(expand_grid(item_id = unique(train_raw$item_id), term = unique(item_weekday_coefs$term)), by = "item_id") %>% 
  left_join(item_weekday_coefs, by = c("item_id", "term")) %>% 
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

item_coefs_nested <- item_weekday_coefs_regr %>% 
  group_by(item_id) %>% 
  nest(non0 = starts_with("non0_"), sales = starts_with("sales_"))
item_preds <- item_weekday_coefs_regr %>% 
  pivot_longer(cols = c(starts_with("non0"), starts_with("sales")), 
               names_to = c(".value", "weekday"), 
               names_sep = "_weekday") %>% 
  left_join(cal, by = "weekday") %>% 
  arrange(item_id, day) %>% 
  select(item_id, day, non0, sales) %>% 
  rename(lpred_non0_item_weekday = non0, 
         lpred_sales_item_weekday = sales) %>% 
  pivot_wider(names_from = day, values_from = c(lpred_non0_item_weekday, lpred_sales_item_weekday))
preds_item_weekday <- train_raw %>% 
  select(id, item_id, store_id) %>%  
  left_join(item_preds, by = c("item_id"))

# checks -----------------------------------------------------------------------

item_weekday_coefs_regr %>% 
  pivot_longer(c(starts_with("non0"), starts_with("sales")), 
               names_to = c("type", "weekday"), 
               names_sep = "_weekday", 
               values_to = "effect") %>% 
  filter(item_id %in% c(
    "FOODS_3_252", "FOODS_2_222", "HOBBIES_1_001", 
    sample(unique(train_raw$item_id), 3)
  )) %>% 
  mutate(weekday = factor(weekday, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>% 
  qplot(as.numeric(weekday), effect, data = ., geom = "line", colour = type) + 
  facet_wrap(~item_id, scales = "free_y")

# saving -----------------------------------------------------------------------

save(item_weekday_coefs_regr, file = "fitted-models/models-weekday-item.RData")
save(preds_item_weekday, file = "predictions/preds-weekday-item.RData")


