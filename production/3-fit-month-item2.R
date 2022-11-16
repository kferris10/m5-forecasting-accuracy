
# notes ------------------------------------------------------------------------

# calculating monthly coefficients for each item
# this takes ~20 minutes

# setup ------------------------------------------------------------------------

library(feather)
library(Hmisc)
library(tidyverse)
library(broom)
library(foreach)
library(doParallel)
library(progress)
source("production/0-helper-funs.R")
options(stringsAsFactors = F, digits = 3, mc.cores = 4)

# loading data
cal <- read_csv("data/calendar.csv") %>% 
  mutate(day = as.numeric(str_replace_all(d, "d_", "")))
load("predictions/preds-global.RData")
load("predictions/preds-time-all.RData")
load("predictions/preds-month-store.RData")
train_raw <- read_feather("data/data-train-wide.feather")
train_raw %>% select(1:20) %>% glimpse()

# setting up the baseline time predictions
preds_base <- bind_cols(
  select(preds_time, ends_with("id")), 
  select(preds_time, -ends_with("id")) + 
    select(preds_store_month, -ends_with("id"))
)
rm(preds_time, preds_store_month)
gc()


# fitting by item -------------------------------------------------------------


#setup parallel backend to use many processors
# cl <- makeCluster(getOption("mc.cores"))
# registerDoParallel(cl)
# item_month_coefs <- foreach(i=unique(train_raw$item_id), .combine=rbind) %dopar% {
#   library(tidyverse)
#   library(broom)
  
item_month_coefs <- tibble(data.frame())
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
  cf_non0 <- if(nrow(non0_mod_data) > 1) {
    m_non0_time_i <- glm(update(f_month, I(sales != 0) ~ .), 
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
  cf_sales <- if(nrow(sales_mod_data) > 1) {
    m_sales_time_i <- glm(update(f_month, sales ~ .), 
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
  
  item_month_coefs <- bind_rows(item_month_coefs, results_i)
}
  
#   results_i
# }
# 
# # closing the clusters
# stopCluster(cl)
# stopImplicitCluster()
# gc()

# summary of results
item_month_coefs %>% 
  group_by(term) %>% 
  summarise(mu_non0 = weighted.mean(estimate_non0, 1 / std.error_non0^2, na.rm = T), 
            sd_between_non0 = sqrt(wtd.var(estimate_non0, 1 / std.error_non0^2)), 
            sd_within_non0 = sqrt(mean(std.error_non0^2, na.rm = T)), 
            sd_within_non0_med = median(std.error_non0, na.rm = T), 
            mu_sales = weighted.mean(estimate_sales, 1 / std.error_sales^2, na.rm = T), 
            sd_between_sales = sqrt(wtd.var(estimate_sales, 1 / std.error_sales^2)), 
            sd_within_sales = sqrt(mean(std.error_sales^2, na.rm = T)), 
            sd_within_non0_med = median(std.error_sales, na.rm = T)) %>% 
  mutate(across(where(is.numeric), round, digits = 2))

# applying RTTM to coefficients
item_month_coefs_regr <- item_month_coefs %>% 
  group_by(term) %>% 
  mutate(
    sd_between_non0 = sqrt(wtd.var(estimate_non0, 1 / std.error_non0^2)), 
    sd_between_sales = sqrt(wtd.var(estimate_sales, 1 / std.error_sales^2)), 
    est_regr_non0 = coalesce(apply_rttm(estimate_non0, std.error_non0, sd_between_non0), 0), 
    est_regr_sales = coalesce(apply_rttm(estimate_sales, std.error_sales, sd_between_sales), 0)
  ) %>% 
  ungroup() %>% 
  select(item_id, term, est_regr_non0, est_regr_sales) %>% 
  pivot_wider(names_from = term, values_from = c(est_regr_non0, est_regr_sales)) %>% 
  rename_with(str_replace_all, pattern = "est_regr_", replacement = "") %>% 
  mutate(across(where(is.numeric), round, digits = 4))

# generating predictions -------------------------------------------------------

item_coefs_nested <- item_month_coefs_regr %>% 
  group_by(item_id) %>% 
  nest(non0 = starts_with("non0_"), sales = starts_with("sales_"))
item_preds <- item_month_coefs_regr %>% 
  pivot_longer(cols = c(starts_with("non0"), starts_with("sales")), 
               names_to = c(".value", "month"), 
               names_sep = "_factor\\(month\\)", 
               names_transform = list(month = as.integer)) %>% 
  left_join(cal %>% mutate(day = as.numeric(str_replace_all(d, "d_", ""))), by = "month") %>% 
  arrange(item_id, day) %>% 
  select(item_id, day, non0, sales) %>% 
  rename(lpred_non0_item_month = non0, 
         lpred_sales_item_month = sales) %>% 
  pivot_wider(names_from = day, values_from = c(lpred_non0_item_month, lpred_sales_item_month))
preds_item_month <- train_raw %>% 
  select(id, item_id, store_id) %>%  
  left_join(item_preds, by = c("item_id"))

# checks -----------------------------------------------------------------------

bind_cols(
  select(preds_item_month, ends_with("id")), 
  select(preds_item_month, starts_with("lpred")) + 
    select(preds_base, starts_with("lpred"))
  )%>% 
  filter(item_id %in% c("FOODS_3_252", "FOODS_2_222", "HOBBIES_1_001"), 
         store_id %in% c("CA_1", "CA_2")) %>% 
  pivot_longer(starts_with("lpred_"), 
               names_prefix = "lpred_", 
               names_to = "day", 
               values_to = "sales")  %>% 
  mutate(type = ifelse(substr(day, 1, 4) == "non0", "non0", "sales"), 
         day = str_replace_all(day, "non0_item_month_", ""), 
         day = str_replace_all(day, "sales_item_month_", ""), 
         day = as.numeric(day)) %>% 
  pivot_wider(names_from = type, values_from = sales) %>% 
  filter(day > 500) %>% 
  left_join(preds_global, by = c("day")) %>% 
  mutate(pred_sales = arm::invlogit(non0 + lp_non0_base) * exp(sales + lp_sales_base)) %>% 
  sample_frac(.2) %>%
  qplot(day, pred_sales, data = ., colour = item_id, geom = "line") + 
  geom_vline(aes(xintercept = 1941)) + 
  facet_wrap(~store_id, scales = "free_y")

# saving -----------------------------------------------------------------------

save(item_month_coefs_regr, file = "fitted-models/models-month-item.RData")
save(preds_item_month, file = "predictions/preds-month-item.RData")


