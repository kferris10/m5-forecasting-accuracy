
# notes ------------------------------------------------------------------------

# calculating time trend coefficients for each item
# this takes ~30 minutes

# setup ------------------------------------------------------------------------

library(feather)
library(Hmisc)
library(tidyverse)
library(broom)
library(progress)
source("production/helper-funs.R")
options(stringsAsFactors = F, digits = 3, mc.cores = 2)

# model specifications I'll be using
f_time <- formula(~ X1 + X2 + X3 + X4)

# loading data
load("predictions/preds-global.RData")
load("predictions/preds-time-store.RData")
train_raw <- read_feather("data/data-train-wide.feather")
train_raw %>% select(1:20) %>% glimpse()

# fitting by item -------------------------------------------------------------


item_coefs <- tibble(data.frame())

pb <- progress_bar$new(total = length(unique(train_raw$item_id)))
for(i in unique(train_raw$item_id)) {
  pb$tick()
  dat_i <- train_raw %>% filter(item_id == i)
  off_dat_i <- preds_store_time %>% filter(item_id == i)
  
  # fitting the models
  time_mod_data_i <- prep_time_data(dat_i, 
                                    off_data = off_dat_i, 
                                    off_data_non0_prefix = "lpred_non0_store_", 
                                    off_data_sales_prefix = "lpred_sales_store_")
  m_non0_time_i <- glm(update(f_time, I(sales != 0) ~ .), 
                       time_mod_data_i, 
                       family = binomial(), 
                       offset = lp_non0_base)
  m_sales_time_i <- glm(update(f_time, sales ~ .), 
                       time_mod_data_i %>% filter(sales > 0), 
                       family = poisson(), 
                       offset = lp_sales_base)
  
  # combining the results into a nice dataset
  results_i <- tidy(m_non0_time_i) %>% 
    left_join(tidy(m_sales_time_i), by = "term", suffix = c("_non0", "_sales")) %>% 
    mutate(item_id = i)
  
  item_coefs <- bind_rows(item_coefs, results_i)
}


# applying RTTM to coefficients
item_coefs_regr <- item_coefs %>% 
  group_by(term) %>% 
  mutate(sd_between_non0 = sqrt(wtd.var(estimate_non0, 1 / std.error_non0^2)), 
         sd_between_sales = sqrt(wtd.var(estimate_sales, 1 / std.error_sales^2)), 
         est_regr_non0 = apply_rttm(estimate_non0, std.error_non0, sd_between_non0), 
         est_regr_sales = apply_rttm(estimate_sales, std.error_sales, sd_between_sales)) %>% 
  ungroup() %>% 
  # organize into a nicer format
  select(item_id, term, est_regr_non0, est_regr_sales) %>% 
  pivot_wider(names_from = term, values_from = c(est_regr_non0, est_regr_sales)) %>% 
  rename_with(str_replace_all, pattern = "est_regr_", replacement = "") %>% 
  mutate(across(where(is.numeric), round, digits = 4))


# generating predictions -------------------------------------------------------

item_coefs_nested <- item_coefs_regr %>% 
  group_by(item_id) %>% 
  nest(non0 = starts_with("non0_"), sales = starts_with("sales_"))
item_preds <- expand_grid(item_id = unique(train_raw$item_id), 
                          day = pp_global$day) %>% 
  left_join(pp_global, by = "day") %>% 
  group_by(item_id) %>% 
  nest(day = day, pp = starts_with("X")) %>% 
  left_join(item_coefs_nested, by = "item_id") %>% 
  mutate(
    lpred_non0_item = map2(pp, non0, ~ as.numeric(cbind(1, as.matrix(.x)) %*% as.numeric(.y))), 
    lpred_sales_item = map2(pp, sales, ~ as.numeric(cbind(1, as.matrix(.x)) %*% as.numeric(.y)))
  ) %>% 
  ungroup() %>% 
  select(item_id, day, lpred_non0_item, lpred_sales_item) %>% 
  unnest(c(day, lpred_non0_item, lpred_sales_item)) %>% 
  mutate(across(where(is.numeric), round, digits = 4)) %>%
  pivot_wider(names_from = day, values_from = c(lpred_non0_item, lpred_sales_item))
preds_item_time <- train_raw %>% 
  select(id, item_id, store_id) %>% 
  left_join(item_preds, by = c("item_id")) 


# checks -----------------------------------------------------------------------

# predictions get completely wonky in the tails...
expand_grid(item_id = sample(train_raw$item_id, 5), day = pp_global$day) %>% 
  left_join(pp_global, by = "day") %>% 
  group_by(item_id) %>% 
  nest(day = day, pp = starts_with("X")) %>% 
  left_join(item_coefs_nested, by = "item_id") %>% 
  mutate(
    lpred_non0_item = map2(pp, non0, ~ as.numeric(cbind(1, as.matrix(.x)) %*% as.numeric(.y))), 
    lpred_sales_item = map2(pp, sales, ~ as.numeric(cbind(1, as.matrix(.x)) %*% as.numeric(.y)))
  ) %>% 
  ungroup() %>% 
  select(item_id, day, lpred_non0_item, lpred_sales_item) %>% 
  unnest(c(day, lpred_non0_item, lpred_sales_item)) %>% 
  sample_frac(.2) %>% 
  left_join(preds_global, by = c("day")) %>% 
  mutate(pred_sales = arm::invlogit(lpred_non0_item + lp_non0_base) * exp(lpred_sales_item + lp_sales_base)) %>% 
  qplot(day, pred_sales, data = ., geom = "line", group = item_id)

# saving -----------------------------------------------------------------------

save(item_coefs_regr, file = "fitted-models/models-time-item.RData")
save(preds_item_time, file = "predictions/preds-time-item.RData")



