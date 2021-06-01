
# notes ------------------------------------------------------------------------

# calculating time trend coefficients for each item
# this takes 2.5 hours, requires at least 2 cores + all my memory

# setup ------------------------------------------------------------------------

library(feather)
library(Hmisc)
library(tidyverse)
library(multidplyr) # note that this package is experimental
source("production/helper-funs.R")
options(stringsAsFactors = F, digits = 3, mc.cores = 2)

# model specifications I'll be using
f_time <- formula(~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10)

# loading data
load("predictions/preds-global.RData")
load("predictions/preds-time-store.RData")
train_raw <- read_feather("data/data-train-wide.feather")
train_raw %>% select(1:20) %>% glimpse()

# grouping by item for dplyr
train_by_item <- train_raw %>%
  # filter(item_id %in% unique(train_raw$item_id)[1:2000]) %>% 
  left_join(preds_store_time, by = c("id", "item_id", "store_id")) %>% 
  mutate(item_id2 = item_id) %>%
  group_by(item_id = item_id2) %>%
  nest(data = c(id, store_id, starts_with("d_")), 
       lpred_non0_store = c(id, store_id, starts_with("lpred_non0_store_")), 
       lpred_sales_store = c(id, store_id, starts_with("lpred_sales_store_")))
gc()

# fitting by item -------------------------------------------------------------


# setting up the cluster (for parallelization)
cluster <- new_cluster(getOption("mc.cores"))
cluster_library(cluster, c("dplyr", "tidyr", "purrr"))
cluster_assign(cluster, 
               prep_time_data = prep_time_data, 
               calc_glm_coefs = calc_glm_coefs, 
               pp_global = pp_global, 
               preds_global = preds_global, 
               extract_se = extract_se, 
               f_time = f_time)

# setting up the data for analysis
item_dat <- train_by_item %>% 
  partition(cluster) %>% 
  mutate(time_mod_data = pmap(
    list(data, lpred_non0_store, lpred_sales_store), 
    function(x, y, z) {
      prep_time_data(x, y, z, "lpred_non0_store_", "lpred_sales_store_")
    }
  )) %>% 
  # mutate(time_mod_data = map2(data, 
  #                             lpred_non0_store, 
  #                             prep_time_data, 
  #                             off_data_non0_prefix = "lpred_non0_store_")) %>% 
  select(item_id, time_mod_data) 
gc()

# fitting models and extracting coefficients
item_coefs <- item_dat %>% 
  mutate(
    m_non0_time = map(time_mod_data, calc_glm_coefs, offset = lp_non0_base, 
                      f = update(f_time, I(sales != 0) ~ .), is_pois = F), 
    m_sales_time = map(time_mod_data, calc_glm_coefs, offset = lp_sales_base, 
                       f = update(f_time, sales ~ .), is_pois = T)
  )  %>% 
  collect() %>% 
  mutate(coefs_all = map2(m_non0_time, m_sales_time, full_join, by = "term", suffix = c("_non0", "_sales"))) %>% 
  ungroup() %>% 
  select(item_id, coefs_all) %>% 
  unnest(coefs_all)

# applying RTTM to coefficients
item_coefs_regr <- item_coefs %>% 
  group_by(term) %>% 
  mutate(sd_between_non0 = sqrt(wtd.var(est_raw_non0, 1 / se_non0^2)), 
         sd_between_sales = sqrt(wtd.var(est_raw_sales, 1 / se_sales^2)), 
         est_regr_non0 = apply_rttm(est_raw_non0, se_non0, sd_between_non0), 
         est_regr_sales = apply_rttm(est_raw_sales, se_sales, sd_between_sales)) %>% 
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
item_preds <- expand_grid(item_id = unique(train_raw$item_id), day = 1:1941) %>% 
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
  mutate(across(where(is.numeric), round, digits = 4)) %>% 
  unnest(c(day, lpred_non0_item, lpred_sales_item)) %>% 
  pivot_wider(names_from = day, values_from = c(lpred_non0_item, lpred_sales_item))
preds_item_time <- train_raw %>% 
  select(id, item_id, store_id) %>% 
  left_join(item_preds, by = c("item_id"))


# checks -----------------------------------------------------------------------

# predictions get completely wonky in the tails...
expand_grid(item_id = unique(train_raw$item_id)[1:5], day = 1:2000) %>% 
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
  mutate(across(where(is.numeric), round, digits = 4)) %>% 
  unnest(c(day, lpred_non0_item, lpred_sales_item)) %>% 
  qplot(day, arm::invlogit(lpred_non0_item), data = ., geom = "line", group = item_id)

# saving -----------------------------------------------------------------------

save(item_coefs_regr, file = "fitted-models/models-time-item.RData")
save(preds_item_time, file = "predictions/preds-time-item.RData")



