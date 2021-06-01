
# notes ------------------------------------------------------------------------

# calculating time trend coefficients for each store
# this takes < 10 minutes, requires at least 2 cores + all my memory

# setup ------------------------------------------------------------------------

library(feather)
library(Hmisc)
library(tidyverse)
library(multidplyr) # note that this package is experimental
source("production/helper-funs.R")
options(stringsAsFactors = F, digits = 3, mc.cores = 2)

# model specifications I'll be using
f_time <- formula(~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10)
f_non0_time <- update(f_time, I(sales != 0) ~ .)
f_sales_time <- update(f_time, sales ~ .)

# loading data
load("fitted-models/models-global.RData")
cal <- read_csv("data/calendar.csv") %>%
  mutate(d = as.numeric(str_replace_all(d, "d_", ""))) %>%
  rename(day = d) %>%
  select(day, month, weekday)
train_raw <- read_feather("data/data-train-wide.feather")
train_raw %>% select(1:20) %>% glimpse()

# grouping by store for dplyr
train_by_store <- train_raw %>%
  select(-id, -item_id) %>% 
  mutate(store_id2 = store_id) %>%
  group_by(store_id = store_id2) %>%
  nest() 


# fitting by store -------------------------------------------------------------


# setting up the cluster (for parallelization)
cluster <- new_cluster(getOption("mc.cores"))
cluster_library(cluster, c("dplyr", "tidyr", "purrr"))
cluster_assign(cluster, 
               prep_time_data = prep_time_data, 
               calc_glm_coefs = calc_glm_coefs, 
               cal = cal, 
               pp = pp, 
               extract_se = extract_se, 
               f_non0_time = f_non0_time, 
               f_sales_time = f_sales_time, 
               m_non0_base = m_non0_base, 
               m_sales_base = m_sales_base)

# setting up the data for analysis
store_dat <- train_by_store %>% 
  # ungroup() %>% slice(1:4) %>% group_by(store_id) %>% 
  partition(cluster) %>% 
  mutate(time_mod_data = map(data, prep_time_data))

# fitting models and extracting coefficients
store_coefs <- store_dat %>% 
  mutate(
    m_non0_time = map(time_mod_data, calc_glm_coefs, offset = lpred_non0_base, f = f_non0_time, is_pois = F), 
    m_sales_time = map(time_mod_data, calc_glm_coefs, offset = lpred_sales_base, f = f_sales_time, is_pois = T)
  )  %>% 
  collect() %>% 
  mutate(coefs_all = map2(m_non0_time, m_sales_time, full_join, by = "term", suffix = c("_non0", "_sales"))) %>% 
  ungroup() %>% 
  select(store_id, coefs_all) %>% 
  unnest(coefs_all)

# applying RTTM to coefficients
store_coefs_regr <- store_coefs %>% 
  group_by(term) %>% 
  mutate(sd_between_non0 = sqrt(wtd.var(est_raw_non0, 1 / se_non0^2)), 
         sd_between_sales = sqrt(wtd.var(est_raw_sales, 1 / se_sales^2)), 
         est_regr_non0 = apply_rttm(est_raw_non0, se_non0, sd_between_non0), 
         est_regr_sales = apply_rttm(est_raw_sales, se_sales, sd_between_sales)) %>% 
  ungroup() %>% 
  select(store_id, term, est_regr_non0, est_regr_sales) %>% 
  pivot_wider(names_from = term, values_from = c(est_regr_non0, est_regr_sales)) %>% 
  rename_with(str_replace_all, pattern = "est_regr_", replacement = "")

# checks -----------------------------------------------------------------------


# saving -----------------------------------------------------------------------

save(store_coefs_regr, file = "fitted-models/models-time-store.RData")



