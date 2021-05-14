
# notes ------------------------------------------------------------------

# automatic gc()?
# built in progress bar?
# why does the rowwise one take up so much memory

# setup ------------------------------------------------------------------------

library(feather)
library(Hmisc)
library(tidyverse)
library(multidplyr)
source("production/helper-funs.R")

load("fitted-models/models-global.RData")
cal <- read_csv("data/calendar.csv") %>%
  mutate(d = as.numeric(str_replace_all(d, "d_", ""))) %>%
  rename(day = d) %>%
  select(date, day, year, month, weekday)
train_raw <- read_feather("data/data-train-wide.feather")
train_raw %>% select(1:20) %>% glimpse()

# train_by_item <- train_raw %>% 
#   mutate(item_id2 = item_id) %>% 
#   group_by(item_id2) %>% 
#   nest() %>% 
#   rowwise()
# train_by_store <- train_raw %>% 
#   mutate(store_id2 = store_id) %>% 
#   group_by(store_id2) %>% 
#   nest() %>% 
#   rowwise()

# model specification I'll be using
f_time <- formula(~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10)
f_non0_time <- update(f_time, I(sales != 0) ~ .)

mod_fun <- function(data, pb = NULL) {
  # need to manually fit the models and a ton of gc()s
  # because my laptop doesn't have much memory
  
  gc()
  if(!is.null(pb)) pb$tick()
  
  df <- data  %>% 
    pivot_longer(starts_with("d_"), 
                 names_to = "day", 
                 names_prefix = "d_", 
                 names_transform = list(day = as.numeric), 
                 values_to = "sales", 
                 values_drop_na = TRUE) %>% 
    left_join(cal, by = "day") %>%
    bind_cols(data.frame(predict(pp, .$day))) %>% 
    mutate(lpred_non0_base = predict(m_non0_base, newdata = .))
  
  gc()
  X <- model.matrix(delete.response(terms(f_non0_time)), df)
  y <- model.response(model.frame(f_non0_time, df))
  m <- glm.fit(X, y, family = binomial(), offset = df$lpred_non0_base)
  coefs <- tibble(term = names(m$coefficients), est_raw = coef(m), se = extract_se(m))
  
  gc()
  coefs
}

# modelling in parallel ------------------------------------------


cluster <- new_cluster(5)
cluster_library(cluster, c("dplyr", "tidyr"))
cluster_assign(cluster, 
               mod_fun = mod_fun, 
               cal = cal, 
               pp = pp, 
               extract_se = extract_se, 
               f_non0_time = f_non0_time, 
               m_non0_base = m_non0_base)

# this takes about 10 minutes
gc()
item_coefs <- train_by_item %>% 
  partition(cluster) %>% 
  mutate(model_dat2 = list(mod_fun(data))) %>% 
  collect()
rm(cluster)
gc()

ic <- item_coefs %>% 
  select(-data) %>% 
  unnest() %>% 
  group_by(term) %>% 
  mutate(bt_sd = sqrt(wtd.var(est_raw, 1 / se^2))) %>% 
  ungroup() %>% 
  mutate(est_regr = apply_rttm(est_raw, se, bt_sd), 
         across(where(is.numeric), round, digits = 4)) %>% 
  select(item_id2, term, est_regr) %>% 
  pivot_wider(names_from = term, values_from = est_regr)

expand_grid(item_id = sample(unique(train_raw$item_id), 8), 
            day = 800:1981) %>% 
  left_join(cal %>% select(day, date), by = "day") %>% 
  mutate(day_raw = day, day = pmin(day, 1941)) %>% 
  bind_cols(data.frame(predict(pp, .$day))) %>% 
  left_join(id, by = c("item_id" = "item_id")) %>% 
  mutate(lpred = `(Intercept)` + 
           X1.x * X1.y + 
           X2.x * X2.y + 
           X3.x * X3.y + 
           X4.x * X4.y + 
           X5.x * X5.y + 
           X6.x * X6.y + 
           X7.x * X7.y + 
           X8.x * X8.y + 
           X9.x * X9.y + 
           X10.x * X10.y, 
         type = ifelse(coalesce(date > as.Date("2016-05-22"), TRUE), "oos", "train")) %>% 
  mutate(day = day_raw) %>% 
  qplot(day, arm::invlogit(lpred), data = ., geom = "line", linetype = type) + 
  facet_wrap(~item_id)

cluster <- new_cluster(5)
cluster_library(cluster, c("dplyr", "tidyr"))
cluster_assign(cluster, 
               mod_fun = mod_fun, 
               cal = cal, 
               pp = pp, 
               extract_se = extract_se, 
               f_non0_time = f_non0_time, 
               m_non0_base = m_non0_base)


# also about 10 minutes
gc()
store_coefs <- train_by_store %>% 
  partition(cluster) %>% 
  mutate(model_dat2 = list(mod_fun(data))) %>% 
  collect()
rm(cluster)
gc()


sc <- store_coefs %>% 
  select(-data) %>% 
  unnest() %>% 
  group_by(term) %>% 
  mutate(bt_sd = sqrt(wtd.var(est_raw, 1 / se^2))) %>% 
  ungroup() %>% 
  mutate(est_regr = apply_rttm(est_raw, se, bt_sd), 
         across(where(is.numeric), round, digits = 4))  %>% 
  select(store_id2, term, est_regr) %>% 
  pivot_wider(names_from = term, values_from = est_regr)


expand_grid(store_id = unique(train_raw$store_id), 
            day = 800:1981) %>% 
  left_join(cal %>% select(day, date), by = "day") %>% 
  mutate(day_raw = day, day = pmin(day, 1941)) %>% 
  bind_cols(data.frame(predict(pp, .$day))) %>% 
  left_join(sc, by = c("store_id" = "store_id2")) %>% 
  mutate(lpred = `(Intercept)` + 
           X1.x * X1.y + 
           X2.x * X2.y + 
           X3.x * X3.y + 
           X4.x * X4.y + 
           X5.x * X5.y + 
           X6.x * X6.y + 
           X7.x * X7.y + 
           X8.x * X8.y + 
           X9.x * X9.y + 
           X10.x * X10.y, 
         type = ifelse(coalesce(date > as.Date("2016-05-22"), TRUE), "oos", "train")) %>% 
  mutate(day = day_raw) %>% 
  qplot(day, arm::invlogit(lpred), data = ., geom = "line", linetype = type) + 
  facet_wrap(~store_id)

# modelling by store/item -----------------------------------------------


cluster <- new_cluster(4)
cluster_library(cluster, c("dplyr", "tidyr"))
cluster_assign(cluster, 
               mod_fun = mod_fun, 
               cal = cal, 
               pp = pp, 
               extract_se = extract_se, 
               f_non0_time = f_non0_time, 
               m_non0_base = m_non0_base)

train_by_store_item <- train_raw %>% 
  mutate(store_id2 = store_id, item_id2 = item_id) %>% 
  group_by(store_id, item_id) %>% 
  nest() %>% 
  rowwise()


# 
gc()
t1 <- Sys.time()
store_item_coefs <- train_by_store_item %>% 
  slice(c(1:6000, 7000:8000)) %>% 
  partition(cluster) %>% 
  mutate(model_dat2 = list(mod_fun(data))) %>% 
  collect()
rm(cluster)
gc()
t2 <- Sys.time()
