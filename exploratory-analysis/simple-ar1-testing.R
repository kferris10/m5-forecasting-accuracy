

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
load("predictions/preds-snap-all.RData")
train_raw %>% select(1:20) %>% glimpse()

# setting up the baseline time predictions
preds_base <- bind_cols(
  select(preds_time, ends_with("id")), 
  select(preds_time, -ends_with("id")) + 
    select(preds_month, -ends_with("id")) + 
    select(preds_weekday, -ends_with("id")) + 
    select(preds_snap, -ends_with("id"))
)
rm(preds_time, preds_month, preds_weekday, preds_snap)
gc()

# quick exploratory work

f90_raw <- train_raw %>% 
  filter(item_id == "FOODS_3_090") %>% 
  pivot_longer(starts_with("d_"), 
               names_to = "day", 
               names_prefix = "d_", 
               names_transform = list(day = as.numeric), 
               values_to = "sales", 
               values_drop_na = TRUE)
f90_pred <- preds_base %>% 
  filter(item_id == "FOODS_3_090") %>% 
  rename_with(~str_replace_all(., "_time", "")) %>% 
  pivot_longer(starts_with("lpred_"), 
               names_prefix = "lpred_", 
               names_to = c(".value", "day"), 
               names_sep = "_", 
               names_transform = list(day = as.numeric),
               values_drop_na = TRUE) %>% 
  rename(lp_non0 = non0, lp_sales = sales)
f90 <- f90_raw %>% 
  inner_join(f90_pred, by = c("id", "item_id", "store_id", "day")) %>%
  left_join(select(cal, day, weekday, snap_CA:snap_WI), by = "day") %>% 
  mutate(is_snap = case_when(store_id %in% c("CA_1", "CA_2", "CA_3", "CA_4") ~ snap_CA, 
                             store_id %in% c("WI_1", "WI_2", "WI_3") ~ snap_WI, 
                             store_id %in% c("TX_1", "TX_2", "TX_3") ~ snap_TX, 
                             TRUE ~ NA_real_)) %>% 
  arrange(store_id, item_id, day) %>% 
  group_by(store_id, item_id) %>% 
  mutate(sales_l1 = lag(sales, 1), 
         lp_sales_l1 = lag(lp_sales, 1),
         sales_l2 = lag(sales, 2), 
         lp_sales_l2 = lag(lp_sales, 2), 
         sales_l7 = lag(sales, 7), 
         lp_sales_l7 = lag(lp_sales, 7), 
         sales_l365 = lag(sales, 365), 
         lp_sales_l365 = lag(lp_sales, 365)) %>% 
  ungroup()

# testing for non0 model -------------------------------------------------------

f90_non0 <- f90 %>% 
  filter(sales > 0, 
         sales_l1 > 0, 
         sales_l2 > 0, 
         sales_l7 > 0)

# dev expl% = 32% - not bad!
m_lag1 <- glm(sales ~ I(log(sales_l1) - lp_sales_l1), 
              data = f90_non0, 
              family = poisson(), 
              offset = lp_sales)
# adding in an AR(2) improves to 35% - real, but not that big of a deal
m_lag2 <- glm(sales ~ I(log(sales_l1) - lp_sales_l1) + I(log(sales_l2) - lp_sales_l2), 
              data = f90_non0, 
              family = poisson(), 
              offset = lp_sales)
# adding in AR(7) also improves to 35% - more independent than AR(2)
m_lag7 <- glm(sales ~ I(log(sales_l1) - lp_sales_l1) + I(log(sales_l7) - lp_sales_l7), 
              data = f90_non0,  
              family = poisson(), 
              offset = lp_sales)

# also testing 365 - should do something about leap years, but :shrug:
f90_non0_l365 <- f90_non0 %>% filter(sales_l365 > 0)
# some signal, but not much here
# better to just build in some custom holiday effects
m_lag365 <- glm(sales ~ I(log(sales_l1) - lp_sales_l1) + I(log(sales_l365) - lp_sales_l365), 
                data = f90_non0_l365,  
                family = poisson(), 
                offset = lp_sales)

# testing some interactions
# not much here
m_lag1_v2 <- update(m_lag1, ~ . + I(log(sales_l1) - lp_sales_l1) * weekday)

# trying on the is0 data -------------------------------------------------------

h1_raw <- train_raw %>% 
  filter(item_id == "HOBBIES_1_002") %>% 
  pivot_longer(starts_with("d_"), 
               names_to = "day", 
               names_prefix = "d_", 
               names_transform = list(day = as.numeric), 
               values_to = "sales", 
               values_drop_na = TRUE)
h1_pred <- preds_base %>% 
  filter(item_id == "HOBBIES_1_002") %>% 
  rename_with(~str_replace_all(., "_time", "")) %>% 
  pivot_longer(starts_with("lpred_"), 
               names_prefix = "lpred_", 
               names_to = c(".value", "day"), 
               names_sep = "_", 
               names_transform = list(day = as.numeric),
               values_drop_na = TRUE) %>% 
  rename(lp_non0 = non0, lp_sales = sales)
h1 <- h1_raw %>% 
  inner_join(h1_pred, by = c("id", "item_id", "store_id", "day")) %>%
  left_join(select(cal, day, weekday, snap_CA:snap_WI), by = "day") %>% 
  mutate(is_snap = case_when(store_id %in% c("CA_1", "CA_2", "CA_3", "CA_4") ~ snap_CA, 
                             store_id %in% c("WI_1", "WI_2", "WI_3") ~ snap_WI, 
                             store_id %in% c("TX_1", "TX_2", "TX_3") ~ snap_TX, 
                             TRUE ~ NA_real_)) %>% 
  arrange(store_id, item_id, day) %>% 
  group_by(store_id, item_id) %>% 
  mutate(sales_l1 = lag(sales, 1), 
         lp_non0_l1 = lag(lp_non0, 1), 
         sales_l2 = lag(sales, 2), 
         lp_non0_l2 = lag(lp_non0, 2), 
         sales_l7 = lag(sales, 7), 
         lp_non0_l7 = lag(lp_non0, 7), 
         sales_l365 = lag(sales, 365), 
         lp_non0_l365 = lag(lp_non0, 365)) %>% 
  ungroup()


# not much here - weird?
m0_lag1 <- glm(I(sales != 0) ~ I(sales_l1 != 0) * lp_non0_l1, 
               data = h1, 
               family = binomial(), 
               offset = lp_non0)
