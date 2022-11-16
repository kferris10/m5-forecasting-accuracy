
# notes ------------------------------------------------------------------------

# calculating time trend coefficients for each store
# this takes ~10 hours

# setup ------------------------------------------------------------------------

library(feather)
library(Hmisc)
library(tidyverse)
library(broom)
library(mgcv)
library(progress)
source("production/0-helper-funs.R")
options(stringsAsFactors = F, digits = 3, mc.cores = 3)

# loading data
cal <- read_feather("data/data-calendar-clean.feather")
train_raw <- read_feather("data/data-train-wide.feather")
train_raw %>% select(1:20) %>% glimpse()
load("predictions/preds-global.RData")

# fitting by store -------------------------------------------------------------

store_preds <- tibble(data.frame())

pb <- progress_bar$new(total = length(unique(train_raw$store_id)))
for(i in unique(train_raw$store_id)) {
  pb$tick()
  gc()
  
  dat_i <- train_raw %>% filter(store_id == i)
  
  # fitting models
  time_mod_data_i <- prep_time_data(dat_i)
  m_non0_time_i <- gam(update(f_time, I(sales != 0) ~ .), 
                       data = time_mod_data_i, 
                       family = binomial(), 
                       offset = lp_non0_base)
  m_sales_time_i <- gam(update(f_time, sales ~ .), 
                        data = time_mod_data_i %>% filter(sales > 0), 
                        family = poisson(), 
                        offset = lp_sales_base)
  
  store_preds_i <- tibble(store_id = i, 
                          day_raw = 1:1969) %>% 
    mutate(day = pmin(1941, day_raw)) %>% 
    mutate(lpred_non0_store = as.numeric(predict(m_non0_time_i, newdata = .)), 
           lpred_sales_store = as.numeric(predict(m_sales_time_i, newdata = .))) %>% 
    mutate(day = day_raw) %>% 
    select(-day_raw) %>% 
    pivot_wider(names_from = day, values_from = c(lpred_non0_store, lpred_sales_store))
  
  store_preds <- bind_rows(store_preds, store_preds_i) 
}

preds_store_time <- train_raw %>% 
  select(id, item_id, store_id) %>% 
  inner_join(store_preds, by = c("store_id"))

# checks -----------------------------------------------------------------------

preds_store_time %>% 
  filter(item_id == "FOODS_3_262") %>% 
  pivot_longer(starts_with("lpred_"), names_prefix = "lpred_", names_to = "day", values_to = "sales")  %>% 
  mutate(type = ifelse(substr(day, 1, 4) == "non0", "non0", "sales"), 
         day = str_replace_all(day, "non0_store_", ""), 
         day = str_replace_all(day, "sales_store_", ""), 
         day = as.numeric(day)) %>% 
  pivot_wider(names_from = type, values_from = sales) %>% 
  left_join(preds_global, by = c("day")) %>% 
  mutate(pred_sales = arm::invlogit(non0 + lp_non0_base) * exp(sales + lp_sales_base)) %>% 
  sample_frac(.2) %>%
  qplot(day, pred_sales, data = ., colour = store_id, geom = "line") + 
  geom_vline(aes(xintercept = 1941)) + 
  ylim(c(0, 5))

# saving -----------------------------------------------------------------------

save(preds_store_time, file = "predictions/preds-time-store.RData")


