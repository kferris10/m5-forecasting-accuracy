
# notes ------------------------------------------------------------------------

# calculating time trend coefficients for each item
# this takes ~1 hour

# setup ------------------------------------------------------------------------

library(feather)
library(tidyverse)
library(mgcv)
library(progress)
library(foreach)
library(doParallel)
library(tcltk)
source("production/0-helper-funs.R")
options(stringsAsFactors = F, digits = 3, mc.cores = 3)

# loading data
load("predictions/preds-global.RData")
load("predictions/preds-time-store.RData")
cal <- read_feather("data/data-calendar-clean.feather")
train_raw <- read_feather("data/data-train-wide.feather")
train_raw %>% select(1:20) %>% glimpse()

# fitting by item -------------------------------------------------------------


#setup parallel backend to use many processors
# cl <- makeCluster(getOption("mc.cores"))
# registerDoParallel(cl)
# items <- unique(train_raw$item_id)
# n <- length(items)
# clusterExport(cl, c("n"))
# item_preds <- foreach(i=icount(n), .packages = c("tidyverse", "broom", "mgcv", "tcltk"), .combine=rbind) %dopar% {
#   if(!exists("pb")) pb <- tkProgressBar("Parallel task", min=1, max=n)
#   setTkProgressBar(pb, i)
  

item_preds <- tibble(data.frame())
pb <- progress_bar$new(total = length(unique(train_raw$item_id)))
for(i in unique(train_raw$item_id)) {
  pb$tick()
  gc()
  
  dat_i <- train_raw %>% filter(item_id == i)
  off_dat_i <- preds_store_time %>% filter(item_id == i)
  
  time_mod_data_i <- prep_time_data(dat_i, 
                                    off_data = off_dat_i, 
                                    off_data_non0_prefix = "lpred_non0_store_", 
                                    off_data_sales_prefix = "lpred_sales_store_")
  
  # fitting models
  m_non0_time_i <- gam(update(f_time, I(sales != 0) ~ .), 
                       data = time_mod_data_i, 
                       family = binomial(), 
                       offset = lp_non0_base)
  m_sales_time_i <- gam(update(f_time, sales ~ .), 
                        data = time_mod_data_i %>% filter(sales > 0), 
                        family = poisson(), 
                        offset = lp_sales_base)
  
  item_preds_i <- tibble(item_id = i, 
                          day_raw = 1:1969) %>% 
    mutate(day = pmin(1941, day_raw)) %>% 
    mutate(lpred_non0_item = as.numeric(predict(m_non0_time_i, newdata = .)), 
           lpred_sales_item = as.numeric(predict(m_sales_time_i, newdata = .))) %>% 
    mutate(day = day_raw) %>% 
    select(-day_raw) %>% 
    pivot_wider(names_from = day, values_from = c(lpred_non0_item, lpred_sales_item))
  
    item_preds <- bind_rows(item_preds, item_preds_i)
  }
  
#   results_i
# }
# 
# # closing the clusters
# stopCluster(cl)
# stopImplicitCluster()
# gc()

preds_item_time <- train_raw %>% 
  select(id, item_id, store_id) %>% 
  inner_join(item_preds, by = c("item_id"))

# checks -----------------------------------------------------------------------

train_262 <- train_raw %>% 
  filter(item_id == "FOODS_3_262") %>% 
  pivot_longer(starts_with("d_"), 
               names_prefix = "d_", 
               names_to = "day", 
               names_transform = list(day = as.integer), 
               values_to = "sales", 
               values_drop_na = TRUE)

preds_item_time %>% 
  filter(item_id == "FOODS_3_262") %>% 
  rename_with(~str_replace_all(., "_item", "")) %>% 
  pivot_longer(starts_with("lpred_"), 
               names_prefix = "lpred_", 
               names_to = c(".value", "day"), 
               names_sep = "_", 
               names_transform = list(day = as.integer)) %>% 
  left_join(preds_global, by = c("day")) %>% 
  mutate(pred_sales = arm::invlogit(non0 + lp_non0_base) * exp(sales + lp_sales_base)) %>% 
  # inner_join(train_262, by = c("id", "day"), suffix = c("", "_act")) %>%
  # mutate(total_sales = cumsum(sales_act),
  #        total_pred = cumsum(pred_sales)) %>%
  sample_frac(.2) %>%
  # qplot(total_pred, total_sales, data = .) + geom_abline()
  qplot(day, pred_sales, data = ., colour = item_id, geom = "line") +
  geom_vline(aes(xintercept = 1941))

# saving -----------------------------------------------------------------------

save(preds_item_time, file = "predictions/preds-time-item.RData")


