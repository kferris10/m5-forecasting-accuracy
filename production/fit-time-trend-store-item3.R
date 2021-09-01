
# notes ------------------------------------------------------------------------

# calculating time trend coefficients for each item
# this takes ~2 hours

# setup ------------------------------------------------------------------------

library(feather)
library(tidyverse)
library(mgcv)
library(progress)
library(foreach)
library(doParallel)
library(tcltk)
source("production/helper-funs.R")
options(stringsAsFactors = F, digits = 3, mc.cores = 2)

# loading data
load("predictions/preds-global.RData")
load("predictions/preds-time-store.RData")
load("predictions/preds-time-item.RData")
cal <- read_feather("data/data-calendar-clean.feather")
train_raw <- read_feather("data/data-train-wide.feather")
train_raw %>% select(1:20) %>% glimpse()

# setting up the baseline time predictions
preds_time0 <- bind_cols(
  select(preds_item_time, ends_with("id")), 
  select(preds_item_time, -ends_with("id")) + 
    select(preds_store_time, -ends_with("id"))
)
rm(preds_item_time, preds_store_time)
gc()

# fitting by item -------------------------------------------------------------


#setup parallel backend to use many processors
cl <- makeCluster(getOption("mc.cores"))
registerDoParallel(cl)
n <- nrow(train_raw)
clusterExport(cl, c("n"))
store_item_preds <- foreach(i=icount(n), .packages = c("tidyverse", "broom", "mgcv", "tcltk"), .combine=rbind) %dopar% {
  if(!exists("pb")) pb <- tkProgressBar("Parallel task", min=1, max=n)
  setTkProgressBar(pb, i)
  

# store_item_preds <- tibble(data.frame())
# pb <- progress_bar$new(total = nrow(train_raw)-29998)
# for(i in 1:nrow(train_raw)) {
#   pb$tick()
  
  gc()
  item_i <- train_raw$item_id[i]
  store_i <- train_raw$store_id[i]
  dat_i <- train_raw %>% 
    filter(item_id == item_i, store_id == store_i)
  off_dat_i <- preds_time0 %>% 
    filter(item_id == item_i, store_id == store_i)
  
  time_mod_data_i <- prep_time_data(dat_i, 
                                    off_data = off_dat_i, 
                                    off_data_non0_prefix = "lpred_non0_item_", 
                                    off_data_sales_prefix = "lpred_sales_item_")
  
  # fitting models
  k_use <- ifelse(i %in% c(2966, 26924), 6, 8)
  m_non0_time_i <- gam(I(sales != 0) ~ s(day, k = k_use, bs = "ts"), 
                       data = time_mod_data_i, 
                       family = binomial(), 
                       offset = lp_non0_base)
  m_sales_time_i <- gam(update(f_time, sales ~ .), 
                        data = time_mod_data_i %>% filter(sales > 0), 
                        family = poisson(), 
                        offset = lp_sales_base)
  
  store_item_preds_i <- tibble(item_id = item_i, 
                               store_id = store_i, 
                               day_raw = 1:1969) %>% 
    mutate(day = pmin(1941, day_raw)) %>% 
    mutate(lpred_non0_store_item = as.numeric(predict(m_non0_time_i, newdata = .)), 
           lpred_sales_store_item = as.numeric(predict(m_sales_time_i, newdata = .))) %>% 
    mutate(day = day_raw) %>% 
    select(-day_raw) %>% 
    pivot_wider(names_from = day, values_from = c(lpred_non0_store_item, lpred_sales_store_item))

  # store_item_preds <- bind_rows(store_item_preds, store_item_preds_i)
# }

  store_item_preds_i
}
# closing the clusters
stopCluster(cl)
stopImplicitCluster()
gc()

preds_store_item_time <- train_raw %>% 
  select(id, item_id, store_id) %>% 
  left_join(store_item_preds, by = c("item_id", "store_id")) %>% 
  mutate(across(where(is.numeric), round, digits = 4))

# checks -----------------------------------------------------------------------

train_262 <- train_raw %>% 
  filter(item_id == "FOODS_3_262") %>% 
  pivot_longer(starts_with("d_"), 
               names_prefix = "d_", 
               names_to = "day", 
               names_transform = list(day = as.integer), 
               values_to = "sales", 
               values_drop_na = TRUE)

bind_cols(preds_store_item_time %>% select(1:3), 
          preds_store_item_time[, 4:3941] + preds_time0[, 4:3941]) %>% 
  filter(item_id == "FOODS_3_262") %>% 
  rename_with(~str_replace_all(., "_store_item", "")) %>% 
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
  qplot(day, pred_sales, data = ., colour = item_id, geom = "line") +
  # qplot(total_pred, total_sales, data = .) + geom_abline()
  geom_vline(aes(xintercept = 1941))

# saving -----------------------------------------------------------------------


preds_time <- bind_cols(
  select(preds_time0, 1:3), 
  select(preds_time0, starts_with("lpred")) + 
    select(preds_store_item_time, starts_with("lpred"))
) %>% 
  mutate(across(where(is.numeric), round, digits = 4)) %>% 
  rename_with(str_replace_all, pattern = "_store_item_", replacement = "_time_") %>% 
  rename_with(str_replace_all, pattern = "_store_", replacement = "_time_") %>% 
  rename_with(str_replace_all, pattern = "_item_", replacement = "_time_")

save(preds_store_item_time, file = "predictions/preds-time-store-item.RData")
save(preds_time, file = "predictions/preds-time-all.RData")





