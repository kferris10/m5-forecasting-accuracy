
# notes ------------------------------------------------------------------------

# calculating time trend coefficients for each item
# this takes < 1 hour, a lot of memory

# setup ------------------------------------------------------------------------

library(feather)
library(Hmisc)
library(tidyverse)
library(broom)
library(progress)
library(foreach)
library(doParallel)
source("production/helper-funs.R")
options(stringsAsFactors = F, digits = 3, mc.cores = 3)

# model specifications I'll be using
f_time <- formula(~ X1 + X2 + X3 + X4)

# loading data
load("predictions/preds-global.RData")
load("predictions/preds-time-store.RData")
load("predictions/preds-time-item.RData")
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

store_item_coefs <- foreach(i=1:nrow(train_raw), .combine=rbind) %dopar% {
  library(tidyverse)
  library(broom)
  
  dat_i <- train_raw %>% 
    filter(item_id == train_raw$item_id[i], store_id == train_raw$store_id[i])
  off_dat_i <- preds_time0 %>% 
    filter(item_id == train_raw$item_id[i], store_id == train_raw$store_id[i])
  
  # fitting the models
  time_mod_data_i <- prep_time_data(dat_i, 
                                    off_data = off_dat_i, 
                                    off_data_non0_prefix = "lpred_non0_item_", 
                                    off_data_sales_prefix = "lpred_sales_item_")
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
    mutate(item_id = train_raw$item_id[i], 
           store_id = train_raw$store_id[i])
  
  results_i
}

# applying RTTM to coefficients
store_item_coefs_regr <- store_item_coefs %>% 
  group_by(term) %>% 
  mutate(sd_between_non0 = sqrt(wtd.var(estimate_non0, 1 / std.error_non0^2)), 
         sd_between_sales = sqrt(wtd.var(estimate_sales, 1 / std.error_sales^2)), 
         est_regr_non0 = apply_rttm(estimate_non0, std.error_non0, sd_between_non0), 
         est_regr_sales = apply_rttm(estimate_sales, std.error_sales, sd_between_sales)) %>% 
  ungroup() %>% 
  # organize into a nicer format
  select(store_id, item_id, term, est_regr_non0, est_regr_sales) %>% 
  pivot_wider(names_from = term, values_from = c(est_regr_non0, est_regr_sales)) %>% 
  rename_with(str_replace_all, pattern = "est_regr_", replacement = "") %>% 
  mutate(across(where(is.numeric), round, digits = 4))

# closing the clusters
stopCluster(cl)
stopImplicitCluster()
gc()

# issues with some absolutely massive coefficients
store_item_coefs_regr %>% 
  pivot_longer(-c(store_id, item_id), names_to = "param", values_to = "est") %>% 
  ggplot(aes(x = log(abs(est)))) + 
  geom_histogram(binwidth = 1, colour = "darkgreen", fill = "grey", alpha = 0.3) + 
  facet_wrap(~param, scales = "free_x")

# generating predictions -------------------------------------------------------

store_item_coefs_nested <- store_item_coefs_regr %>% 
  group_by(store_id, item_id) %>% 
  nest(non0 = starts_with("non0_"), sales = starts_with("sales_"))
preds_store_item_time <- expand_grid(store_id = unique(train_raw$store_id), 
                                item_id = unique(train_raw$item_id), 
                                day = pp_global$day) %>% 
  left_join(pp_global, by = "day") %>% 
  group_by(store_id, item_id) %>% 
  nest(day = day, pp = starts_with("X")) %>% 
  left_join(store_item_coefs_nested, by = c("item_id", "store_id")) %>% 
  mutate(
    lpred_non0_store_item = map2(pp, non0, ~ as.numeric(cbind(1, as.matrix(.x)) %*% as.numeric(.y))), 
    lpred_sales_store_item = map2(pp, sales, ~ as.numeric(cbind(1, as.matrix(.x)) %*% as.numeric(.y)))
  ) %>% 
  ungroup() %>% 
  select(store_id, item_id, day, lpred_non0_store_item, lpred_sales_store_item) %>% 
  unnest(c(day, lpred_non0_store_item, lpred_sales_store_item)) %>% 
  mutate(across(where(is.numeric), round, digits = 4)) %>%
  pivot_wider(names_from = day, values_from = c(lpred_non0_store_item, lpred_sales_store_item))


# checks -----------------------------------------------------------------------

# predictions get completely wonky in the tails...
preds_new <- expand_grid(item_id = c(sample(train_raw$item_id, 3), "FOODS_3_555"), 
            store_id = c("TX_1", "CA_1"), 
            day = pp_global$day) %>% 
  left_join(pp_global, by = "day") %>% 
  group_by(store_id, item_id) %>% 
  nest(day = day, pp = starts_with("X")) %>% 
  left_join(store_item_coefs_nested, by = c("store_id", "item_id")) %>% 
  mutate(
    lpred_non0_item = map2(pp, non0, ~ as.numeric(cbind(1, as.matrix(.x)) %*% as.numeric(.y))), 
    lpred_sales_item = map2(pp, sales, ~ as.numeric(cbind(1, as.matrix(.x)) %*% as.numeric(.y)))
  ) %>% 
  ungroup() %>% 
  select(store_id, item_id, day, lpred_non0_item, lpred_sales_item) %>% 
  unnest(c(day, lpred_non0_item, lpred_sales_item))

preds_old <- preds_time0 %>% 
  filter(store_id %in% preds_new$store_id, item_id %in% preds_new$item_id) %>% 
  pivot_longer(cols = starts_with("lpred"), 
               names_to = "day", 
               names_prefix = "lpred_", 
               values_to = c("lpred_old")) %>% 
  mutate(day = str_replace_all(day, "_item", "")) %>% 
  separate(day, c("type", "day")) %>% 
  mutate(day = as.numeric(day), 
         type = paste("lpred", type, "old", sep = "_")) %>% 
  pivot_wider(names_from = type, values_from = lpred_old)

preds_new %>% 
  sample_frac(.2)  %>% 
  left_join(preds_global, by = c("day")) %>% 
  left_join(preds_old, by = c("item_id", "store_id", "day")) %>% 
  mutate(pred_non0 = arm::invlogit(lpred_non0_item + lpred_non0_old + lp_non0_base), 
         pred_sales_non0 = exp(lpred_sales_item + lp_sales_base + lpred_sales_old), 
         pred_sales = pred_non0 * pred_sales_non0) %>% 
  qplot(day, pred_sales, data = ., geom = "line", colour = store_id, group = item_id) + 
  facet_wrap(~item_id, scales = "free_y")

# saving -----------------------------------------------------------------------

# reloading raw predictions - memory issues suck!
load("predictions/preds-time-store.RData")
load("predictions/preds-time-item.RData")

preds_time <- bind_cols(
  select(preds_time0, 1:3), 
  select(preds_store_time, starts_with("lpred")) + 
    select(preds_item_time, starts_with("lpred")) + 
    select(preds_store_item_time, starts_with("lpred"))
) %>% 
  mutate(across(where(is.numeric), round, digits = 4)) %>% 
  rename_with(str_replace_all, pattern = "_store_", replacement = "_time_")


save(store_item_coefs_regr, file = "fitted-models/models-time-store-item.RData")
save(preds_store_item_time, file = "predictions/preds-time-store-item.RData")
save(preds_time, file = "predictions/preds-time-all.RData")



