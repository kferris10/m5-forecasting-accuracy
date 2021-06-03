
# notes ------------------------------------------------------------------------

# calculating time trend coefficients for each store/item
# takes a couple hours

# setup ------------------------------------------------------------------------

library(feather)
library(Hmisc)
library(tidyverse)
library(broom)
library(mgcv)
library(progress)
library(foreach)
library(doParallel)
source("production/helper-funs.R")
options(stringsAsFactors = F, digits = 3, mc.cores = 3)

# loading data
cal <- read_feather("data/data-calendar-clean.feather")
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

# setup parallel backend to use many processors
cl <- makeCluster(getOption("mc.cores"))
registerDoParallel(cl)
n <- nrow(train_raw)
clusterExport(cl, c("n"))
store_item_coefs <- foreach(i=icount(n), .packages = c("tidyverse", "broom", "mgcv", "tcltk"), .combine=rbind) %dopar% {
  if(!exists("pb")) pb <- tkProgressBar("Parallel task", min=1, max=n)
  setTkProgressBar(pb, i)

# # breaks for 955, 9012, 17981, 26924
# store_item_coefs <- tibble(data.frame())
# pb <- progress_bar$new(total = nrow(train_raw))
# for(i in 1:nrow(train_raw)) {
#   pb$tick()
  
  dat_i <- train_raw %>% 
    filter(item_id == train_raw$item_id[i], store_id == train_raw$store_id[i])
  off_dat_i <- preds_time0 %>% 
    filter(item_id == train_raw$item_id[i], store_id == train_raw$store_id[i])
  
  # fitting the models
  time_mod_data_i <- prep_time_data(dat_i, 
                                    off_data = off_dat_i, 
                                    off_data_non0_prefix = "lpred_non0_item_", 
                                    off_data_sales_prefix = "lpred_sales_item_")

  # fitting gams initially
  k <- ifelse(i %in% c(955, 9012, 17981, 26924), 6, 8)
  gam_non0_i <- time_mod_data_i %>% 
    filter(between(lp_non0_base, -7, 7)) %>% 
    gam(I(sales != 0) ~ s(day, bs = "ts", k = k), 
        data = ., 
        family = binomial(), 
        offset = lp_non0_base)
  gam_sales_i <- time_mod_data_i %>% 
    filter(sales > 0, between_lp_sales_base, -8, 8) %>% 
    gam(sales ~ s(day, bs = "ts", k = k), 
        data = ., 
        family = poisson(), 
        offset = lp_sales_base)
  time_mod_data_i$gam_pred_non0 <- as.numeric(predict(gam_non0_i, newdata = time_mod_data_i))
  time_mod_data_i$gam_se_non0 <- as.numeric(predict(gam_non0_i, newdata = time_mod_data_i, se.fit = TRUE)$se.fit)
  time_mod_data_i$gam_pred_sales <- as.numeric(predict(gam_sales_i, newdata = time_mod_data_i))
  time_mod_data_i$gam_se_sales <- as.numeric(predict(gam_sales_i, newdata = time_mod_data_i, se.fit = TRUE)$se.fit)
  time_mod_data_i$gam_non0 <- rnorm(nrow(time_mod_data_i), time_mod_data_i$gam_pred_non0, time_mod_data_i$gam_se_non0)
  time_mod_data_i$gam_sales <- rnorm(nrow(time_mod_data_i), time_mod_data_i$gam_pred_sales, time_mod_data_i$gam_se_sales)
  
  # calculating polynomial approximations of the gams
  m_non0_time_i <- lm(update(f_time, gam_non0 ~ .), time_mod_data_i)
  m_sales_time_i <- lm(update(f_time, gam_sales ~ .), data = time_mod_data_i %>% filter(sales > 0))
  
  # applying reasonable priors
  j <- length(coef(m_non0_time_i))
  results_non0_i <- apply_rttm_mvn(m_non0_time_i, rep(0, j), diag(25, j, j))
  results_sales_i <- apply_rttm_mvn(m_sales_time_i, rep(0, j), diag(25, j, j))
  
  # combining the results into a nice dataset
  results_i <- results_non0_i %>% 
    left_join(results_sales_i, by = "term", suffix = c("_non0", "_sales")) %>% 
    mutate(item_id = train_raw$item_id[i], store_id = train_raw$store_id[i])
  
  
#   store_item_coefs <- bind_rows(store_item_coefs, results_i)
# }
  
  results_i
}
# closing the clusters
stopCluster(cl)
stopImplicitCluster()
gc()

# summary of results
store_item_coefs %>% 
  group_by(term) %>% 
  summarise(mu_non0 = weighted.mean(estimate_non0, 1 / se_non0^2, na.rm = T), 
            sd_between_non0 = sqrt(wtd.var(estimate_non0, 1 / se_non0^2)), 
            sd_within_non0 = sqrt(mean(se_non0^2, na.rm = T)), 
            mu_sales = weighted.mean(estimate_sales, 1 / se_sales^2, na.rm = T), 
            sd_between_sales = sqrt(wtd.var(estimate_sales, 1 / se_sales^2)), 
            sd_within_sales = sqrt(mean(se_sales^2, na.rm = T))) %>% 
  mutate(across(where(is.numeric), round, digits = 2))

# applying RTTM to coefficients
store_item_coefs_regr <- store_item_coefs %>% 
  group_by(term) %>% 
  mutate(mu_non0 = weighted.mean(estimate_non0, 1 / se_non0^2), 
         sd_between_non0 = sqrt(wtd.var(estimate_non0, 1 / se_non0^2)), 
         mu_sales = weighted.mean(estimate_sales, 1 / se_sales^2), 
         sd_between_sales = sqrt(wtd.var(estimate_sales, 1 / se_sales^2))) %>% 
  mutate(est_regr_non0 = apply_rttm(estimate_non0, se_non0, sd_between_non0, mu_non0), 
         est_regr_sales = apply_rttm(estimate_sales, se_sales, sd_between_sales, mu_sales)) %>% 
  ungroup() %>% 
  # organize into a nicer format
  select(store_id, item_id, term, est_regr_non0, est_regr_sales) %>% 
  pivot_wider(names_from = term, values_from = c(est_regr_non0, est_regr_sales)) %>% 
  rename_with(str_replace_all, pattern = "est_regr_", replacement = "") %>% 
  mutate(across(where(is.numeric), round, digits = 4))

# generating predictions -------------------------------------------------------


store_item_coefs_nested <- store_item_coefs_regr %>% 
  group_by(store_id, item_id) %>% 
  nest(non0 = starts_with("non0_"), sales = starts_with("sales_"))

# was running into memory issues with a tidy approach, so switching to a for loop
preds_store_item_time <- tibble(data.frame())
pb <- progress_bar$new(total = nrow(train_raw))
for(i in 1:nrow(train_raw)) {
  pb$tick()

  df_i <- expand_grid(store_id = train_raw$store_id[i],
                      item_id = train_raw$item_id[i],
                      day = pp_global$day) %>%
    mutate(day2 = pmin(day, 1941)) %>%
    left_join(pp_global, by = c("day2" = "day")) %>%
    select(-day2) %>%
    group_by(store_id, item_id) %>%
    nest(day = day, pp = starts_with("X"))

  preds_i <- df_i %>%
    left_join(store_item_coefs_nested, by = c("item_id", "store_id")) %>%
    mutate(
      lpred_non0_store_item = map2(pp, non0, ~ as.numeric(cbind(1, as.matrix(.x)) %*% as.numeric(.y))),
      lpred_sales_store_item = map2(pp, sales, ~ as.numeric(cbind(1, as.matrix(.x)) %*% as.numeric(.y)))
    ) %>%
    ungroup() %>%
    select(store_id, item_id, day, lpred_non0_store_item, lpred_sales_store_item) %>%
    unnest(c(day, lpred_non0_store_item, lpred_sales_store_item)) %>%
    pivot_wider(names_from = day, values_from = c(lpred_non0_store_item, lpred_sales_store_item))


  preds_store_item_time <- bind_rows(preds_store_item_time, preds_i)
}

# this started sucking up too much memory for some reason
# # note that I'm truncating day for this exercise
# preds_store_item_time <- expand_grid(store_id = unique(train_raw$store_id), 
#                                 item_id = unique(train_raw$item_id), 
#                                 day = pp_global$day) %>% 
#   mutate(day2 = pmin(day, 1941)) %>%
#   left_join(pp_global, by = c("day2" = "day")) %>% 
#   group_by(store_id, item_id) %>% 
#   nest(day = day, pp = starts_with("X")) %>% 
#   left_join(store_item_coefs_nested, by = c("item_id", "store_id")) %>% 
#   mutate(
#     lpred_non0_store_item = map2(pp, non0, ~ as.numeric(cbind(1, as.matrix(.x)) %*% as.numeric(.y))), 
#     lpred_sales_store_item = map2(pp, sales, ~ as.numeric(cbind(1, as.matrix(.x)) %*% as.numeric(.y)))
#   ) %>% 
#   ungroup() %>% 
#   select(store_id, item_id, day, lpred_non0_store_item, lpred_sales_store_item) %>% 
#   unnest(c(day, lpred_non0_store_item, lpred_sales_store_item)) %>% 
#   mutate(across(where(is.numeric), round, digits = 4)) %>%
#   pivot_wider(names_from = day, values_from = c(lpred_non0_store_item, lpred_sales_store_item))


# checks -----------------------------------------------------------------------

# predictions from store-item effects
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

# predictions from store + item main effects
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

# actual sales
train_sub_long <- train_raw %>% 
  filter(store_id %in% preds_new$store_id, item_id %in% preds_new$item_id) %>% 
  pivot_longer(starts_with("d_"), 
               names_to = "day", 
               names_prefix = "d_", 
               names_transform = list(day = as.integer), 
               values_to = "sales") 

preds_new %>% 
  filter(day > 500) %>% 
  sample_frac(.2)  %>% 
  left_join(preds_global, by = c("day")) %>% 
  left_join(preds_old, by = c("item_id", "store_id", "day")) %>% 
  left_join(train_sub_long, by = c("item_id", "store_id", "day")) %>% 
  mutate(pred_non0 = arm::invlogit(lpred_non0_item + lpred_non0_old + lp_non0_base), 
         pred_sales_non0 = exp(lpred_sales_item + lp_sales_base + lpred_sales_old), 
         pred_sales = pred_non0 * pred_sales_non0) %>% 
  qplot(day, pred_sales, data = ., geom = "line") + 
  geom_line(aes(y = sales), colour = "steelblue") + 
  facet_wrap(store_id~item_id, scales = "free_y")

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


save(store_item_coefs_regr, file = "fitted-models/models-time-store-item.RData")
save(preds_store_item_time, file = "predictions/preds-time-store-item.RData")
save(preds_time, file = "predictions/preds-time-all.RData")



