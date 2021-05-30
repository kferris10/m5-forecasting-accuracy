
# notes ------------------------------------------------------------------------

# calculating time trend coefficients for each item
# this takes ~30 minutes

# setup ------------------------------------------------------------------------

library(feather)
library(Hmisc)
library(tidyverse)
library(broom)
library(progress)
library(mgcv)
source("production/helper-funs.R")
options(stringsAsFactors = F, digits = 3, mc.cores = 2)

# model specifications I'll be using
f_time <- formula(~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10)

# loading data
load("predictions/preds-global.RData")
load("predictions/preds-time-store.RData")
cal <- read_csv("data/calendar.csv") %>% 
  mutate(day = as.numeric(str_replace_all(d, "d_", ""))) %>% 
  select(day, date, weekday, month)
train_raw <- read_feather("data/data-train-wide.feather")
train_raw %>% select(1:20) %>% glimpse()

# fitting by item -------------------------------------------------------------


item_coefs <- tibble(data.frame())

pb <- progress_bar$new(total = length(unique(train_raw$item_id)))
for(i in unique(train_raw$item_id)) {
  pb$tick()
  dat_i <- train_raw %>% filter(item_id == i)
  off_dat_i <- preds_store_time %>% filter(item_id == i)
  
  # fitting gams initially
  time_mod_data_i <- prep_time_data(dat_i, 
                                    off_data = off_dat_i, 
                                    off_data_non0_prefix = "lpred_non0_store_", 
                                    off_data_sales_prefix = "lpred_sales_store_")
  gam_non0_i <- gam(I(sales != 0) ~ s(day, bs = "ts", k = 10), 
                    data = time_mod_data_i, 
                    family = binomial(), 
                    offset = lp_non0_base)
  gam_sales_i <- gam(sales ~ s(day, bs = "ts", k = 10), 
                     data = time_mod_data_i %>% filter(sales > 0), 
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
    mutate(item_id = i)
  
  item_coefs <- bind_rows(item_coefs, results_i)
}


# applying RTTM to coefficients
item_coefs_regr <- item_coefs %>% 
  group_by(term) %>% 
  mutate(mu_non0 = weighted.mean(estimate_non0, 1 / se_non0^2), 
         sd_between_non0 = sqrt(wtd.var(estimate_non0, 1 / se_non0^2)), 
         mu_sales = weighted.mean(estimate_sales, 1 / se_sales^2), 
         sd_between_sales = sqrt(wtd.var(estimate_sales, 1 / se_sales^2))) %>% 
  mutate(est_regr_non0 = apply_rttm(estimate_non0, se_non0, sd_between_non0, mu_non0), 
         est_regr_sales = apply_rttm(estimate_sales, se_sales, sd_between_sales, mu_sales)) %>% 
  ungroup() %>% 
  # organize into a nicer format
  select(item_id, term, est_regr_non0, est_regr_sales) %>% 
  pivot_wider(names_from = term, values_from = c(est_regr_non0, est_regr_sales)) %>% 
  rename_with(str_replace_all, pattern = "est_regr_", replacement = "") %>% 
  mutate(across(where(is.numeric), round, digits = 4))
# mean(item_coefs_regr$non0_X5 > 20)
# [1] .0236

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
expand_grid(item_id = c(sample(train_raw$item_id, 3), "FOODS_3_252", "FOODS_2_222", "HOUSEHOLD_1_459"), 
            day = pmin(2000, pp_global$day)) %>% 
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
  left_join(preds_global, by = c("day")) %>% 
  filter(day > 500) %>%
  mutate(pred_sales = arm::invlogit(lpred_non0_item + lp_non0_base) * exp(lpred_sales_item + lp_sales_base)) %>% 
  qplot(day, pred_sales, data = ., geom = "line", colour = item_id)

expand_grid(item_id = c("FOODS_3_282", "FOODS_3_370", "HOUSEHOLD_1_459"), day = pp_global$day) %>% 
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
  filter(day > 500) %>% 
  mutate(pred_sales = arm::invlogit(lpred_non0_item + lp_non0_base) * exp(lpred_sales_item + lp_sales_base)) %>% 
  qplot(day, pred_sales, data = ., geom = "line", colour = item_id)

# this looks good
item_coefs_regr %>% 
  pivot_longer(cols = -item_id, names_to = "variable", values_to = "value") %>% 
  ggplot(aes(x = log(abs(value)))) + 
  geom_histogram(binwidth = 1, alpha = 0.5, fill = "grey", colour = "darkgreen") + 
  facet_wrap(~variable, scales = "free") + 
  theme_bw()


# saving -----------------------------------------------------------------------

save(item_coefs_regr, file = "fitted-models/models-time-item.RData")
save(preds_item_time, file = "predictions/preds-time-item.RData")



