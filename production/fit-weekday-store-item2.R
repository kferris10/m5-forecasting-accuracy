
# notes ------------------------------------------------------------------------

# calculating weekday coefficients for each store/item
# takes ~ 1 hour

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
options(stringsAsFactors = F, digits = 3, mc.cores = 3)

# loading data
cal <- read_feather("data/data-calendar-clean.feather")
load("predictions/preds-global.RData")
load("predictions/preds-time-all.RData")
load("predictions/preds-month-all.RData")
load("predictions/preds-weekday-store.RData")
load("predictions/preds-weekday-item.RData")
train_raw <- read_feather("data/data-train-wide.feather")
train_raw %>% select(1:20) %>% glimpse()

# setting up the baseline time predictions
preds_base <- bind_cols(
  select(preds_time, ends_with("id")), 
  select(preds_time, -ends_with("id")) + 
    select(preds_month, -ends_with("id")) + 
    select(preds_store_weekday, -ends_with("id")) + 
    select(preds_item_weekday, -ends_with("id"))
)
rm(preds_time, preds_month, preds_store_weekday, preds_item_weekday)
gc()

# fitting by item -------------------------------------------------------------

# setup parallel backend to use many processors
cl <- makeCluster(getOption("mc.cores"))
registerDoParallel(cl)
n <- nrow(train_raw)
clusterExport(cl, c("n"))
store_item_weekday_coefs <- foreach(i=icount(n), .packages = c("tidyverse", "broom", "tcltk"), .combine=rbind) %dopar% {
  if(!exists("pb")) pb <- tkProgressBar("Parallel task", min=1, max=n)
  setTkProgressBar(pb, i)  
  
# if I need to test one at a time
# store_item_weekday_coefs <- tibble(data.frame())
# pb <- progress_bar$new(total = nrow(train_raw))
# for(i in 1:nrow(train_raw)) {
#   pb$tick()
  
  dat_i <- train_raw %>% 
    filter(item_id == train_raw$item_id[i], store_id == train_raw$store_id[i])
  off_dat_i <- preds_base %>% 
    filter(item_id == train_raw$item_id[i], store_id == train_raw$store_id[i])
  
  # fitting the models
  mod_data_i <- prep_time_data(dat_i, 
                               off_data = off_dat_i, 
                               off_data_non0_prefix = "lpred_non0_time_", 
                               off_data_sales_prefix = "lpred_sales_time_")
  # non0 model
  non0_mod_data <- mod_data_i %>% filter(between(lp_non0_base, -7, 7))
  cf_non0 <- if(length(unique(non0_mod_data$weekday)) > 1) {
    m_non0_time_i <- glm(update(f_weekday, I(sales != 0) ~ .), 
                         non0_mod_data, 
                         family = binomial(), 
                         offset = lp_non0_base)
    tidy(m_non0_time_i)
  } else {
    data.frame(term = character(), 
               estimate = numeric(), 
               std.error = numeric(), 
               statistic = numeric(), 
               p.value = numeric())
  }
  # sales modes
  sales_mod_data <- mod_data_i %>% filter(sales > 0, between(lp_sales_base, -5, 5))
  cf_sales <- if(length(unique(sales_mod_data$weekday)) > 1) {
    m_sales_time_i <- glm(update(f_weekday, sales ~ .), 
                          sales_mod_data, 
                          family = poisson(), 
                          offset = lp_sales_base)
    tidy(m_sales_time_i)
  } else {
    data.frame(term = character(), 
               estimate = numeric(), 
               std.error = numeric(), 
               statistic = numeric(), 
               p.value = numeric())
  }
  
  # combining the results into a nice dataset
  results_i <- cf_non0 %>% 
    left_join(cf_sales, by = "term", suffix = c("_non0", "_sales")) %>% 
    mutate(item_id = train_raw$item_id[i], store_id = train_raw$store_id[i])
  
  
# store_item_weekday_coefs <- bind_rows(store_item_weekday_coefs, results_i)
# }

  results_i
}

# closing the clusters
stopCluster(cl)
stopImplicitCluster()
gc()

# summary of results
store_item_weekday_coefs %>% 
  group_by(term) %>% 
  summarise(mu_non0 = weighted.mean(estimate_non0, 1 / std.error_non0^2, na.rm = T), 
            sd_between_non0 = sqrt(wtd.var(estimate_non0, 1 / std.error_non0^2)), 
            sd_within_non0 = sqrt(mean(std.error_non0^2, na.rm = T)), 
            mu_sales = weighted.mean(estimate_sales, 1 / std.error_sales^2, na.rm = T), 
            sd_between_sales = sqrt(wtd.var(estimate_sales, 1 / std.error_sales^2)), 
            sd_within_sales = sqrt(mean(std.error_sales^2, na.rm = T))) %>% 
  mutate(across(where(is.numeric), round, digits = 2))

# applying RTTM to coefficients
store_item_weekday_coefs_regr <- expand_grid(store_id = unique(train_raw$store_id), 
                                             item_id = unique(train_raw$item_id), 
                                             term = unique(store_item_weekday_coefs$term)) %>%
  left_join(store_item_weekday_coefs, by = c("store_id", "item_id", "term")) %>% 
  # applying RTTM
  group_by(term) %>% 
  mutate(mu_non0 = weighted.mean(estimate_non0, 1 / std.error_non0^2, na.rm = T), 
         sd_between_non0 = sqrt(wtd.var(estimate_non0, 1 / std.error_non0^2)), 
         mu_sales = weighted.mean(estimate_sales, 1 / std.error_sales^2, na.rm = T), 
         sd_between_sales = sqrt(wtd.var(estimate_sales, 1 / std.error_sales^2))) %>% 
  mutate(est_regr_non0 = coalesce(apply_rttm(estimate_non0, std.error_non0, sd_between_non0, mu_non0), 0), 
         est_regr_sales = coalesce(apply_rttm(estimate_sales, std.error_sales, sd_between_sales, mu_sales), 0)) %>% 
  ungroup() %>% 
  # organize into a nicer format
  select(store_id, item_id, term, est_regr_non0, est_regr_sales) %>% 
  pivot_wider(names_from = term, values_from = c(est_regr_non0, est_regr_sales)) %>% 
  rename_with(str_replace_all, pattern = "est_regr_", replacement = "") %>% 
  mutate(across(where(is.numeric), round, digits = 4))

# generating predictions -------------------------------------------------------


store_item_weekday_coefs_nested <- store_item_weekday_coefs_regr %>% 
  group_by(store_id, item_id) %>% 
  nest(non0 = starts_with("non0_"), sales = starts_with("sales_"))

# not pretty code, but it works
# Hadley's nested data approach seems to have a ways to go
preds_store_item_weekday <- expand_grid(store_id = unique(train_raw$store_id), 
                                item_id = unique(train_raw$item_id), 
                                day = pp_global$day) %>% 
  left_join(cal %>% select(day, weekday), by = "day") %>% 
  group_by(store_id, item_id) %>% 
  nest(day = day, weekday = weekday) %>% 
  mutate(weekday_matrix = map(weekday, ~ model.matrix(f_weekday, data = .x))) %>% 
  left_join(store_item_weekday_coefs_nested, by = c("item_id", "store_id")) %>% 
  mutate(
    lpred_non0_store_item = map2(weekday_matrix, non0, ~ as.numeric(.x %*% as.numeric(.y))), 
    lpred_sales_store_item = map2(weekday_matrix, sales, ~ as.numeric(.x %*% as.numeric(.y)))
  ) %>% 
  ungroup() %>% 
  select(store_id, item_id, day, lpred_non0_store_item, lpred_sales_store_item) %>% 
  unnest(c(day, lpred_non0_store_item, lpred_sales_store_item)) %>% 
  mutate(across(where(is.numeric), round, digits = 4)) %>%
  pivot_wider(names_from = day, values_from = c(lpred_non0_store_item, lpred_sales_store_item))


# checks -----------------------------------------------------------------------

# predictions from store-item effects
preds_new <- preds_store_item_weekday %>% 
  filter(store_id %in% c("TX_1", "CA_1"), 
         item_id %in% c(sample(train_raw$item_id, 3), "FOODS_3_555", "HOUSEHOLD_1_441")) %>% 
  pivot_longer(cols = starts_with("lpred"), 
               names_prefix = "lpred_", 
               names_to = c("type", "day"), 
               names_sep = "_store_item_", 
               names_transform = list(day = as.integer), 
               values_to = "effect") %>% 
  left_join(cal, by = "day") %>% 
  select(store_id, item_id, weekday, type, effect) %>% 
  distinct() %>% 
  mutate(weekday = factor(weekday, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) 

# seems reasonable
preds_new %>% 
  qplot(as.numeric(weekday), effect, data = ., geom = "line", colour = type) + 
  facet_wrap(store_id~item_id, scales = "free_y")

# saving -----------------------------------------------------------------------

# saving the full weekday effect
load("predictions/preds-weekday-store.RData")
load("predictions/preds-weekday-item.RData")

preds_weekday <- bind_cols(
  select(train_raw, 1:3), 
  select(preds_store_weekday, starts_with("lpred")) + 
    select(preds_item_weekday, starts_with("lpred")) + 
    select(preds_store_item_weekday, starts_with("lpred"))
) %>% 
  rename_with(str_replace_all, pattern = "_store_item_", replacement = "_") %>% 
  rename_with(str_replace_all, pattern = "_store_", replacement = "_") %>% 
  rename_with(str_replace_all, pattern = "_item_", replacement = "_")


save(store_item_weekday_coefs_regr, file = "fitted-models/models-weekday-store-item.RData")
save(preds_store_item_weekday, file = "predictions/preds-weekday-store-item.RData")
save(preds_weekday, file = "predictions/preds-weekday-all.RData")



