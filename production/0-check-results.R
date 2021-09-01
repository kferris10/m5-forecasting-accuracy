
#' notes -----------------------------------------------------------
#' 
#' this worked shockingly well
#' major issues with 0s and extreme store/item coefs.  But otherwise I'm pretty happy

# setup -----------------------------------------------------------

library(feather)
library(arm)
library(tidyverse)
library(progress)
options(stringsAsFactors = F, digits = 3, mc.cores = 3)

# loading data
load("predictions/preds-all.RData")
cal <- read_csv("data/calendar.csv") %>% 
  mutate(day = as.numeric(str_replace_all(d, "d_", ""))) %>% 
  select(date, day, year, month, weekday)
train_raw <- read_feather("data/data-train-wide.feather") %>% 
  # just using post-2012 for simplicity
  select(-(d_1:d_337))
train_raw %>% select(1:20) %>% glimpse()

# generating overal predictions
preds <- preds %>% 
  # just using post-2012 for simplicity
  select(-(lp_non0_1:lp_non0_337), -(lp_non0_1942:lp_non0_1969), 
         -c(lp_non0_331, lp_non0_697, lp_non0_1062, lp_non0_1427, lp_non0_1792), 
         -c(lp_non0_300, lp_non0_664, lp_non0_1035, lp_non0_1399, lp_non0_1763), 
         -(lp_sales_1:lp_sales_337), -(lp_sales_1942:lp_sales_1969), 
         -c(lp_sales_331, lp_sales_697, lp_sales_1062, lp_sales_1427, lp_sales_1792), 
         -c(lp_sales_300, lp_sales_664, lp_sales_1035, lp_sales_1399, lp_sales_1763))

# totals
act <- select(train_raw, starts_with("d_"))
train_raw$total_sales <- apply(act, 1, sum, na.rm = T)
train_raw$total_non0s <- apply(act, 1, function(x) sum(x != 0, na.rm = T))
train_raw$total_pred_non0s <- rep(NA, nrow(train_raw))
train_raw$total_pred_sales <- rep(NA, nrow(train_raw))

# predicted totals is a little complicated because I NA'ed stuff out
# takes about 10 minutes
pb <- progress_bar$new(total = nrow(train_raw))
for(i in 1:nrow(train_raw)) {
  pb$tick()
  act_i <- as.numeric(act[i, ])
  p <- ifelse(is.na(act_i), NA, as.numeric(preds[i, 4:1599]))
  lp_non0_i <- ifelse(is.na(act_i), NA, as.numeric(preds[i, 4:1599]))
  lp_sales_i <- ifelse(is.na(act_i), NA, as.numeric(preds[i, 1600:3195]))

  train_raw$total_pred_non0s[i] <- round(sum(invlogit(lp_non0_i), na.rm = T))
  train_raw$total_pred_sales[i] <- round(sum(invlogit(lp_non0_i) * exp(lp_sales_i), na.rm = T))
}

# good sign! - not predicting quite enough 0s, but otherwise really good
qplot(total_pred_non0s, total_non0s, data = train_raw, geom = "smooth") + geom_abline()
# predictions are slightly too high above 100k sales, but this is really good
qplot(total_pred_sales, total_sales, data = train_raw, geom = "smooth") + geom_abline()

# checks -----------------------------------------------------------

bdown <- function(store, item) {
  act <- train_raw %>% 
    filter(store_id %in% store, item_id %in% item) %>% 
    pivot_longer(cols = starts_with("d_"), 
                 names_to = "day", 
                 names_prefix = "d_", 
                 names_transform = list(day = as.integer), 
                 values_to = "sales", 
                 values_drop_na = TRUE) %>% 
    select(store_id, item_id, day, sales)
  
  pred <- preds %>% 
    filter(store_id %in% store, item_id %in% item) %>% 
    pivot_longer(cols = starts_with("lp_"), 
                 names_to = c(".value", "day"), 
                 names_prefix = "lp_", 
                 names_sep = "_", 
                 names_transform = list(day = as.integer)) %>% 
    mutate(pred_non0 = invlogit(non0), 
           pred_sales = invlogit(non0) * exp(sales)) %>% 
    select(store_id, item_id, day, pred_non0, pred_sales)
  
  pred %>% 
    left_join(act, by = c("store_id", "item_id", "day")) %>% 
    left_join(cal, by = c("day")) %>% 
    arrange(store_id, item_id, day) %>% 
    group_by(store_id, item_id) %>% 
    mutate(cum_sales = cumsum(coalesce(sales, 0)),
           cum_non0s = cumsum(coalesce(sales != 0, 0)),
           cum_pred = cumsum(ifelse(is.na(sales) & day <= 1941, 0, pred_sales)),
           cum_pred_non0s = cumsum(ifelse(is.na(sales) & day <= 1941, 0, pred_non0))) %>%
    ungroup()
}

bdown("TX_1", "FOODS_3_099") %>% 
  ggplot(aes(x = date, y = sales)) + 
  geom_line() + 
  geom_line(aes(y = pred_sales), colour = "steelblue") + 
  geom_vline(aes(xintercept = as.Date("2016-05-22")), colour = "purple4", linetype = "dashed")
bdown("TX_1", "FOODS_3_099") %>% 
  ggplot(aes(x = date, y = cum_sales)) + 
  geom_line() + 
  geom_line(aes(y = cum_pred), colour = "steelblue") + 
  geom_vline(aes(xintercept = as.Date("2016-05-22")), colour = "purple4", linetype = "dashed")

bdown(c("TX_1", "WI_1", "CA_1"), "FOODS_3_252") %>% 
  ggplot(aes(x = date, y = sales)) + 
  geom_line() + 
  geom_line(aes(y = pred_sales), colour = "steelblue") + 
  facet_wrap(~store_id)

bdown(c("CA_1"), sample(train_raw$item_id, 6)) %>% 
  ggplot(aes(x = date, y = sales)) + 
  geom_line() + 
  geom_line(aes(y = pred_sales), colour = "steelblue") + 
  facet_wrap(~item_id)

# umm... ?
bdown(c("CA_3", "CA_1", "CA_2"), "FOODS_3_514") %>% 
  ggplot(aes(x = date, y = sales)) + 
  geom_line() + 
  geom_line(aes(y = pred_sales), colour = "steelblue") +
  facet_wrap(~store_id)

# biggest residuals -------------------------------------------------------

# only HOBBIES_1_311 and HOUSEHOLD_2_162 have 0 sales across all
# ~100 other random items have 0 sales across 1 or 2 stores
# most of the other ones the model just never gets off 0 - fix
train_raw %>% 
  select(ends_with("_id"), starts_with("total")) %>% 
  mutate(log_resid = log(total_sales) - log(total_pred_sales)) %>% 
  arrange(desc(log_resid), desc(total_sales))
bdown("WI_3", "FOODS_3_282") %>%
# bdown("TX_1", "FOODS_3_135") %>%
# bdown("WI_1", "HOUSEHOLD_1_399") %>%
# bdown("WI_3", "FOODS_3_654") %>%
# bdown("CA_4", "HOBBIES_1_016") %>%
# bdown("WI_1", "FOODS_3_073") %>%
# bdown("WI_1", "HOUSEHOLD_1_066") %>%
  filter(!is.na(sales)) %>%
  ggplot(aes(x = date, y = sales)) + 
  geom_line() + 
  geom_line(aes(y = pred_sales), colour = "steelblue")

# this is actually really, really good!
train_raw %>% 
  select(ends_with("_id"), starts_with("total")) %>% 
  filter(total_sales > 0, total_pred_sales < 1e4) %>% 
  mutate(log_resid = log(total_sales) - log(total_pred_sales)) %>% 
  arrange((log_resid))
# bdown("WI_2", "FOODS_3_135") %>% 
# bdown("TX_3", "FOODS_2_070") %>%
bdown("WI_3", "FOODS_3_665") %>%
# bdown("CA_2", "HOUSEHOLD_1_518") %>% 
# bdown("CA_3", "FOODS_3_152") %>% 
  filter(!is.na(sales)) %>% 
  ggplot(aes(x = date, y = sales)) + 
  geom_line() + 
  geom_line(aes(y = pred_sales), colour = "steelblue")


