
#' notes -----------------------------------------------------------
#' 
#' this worked shockingly well
#' major issues with 0s and extreme store/item coefs.  But otherwise I'm pretty happy

# setup -----------------------------------------------------------

library(feather)
library(arm)
library(tidyverse)
options(stringsAsFactors = F, digits = 3, mc.cores = 3)

# loading data
cal <- read_csv("data/calendar.csv")
load("predictions/preds-global.RData")
load("predictions/preds-time-all.RData")
train_raw <- read_feather("data/data-train-wide.feather") %>% 
  # just using post-2012 for simplicity
  select(-(d_1:d_337))
train_raw %>% select(1:20) %>% glimpse()

act <- select(train_raw, starts_with("d_"))
train_raw$total_sales <- apply(act, 1, sum, na.rm = T)
train_raw$total_non0s <- apply(act, 1, function(x) sum(x != 0, na.rm = T))

preds_non0_clean <- select(preds_time, starts_with("lpred_non0_time_")) %>%
  rename_with(str_replace_all, pattern = "lpred_non0_time_", replacement = "d_") %>% 
  # also removing thanksgiving and christmas
  select(-(d_1:d_337), 
         -c(d_331, d_697, d_1062, d_1427, d_1792), 
         -c(d_300, d_664, d_1035, d_1399, d_1763), 
         -c(d_1942:d_1969))
preds_non0_sales_clean <- select(preds_time, starts_with("lpred_sales_time_")) %>%
  rename_with(str_replace_all, pattern = "lpred_sales_time_", replacement = "d_") %>% 
  # also removing thanksgiving and christmas
  select(-(d_1:d_337), 
         -c(d_331, d_697, d_1062, d_1427, d_1792), 
         -c(d_300, d_664, d_1035, d_1399, d_1763), 
         -c(d_1942:d_1969))
preds_global_clean <- preds_global %>% 
  filter(day %in% as.numeric(str_replace_all(colnames(preds_non0_clean), "d_", "")))
preds_non0 <- act %>% 
  as.matrix() %>% 
  is.na() %>% 
  ifelse(NA, as.matrix(preds_non0_clean)) %>% 
  apply(1, function(x) invlogit(x + preds_global_clean$lp_non0_base)) %>% 
  t()
preds_sales2 <- preds_non0 * t(apply(
  as.matrix(preds_non0_sales_clean), 1, function(x) exp(x + preds_global_clean$lp_sales_base)
))

train_raw$total_pred_sales <- round(apply(preds_sales2, 1, sum, na.rm = T), 1)
train_raw$total_pred_non0s <- round(apply(preds_non0, 1, sum, na.rm = T), 1)


# good sign! - not predicting quite enough 0s, but otherwise really good
qplot(total_pred_non0s, total_non0s, data = train_raw, geom = "smooth") + geom_abline()

# very very good
train_raw %>% 
  # filter(total_pred_sales < 1e4) %>% 
  qplot(total_pred_sales, total_sales, data = ., geom = "smooth") + 
  geom_abline()

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
    left_join(cal %>% mutate(day = as.numeric(str_replace_all(d, "d_", ""))), by = "day") %>% 
    select(store_id, item_id, day, date, sales)
  
  preds <- preds_time %>% 
    filter(store_id %in% store, item_id %in% item) %>% 
    pivot_longer(cols = starts_with("lpred_"), 
                 names_to = c(".value", "temp", "day"), 
                 names_prefix = "lpred_", 
                 names_sep = "_", 
                 names_transform = list(day = as.integer)) %>% 
    left_join(preds_global, by = "day") %>% 
    mutate(lpred_non0 = lp_non0_base + non0, 
           lpred_sales = lp_sales_base + sales, 
           pred_non0 = invlogit(lpred_non0), 
           pred_sales = invlogit(lpred_non0) * exp(lpred_sales)) %>% 
    select(store_id, item_id, day, pred_non0, pred_sales)
  
  left_join(act, preds, by = c("store_id", "item_id", "day")) %>% 
    arrange(store_id, item_id, day) %>% 
    group_by(store_id, item_id) %>% 
    mutate(cum_sales = cumsum(sales), 
           cum_0s = cumsum(sales == 0), 
           cum_pred = cumsum(pred_sales), 
           cum_pred_0s = cumsum(1 - pred_non0)) %>% 
    ungroup()
}

bdown("TX_1", "FOODS_3_099") %>% 
  ggplot(aes(x = date, y = sales)) + 
  geom_line() + 
  geom_line(aes(y = pred_sales), colour = "steelblue")

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

# looks like we're just having trouble with the 0s...
# a lot of this is my strip_left code note working...
train_raw %>% 
  select(ends_with("_id"), starts_with("total")) %>% 
  mutate(log_resid = log(total_sales) - log(total_pred_sales)) %>% 
  arrange(desc(log_resid), desc(total_sales))
# bdown("WI_3", "FOODS_3_282") %>% 
# bdown("TX_1", "FOODS_3_135") %>% 
bdown("WI_1", "HOUSEHOLD_1_399") %>%
# bdown("WI_3", "FOODS_3_654") %>%
  filter(!is.na(sales)) %>%
  ggplot(aes(x = date, y = sales)) + 
  geom_line() + 
  geom_line(aes(y = pred_sales), colour = "steelblue")

# think the big thing is that the non0 predictions get way too low sometimes
# that's really annoying, going to mess with the months code
train_raw %>% 
  select(ends_with("_id"), starts_with("total")) %>% 
  filter(total_sales > 0, total_pred_sales < 1e4) %>% 
  mutate(log_resid = log(total_sales) - log(total_pred_sales)) %>% 
  arrange((log_resid))
bdown("CA_3", "FOODS_3_152") %>% 
  filter(!is.na(sales)) %>% 
  ggplot(aes(x = date, y = sales)) + 
  geom_line() + 
  geom_line(aes(y = pred_sales), colour = "steelblue")


