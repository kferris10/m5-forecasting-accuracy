
# notes -----------------------------------------------------------------------

# I should probably just remove christmas and thanksgiving

# setup ------------------------------------------------------------

library(tidyverse)
library(feather)
library(pbapply)
library(mgcv)

cal <- read_csv("data/calendar.csv")
prices <- read_csv("data/sell_prices.csv")
train_eval <- read_csv("data/sales_train_evaluation.csv")
train_eval %>% select(1:20) %>% glimpse()

# cleaning up the calendar ----------------------------------------------


# cleaner calendar
cal_df <- cal %>% 
  # manually creating indicators for a bunch of days
  mutate(
    day = as.numeric(str_replace_all(d, "d_", "")), 
    event1 = coalesce(event_name_1, ""), 
    event2 = coalesce(event_name_2, ""), 
    is_easter = ifelse(event1 == "Easter" | event2 == "Easter", 1, 0), 
    is_cinco = ifelse(event1 == "Cinco De Mayo" | event2 == "Cinco De Mayo", 1, 0), 
    is_fday = ifelse(event1 == "Father's day" | event2 == "Father's day", 1, 0), 
    # skipping a bunch of holiday???
    mem = ifelse(event1 == "MemorialDay", 1, 0), 
    mem_p1 = ifelse(lead(event1, 1, default = "") == "MemorialDay", 1, 0), 
    mem_p2 = ifelse(lead(event1, 2, default = "") == "MemorialDay", 1, 0), 
    mem_m1 = ifelse(lag(event1, 1, default = "") == "MemorialDay", 1, 0),  
    mem_m2 = ifelse(lag(event1, 2, default = "") == "MemorialDay", 1, 0),  
    mem_m3 = ifelse(lag(event1, 3, default = "") == "MemorialDay", 1, 0),  
    mem_m8 = ifelse(lag(event1, 8, default = "") == "MemorialDay", 1, 0),  
    mem_m9 = ifelse(lag(event1, 9, default = "") == "MemorialDay", 1, 0), 
    mday = ifelse(event1 == "Mother's day", 1, 0), 
    mday_p1 = ifelse(lead(event1, 1, default = "") == "Mother's day", 1, 0), 
    mday_p2 = ifelse(lead(event1, 2, default = "") == "Mother's day", 1, 0), 
    mday_m1 = ifelse(lag(event1, 1, default = "") == "Mother's day", 1, 0),  
    mday_m2 = ifelse(lag(event1, 2, default = "") == "Mother's day", 1, 0), 
    sb = ifelse(event1 == "SuperBowl", 1, 0), 
    sb_p1 = ifelse(lead(event1, 1, default = "") == "SuperBowl", 1, 0), 
    sb_p2 = ifelse(lead(event1, 2, default = "") == "SuperBowl", 1, 0), 
    sb_m1 = ifelse(lag(event1, 1, default = "") == "SuperBowl", 1, 0), 
    sb_m2 = ifelse(lag(event1, 2, default = "") == "SuperBowl", 1, 0), 
    sb_m3 = ifelse(lag(event1, 3, default = "") == "SuperBowl", 1, 0), 
    stpats = ifelse(event1 == "StPatricksDay", 1, 0), 
    vday = ifelse(event1 == "ValentinesDay", 1, 0), 
    cinco = ifelse(is_cinco, 1, 0), 
    cinco_p1 = ifelse(lead(is_cinco, 1, default = T), 1, 0), 
    cinco_p2 = ifelse(lead(is_cinco, 2, default = T), 1, 0), 
    cinco_m1 = ifelse(lag(is_cinco, 1, default = T), 1, 0), 
    cinco_m2 = ifelse(lag(is_cinco, 2, default = T), 1, 0), 
    columbus = ifelse(event1 == "ColumbusDay", 1, 0), 
    easter = ifelse(is_easter, 1, 0), 
    easter_p1 = ifelse(lead(is_easter, 1, default = T), 1, 0), 
    easter_p2 = ifelse(lead(is_easter, 2, default = T), 1, 0), 
    easter_m1 = ifelse(lag(is_easter, 1, default = T), 1, 0), 
    easter_m2 = ifelse(lag(is_easter, 2, default = T), 1, 0), 
    hallo = ifelse(event1 == "Halloween", 1, 0), 
    ind = ifelse(event1 == "IndependenceDay", 1, 0), 
    labor = ifelse(event1 == "LaborDay", 1, 0), 
    mlk = ifelse(event1 == "MartinLutherKingDay", 1, 0), 
    mlk_p1 = ifelse(lead(event1, 1, default = "") == "MartinLutherKingDay", 1, 0), 
    mlk_m1 = ifelse(lag(event1, 1, default = "") == "MartinLutherKingDay", 1, 0), 
    mlk_m2 = ifelse(lag(event1, 2, default = "") == "MartinLutherKingDay", 1, 0), 
    vet = ifelse(event1 == "VeteransDay", 1, 0), 
    fday = ifelse(is_fday, 1, 0), 
    fday_p1 = ifelse(lead(is_fday, 1, default = T), 1, 0), 
    fday_p2 = ifelse(lead(is_fday, 2, default = T), 1, 0), 
    fday_m1 = ifelse(lag(is_fday, 1, default = T), 1, 0), 
    fday_m2 = ifelse(lag(is_fday, 2, default = T), 1, 0)
  ) %>% 
  select(year, month, date, day, wm_yr_wk, weekday, event1, snap_CA, snap_TX, snap_WI, mem:fday_m2)


# remove all elements until we get to a 0 --------------------------

### identifying what day the first sale occurred for each row
train_eval$first_non0 <- train_eval %>%
  select(starts_with("d_")) %>%
  pbapply(1, function(x) cumsum(x == 0)) %>%
  pbapply(2, function(x) min(x[x != 1:1941]))
# checking to make sure that worked
train_eval %>%
  slice(2) %>%
  pivot_longer(starts_with("d_"),
               names_to = "day",
               names_prefix = "d_",
               names_transform = as.numeric,
               values_to = "sales") %>%
  mutate(day = as.numeric(day)) %>%
  qplot(day, sales, data = ., geom = "line") +
  geom_vline(aes(xintercept = unique(first_non0)), linetype = "dashed")

### NA'ing out anything before that date

# there's probably a tidy way to do this, but a for loop works pretty well too
for(j in 1:1941) {
  name_j <- paste0("d_", j)
  train_eval[[name_j]] <- ifelse(j < train_eval$first_non0,
                                NA,
                                train_eval[[name_j]])
}

# checking to make sure that worked
train_eval %>%
  filter(store_id == "CA_1") %>%
  # sample_n(6) %>%
  filter(item_id %in% c(
    "FOODS_2_197", "FOODS_2_268", "FOODS_3_409",
    "FOODS_3_768", "FOODS_3_822", "HOUSEHOLD_1_507"
  )) %>%
  pivot_longer(starts_with("d_"),
               names_to = "day",
               names_prefix = "d_",
               names_transform = as.numeric,
               values_to = "sales") %>%
  mutate(day = as.numeric(day))  %>%
  qplot(day, sales, data = ., geom = "line") +
  facet_wrap(~item_id, scales = "free_y")

# data in wide format
save_dat <- train_eval %>% 
  select(id, item_id, store_id, d_1:d_1941) %>% 
  # removing thanksgiving and christmas
  select(-c(d_331, d_697, d_1062, d_1427, d_1792), 
         -c(d_300, d_664, d_1035, d_1399, d_1763))

# create an analogous dataset for prices --------------------------------------

zscore_prices <- function(prices) {
  df <- data.frame(price = prices) %>% 
    mutate(day = 1:n()) 
  
  result <- if(nrow(df) > 1 & n_distinct(prices) > 1) {
    m2 <- gam(price ~ s(day, k = 5, bs = "ts"), data = df)
    adjusted <- df %>% 
      mutate(r = price - as.numeric(predict(m2, newdata = .)), 
             z = coalesce((r - mean(r, na.rm = T)) / sd(r, na.rm = T)))
    round(adjusted$z, 2)
  } else {
    rep(0, length(prices))
  }
  
  return(result)
}

# transform into a dataset with columns for price on each day
price_wide <- cal_df %>% 
  select(wm_yr_wk, day) %>% 
  full_join(prices, by = c("wm_yr_wk")) %>% 
  select(store_id, item_id, day, sell_price) %>% 
  pivot_wider(names_from = day, names_prefix = "d_", values_from = sell_price) %>% 
  arrange(store_id, item_id)

# turn prices into z-scores
price_z <- tibble(data.frame(t(
  pbapply(price_wide[, -c(1, 2)], 1, zscore_prices)
)))

save_price <- save_dat %>% 
  select(id, item_id, store_id) %>% 
  arrange(store_id, item_id) %>% 
  bind_cols(price_z)
  

# save ----------------------------------

# saving as feather files
# hadley says that they are fast and he's THE MAN so...
write_feather(save_dat, "data/data-train-wide.feather")
write_feather(cal_df, "data/data-calendar-clean.feather")
write_feather(save_price, "data/data-price-clean.feather")

