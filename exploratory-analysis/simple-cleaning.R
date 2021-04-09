
library(tidyverse)
library(RcppRoll)

cal <- read_csv("calendar.csv")
train_eval <- read_csv("sales_train_evaluation.csv")

train_sample <- train_eval %>% 
  filter(dept_id == "FOODS_3", 
         item_id %in% c("FOODS_3_586", "FOODS_3_120", "FOODS_3_090", "FOODS_3_681", 
                        "FOODS_3_089", "FOODS_3_088", "FOODS_3_681", "FOODS_3_811", 
                        "FOODS_3_252", "FOODS_3_251")) %>% 
  select(id, item_id, store_id, d_1:d_1941) %>% 
  pivot_longer(cols = starts_with("d_"), names_to = "day", values_to = "sales") %>% 
  # adding in some simple calendar info
  left_join(cal, by = c("day" = "d")) %>% 
  mutate(day = as.numeric(str_replace_all(day, "d_", "")), 
         day_z = (day - mean(day)) / sd(day)) %>% 
  select(id, item_id, store_id, date, weekday, year, month, day, day_z, sales)

train_sample %>% 
  filter(store_id == "CA_1") %>% 
  qplot(day, sales, data = ., geom = "line") + 
  facet_wrap(~item_id, scales = "free_y")
train_sample  %>% 
  group_by(id) %>% 
  mutate(cum_sales = cumsum(sales)) %>% 
  filter(store_id == "CA_1") %>% 
  qplot(day, cum_sales, data = ., geom = "line") + 
  facet_wrap(~item_id, scales = "free_y")

# removing leading 0s
train_df <- train_sample %>% 
  group_by(id) %>% 
  mutate(roll_0 = roll_sum(sales == 0, 100, align = "right", fill = NA), 
         cum_sales = cumsum(sales)) %>% 
  ungroup() %>% 
  filter(roll_0 < 90) %>% 
  select(-roll_0)

train_df  %>% 
  filter(store_id == "CA_1") %>% 
  qplot(day, sales, data = ., geom = "line") + 
  facet_wrap(~item_id, scales = "free_y")
train_df  %>% 
  filter(store_id == "CA_1") %>% 
  qplot(day, cum_sales, data = ., geom = "line") + 
  facet_wrap(~item_id, scales = "free_y")

train_0 <- train_df
train_non0 <- train_df %>% filter(sales != 0)

save(train_0, train_non0, file = "data-train-simple.RData")
