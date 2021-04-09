
# this'll work
# ugly, but it'll work...

library(lme4)
library(arm)
library(tidyverse)
library(RcppRoll)

cal <- read_csv("calendar.csv")
train_eval <- read_csv("sales_train_evaluation.csv")

train_sample <- train_eval %>% 
  filter(dept_id == "FOODS_1") %>%
  select(id, item_id, store_id, d_1:d_1941) %>%
  pivot_longer(cols = starts_with("d_"), names_to = "day", values_to = "sales") %>% 
  # adding in some simple calendar info
  left_join(cal, by = c("day" = "d")) %>% 
  mutate(day = as.numeric(str_replace_all(day, "d_", "")), 
         mu = mean(day), 
         sigma = sd(day), 
         day_z = (day - mu) / sigma) %>% 
  select(id, item_id, store_id, date, weekday, year, month, day, day_z, mu, sigma, sales)

m <- glm(I(sales == 0) ~ day_z + I(day_z^2) + I(day_z^3) + I(day_z^4) + 
           factor(month) + weekday, 
         data = train_sample, family = binomial())


df <- train_eval %>% 
  filter(dept_id == "FOODS_1") %>% 
  # select(1:20, d_1941) %>% 
  summarise(across(d_1:d_1941, list(n = length, n0 = ~ sum(.x == 0)))) %>% 
  pivot_longer(cols = everything(), 
               names_to = c("day", ".value"), 
               names_prefix = "d_", 
               names_sep = "_", 
               names_transform = list(day = as.numeric), 
               values_drop_na = TRUE) %>% 
  left_join(cal %>% mutate(d = as.numeric(str_replace_all(d, "d_", ""))), by = c("day" = "d")) %>% 
  mutate(day_z = (day - 971) / 560.32)

m2 <- glm(cbind(n0, n - n0) ~ day_z + I(day_z^2) + I(day_z^3) + I(day_z^4) + 
            factor(month) + weekday, 
          data = df, family = binomial())


