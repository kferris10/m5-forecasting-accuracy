
# notes -----------------------------------------------------------------------

# I should probably just remove christmas and thanksgiving

# setup ------------------------------------------------------------

library(tidyverse)
library(feather)
library(pbapply)

cal <- read_csv("data/calendar.csv")
train_eval <- read_csv("data/sales_train_evaluation.csv")
train_eval %>% select(1:20) %>% glimpse()

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


# save ----------------------------------

# data in wide format
save_dat <- train_eval %>% 
  select(id, item_id, store_id, d_1:d_1941) %>% 
  # removing thanksgiving and christmas
  select(-c(d_331, d_697, d_1062, d_1427, d_1792), 
         -c(d_300, d_664, d_1035, d_1399, d_1763))
  

# saving as feather files
# hadley says that they are fast and he's THE MAN so...
write_feather(save_dat, "data/data-train-wide.feather")

