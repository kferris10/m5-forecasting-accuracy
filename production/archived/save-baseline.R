
# setup -----------------------------------------------------------

library(feather)
library(tidyverse)
options(stringsAsFactors = F, digits = 3, mc.cores = 3)

# loading data
cal <- read_feather("data/data-calendar-clean.feather")
train_raw <- read_feather("data/data-train-wide.feather")

baseline_validation <- train_raw %>% 
  select(id, d_1548:d_1575) %>% 
  mutate(id = str_replace_all(id, "evaluation", "validation")) %>% 
  mutate(across(where(is.numeric), ~ coalesce(., 0)))
names(baseline_validation) <- c("id", paste0("F", 1:28))

baseline_evaluation <- train_raw %>% 
  select(id, d_1576:d_1603) %>% 
  mutate(across(where(is.numeric), ~ coalesce(., 0)))
names(baseline_evaluation) <- c("id", paste0("F", 1:28))

baseline_submission <- bind_rows(baseline_validation, baseline_evaluation)

write.csv(baseline_submission, file = "predictions/preds-baseline.csv", row.names = F, quote = F)
