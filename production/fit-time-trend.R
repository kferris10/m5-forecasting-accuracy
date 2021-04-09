
# notes ------------------------------------------------------------------------



# setup ------------------------------------------------------------------------

library(feather)
library(lme4)
library(Hmisc)
library(tidyverse)
library(multidplyr)
library(broom)
library(progress)
source("helper-funs.R")

load("models-global.RData")
cal <- read_csv("calendar.csv") %>%
  mutate(d = as.numeric(str_replace_all(d, "d_", ""))) %>%
  rename(day = d) %>%
  select(date, day, year, month, weekday)
train_raw <- read_feather("data-train-wide.feather")
train_raw %>% select(1:20) %>% glimpse()


train_by_store <- train_raw %>% 
  mutate(store_id2 = store_id) %>% 
  group_by(store_id2) %>% 
  nest() %>% 
  rowwise()
train_by_item <- train_raw %>% 
  mutate(item_id2 = item_id) %>% 
  group_by(item_id2) %>% 
  nest() %>% 
  rowwise()

# model specification I'll be using
f_time <- formula(~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10)
f_non0_time <- update(f_time, I(sales != 0) ~ .)
f_sales_time <- update(f_time, sales ~ . + offset(non0))

mod_fun <- function(data, pb = NULL) {
  # need to manually fit the models and a ton of gc()s
  # because my laptop doesn't have much memory
  
  gc()
  if(!is.null(pb)) pb$tick()
  
  df <- data  %>% 
    pivot_longer(starts_with("d_"), 
                 names_to = "day", 
                 names_prefix = "d_", 
                 names_transform = list(day = as.numeric), 
                 values_to = "sales", 
                 values_drop_na = TRUE) %>% 
    left_join(cal, by = "day") %>%
    bind_cols(data.frame(predict(pp, .$day))) %>% 
    mutate(lpred_non0_base = predict(m_non0_base, newdata = .))
  
  gc()
  X <- model.matrix(delete.response(terms(f_non0_time)), df)
  y <- model.response(model.frame(f_non0_time, df))
  m <- glm.fit(X, y, family = binomial(), offset = df$lpred_non0_base)
  coefs <- tibble(est_raw = coef(m), se = extract_se(m))
  
  gc()
  coefs
}

# fitting by store -------------------------------------------------------------

# store coefficints
pb <- progress_bar$new(total = nrow(train_by_store))
store_coefs <- train_by_store %>% 
  mutate(m_non0_time = list(mod_fun(data, pb = pb)))
gc()

pb <- progress_bar$new(total = nrow(train_by_item))
item_coefs <- train_by_item %>% 
  mutate(m_non0_time = list(mod_fun(data, pb = pb))) %>% 
  ungroup() %>% 
  select(-data) 
gc()



