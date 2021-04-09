
# notes ------------------------------------------------------------------------

# interesting note from month*weekday interaction
# weekends in the summer might have slightly more 0s
# couple days before thanksgiving have slightly fewer

# in final date vs sales plot - is actual always more extreme?

# setup ------------------------------------------------------------------------

library(feather)
library(lme4)
library(Hmisc)
library(tidyverse)
library(broom)
source("production/helper-funs.R")
set.seed(314)

cal <- read_csv("data/calendar.csv")
train_raw <- read_feather("data/data-train-wide.feather")
train_raw %>% select(1:20) %>% glimpse()

# 10th order polynomial decomposition for indendence!
pp <- poly(1:1941, degrees = 10)


# large enough sample I can just throw everything in here
f_base <- formula(~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + 
                    factor(month) + weekday)

# aggregate by day and convert to long format for modeling
train_dat <- train_raw %>% 
  summarise(across(
    d_1:d_1941, 
    list(n = ~ sum(!is.na(.x)), 
         non0 = ~ sum(.x != 0, na.rm = T), 
         sales = ~ sum(.x, na.rm = T))
  )) %>% 
  pivot_longer(cols = everything(), 
               names_to = c("day", ".value"), 
               names_prefix = "d_", 
               names_sep = "_", 
               names_transform = list(day = as.numeric), 
               values_drop_na = TRUE) %>% 
  left_join(cal %>% mutate(d = as.numeric(str_replace_all(d, "d_", ""))), by = c("day" = "d")) %>% 
  bind_cols(data.frame(predict(pp, .$day)))

# fitting the global models -----------------------------------------------------

### fit models

# Pr(sales > 0)
m_non0_base <- glm(update(f_base, cbind(non0, n - non0) ~ .), data = train_dat, family = binomial())
train_dat$pred_non0_base <- predict(m_non0_base, newdata = train_dat, type = "response")
# E(sales | sales > 0)
m_sales_base <- train_dat %>% 
  filter(non0 > 0) %>% 
  glm(update(f_base, sales ~ . + offset(log(non0))), data = ., family = poisson())
train_dat$pred_sales_base <- predict(m_sales_base, newdata = train_dat, type = "response")

### check coefs

# no RTTM required here.. yet!
coefs_non0_base <- tidy(m_non0_base) %>% 
  mutate(type = case_when(str_detect(term, "month") ~ "month", 
                          str_detect(term, "weekday") ~ "weekday", 
                          str_detect(term, "X") | term == "(Intercept)" ~ "time", 
                          TRUE ~ NA_character_)) %>% 
  group_by(type) %>% 
  mutate(sig_coef = sqrt(wtd.var(estimate, 1 / std.error^2) / n())) %>% 
  ungroup() %>% 
  mutate(est_regr = apply_rttm(estimate, std.error, sig_coef), 
         across(where(is.numeric), round, digits = 4)) %>% 
  select(type, term, estimate, std.error, est_regr)
coefs_sales_base <- tidy(m_sales_base) %>% 
  mutate(type = case_when(str_detect(term, "month") ~ "month", 
                          str_detect(term, "weekday") ~ "weekday", 
                          str_detect(term, "X") | term == "(Intercept)" ~ "time", 
                          TRUE ~ NA_character_)) %>% 
  group_by(type) %>% 
  mutate(sig_coef = sqrt(wtd.var(estimate, 1 / std.error^2) / n())) %>% 
  ungroup() %>% 
  mutate(est_regr = apply_rttm(estimate, std.error, sig_coef), 
         across(where(is.numeric), round, digits = 4)) %>% 
  select(type, term, estimate, std.error, est_regr)

#### quick check of results

# probs look good
train_dat %>% 
  sample_n(200) %>%
  # filter(year == 2014) %>%
  qplot(date, non0 / n, data = ., geom = "line") + 
  geom_line(aes(y = pred_non0_base), colour = "red")

# so do overall sales
train_dat %>% 
  mutate(pred_total_sales = pred_non0_base * pred_sales_base) %>% 
  # sample_n(200) %>%
  filter(year == 2014) %>%
  qplot(date, sales * non0 / n, data = ., geom = "line") + geom_line(aes(y = pred_total_sales), colour = "red")
  # qplot(pred_total_sales, sales * non0 / n, data = .) + geom_abline()


# tought to see a pattern (outside of holdiays) - good
train_dat %>% 
  mutate(pred_total_sales = pred_non0_base * pred_sales_base) %>% 
  arrange(desc(sales * non0 / n - pred_total_sales)) %>% 
  glimpse()
  

# saving -----------------------------------------------------------------------

save(m_non0_base, m_sales_base, pp, file = "models-global.RData")
