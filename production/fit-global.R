
# notes ------------------------------------------------------------------------

# interesting note from month*weekday interaction
# weekends in the summer might have slightly more 0s
# couple days before thanksgiving have slightly fewer

# in final date vs sales plot - is actual always more extreme?

# setup ------------------------------------------------------------------------

library(feather)
library(Hmisc)
library(tidyverse)
library(broom)
source("production/helper-funs.R")
set.seed(314)

cal <- read_feather("data/data-calendar-clean.feather")
train_raw <- read_feather("data/data-train-wide.feather")
train_raw %>% select(1:20) %>% glimpse()

# 10th order polynomial decomposition for indendence!
pp <- poly(1:1969, degrees = 10)


# large enough sample I can just throw everything in here
f_base <- formula(~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + 
                    factor(month) + weekday + snap_CA + snap_TX + snap_WI)
f_time <- formula(~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10)
f_month <- formula(~ 0 + factor(month))
f_weekday <- formula(~ 0 + weekday)
f_snap <- formula(~ 0 + factor(is_snap))

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
  left_join(cal, by = c("day")) %>% 
  bind_cols(data.frame(predict(pp, .$day)))

# fitting the global models -----------------------------------------------------

### fit models

# Pr(sales > 0)
m_non0_base <- glm(update(f_base, cbind(non0, n - non0) ~ .), data = train_dat, family = binomial())
train_dat$pred_non0_base <- predict(m_non0_base, newdata = train_dat, type = "response")
# E(sales | sales > 0)
m_sales_base <- train_dat %>% 
  filter(non0 > 0) %>% 
  glm(update(f_base, sales ~ .), data = ., family = poisson(), offset = log(non0))
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
  # qplot(date, sales * non0 / n, data = ., geom = "line") + geom_line(aes(y = pred_total_sales), colour = "red")
  qplot(pred_total_sales, sales * non0 / n, data = .) + geom_abline()


# tought to see a pattern (outside of holdiays) - good
train_dat %>% 
  mutate(pred_total_sales = pred_non0_base * pred_sales_base) %>% 
  arrange(desc(sales * non0 / n - pred_total_sales)) %>% 
  glimpse()

qplot(day, sales / 32000, data = train_dat, geom = c("line", "smooth")) + 
  geom_vline(aes(xintercept = 1941)) + 
  xlim(c(1000, 2000)) + ylim(c(0, 3))
  
# saving -----------------------------------------------------------------------

# global predictions
preds_global <- tibble(day = 1:1969) %>% 
  left_join(cal, by = c("day")) %>% 
  # cute hack to be very careful extrapolatin
  bind_cols(data.frame(predict(pp, pmin(1941, .$day)))) %>% 
  mutate(non0 = 1) %>% 
  mutate(lp_non0_base = predict(m_non0_base, newdata = .), 
         lp_sales_base = predict(m_sales_base, newdata = .)) %>% 
  select(day, lp_non0_base, lp_sales_base) %>% 
  mutate(across(where(is.numeric), round, digits = 4))
qplot(day, arm::invlogit(lp_non0_base) * exp(lp_sales_base), data = preds_global, geom = 'line') + 
  geom_vline(aes(xintercept = 1941)) + 
  xlim(c(1000, 2000)) + ylim(c(0, 3))


pp_global <- tibble(day = 1:1969) %>% 
  bind_cols(data.frame(predict(pp, .$day))) %>% 
  mutate(across(where(is.numeric), round, digits = 6))

save(m_non0_base, m_sales_base, pp, file = "fitted-models/models-global.RData")
save(pp_global, preds_global, f_time, f_month, f_weekday, f_snap, file = "predictions/preds-global.RData")
