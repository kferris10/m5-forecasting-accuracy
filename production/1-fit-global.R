
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
library(mgcv)
source("production/helper-funs.R")
options(stringsAsFactors = F, digits = 3, show.signif.stars = F)
set.seed(314)

cal <- read_feather("data/data-calendar-clean.feather")
train_raw <- read_feather("data/data-train-wide.feather")
train_raw %>% select(1:20) %>% glimpse()


# large enough sample I can just throw everything in here
f_base <- formula(~ s(day, k = 10, bs = "ts") + 
                    factor(month) + weekday + 
                    snap_CA + snap_TX + snap_WI + 
                    mem + mem_p1 + mem_p2 + mem_m1 + mem_m2 + mem_m3 + mem_m8 + mem_m9 + 
                    mday + mday_p1 + mday_p2 + mday_m1 + mday_m2 + 
                    stpats + vday + columbus + hallo + ind + labor + vet + 
                    sb + sb_p1 + sb_p2 + sb_m1 + sb_m2 + sb_m3 + 
                    cinco + cinco_p1 + cinco_p2 + cinco_m1 + cinco_m2 + 
                    easter + easter_p1 + easter_p2 + easter_m1 + easter_m2 + 
                    mlk + mlk_p1 + mlk_m1 + mlk_m2 + 
                    fday + fday_p1 + fday_p2 + fday_m1 + fday_m2)
f_time <- formula(~ s(day, k = 8, bs = "ts"))
f_month <- formula(~ 0 + factor(month))
f_weekday <- formula(~ 0 + weekday)
f_snap <- formula(~ 0 + factor(is_snap))
f_event <- formula(~0 + 
                     mem + mem_p1 + mem_p2 + mem_m1 + mem_m2 + mem_m3 + mem_m8 + mem_m9 + 
                     mday + mday_p1 + mday_p2 + mday_m1 + mday_m2 + 
                     stpats + vday + columbus + hallo + ind + labor + vet + 
                     sb + sb_p1 + sb_p2 + sb_m1 + sb_m2 + sb_m3 + 
                     cinco + cinco_p1 + cinco_p2 + cinco_m1 + cinco_m2 + 
                     easter + easter_p1 + easter_p2 + easter_m1 + easter_m2 + 
                     mlk + mlk_p1 + mlk_m1 + mlk_m2 + 
                     fday + fday_p1 + fday_p2 + fday_m1 + fday_m2)

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
  left_join(cal, by = c("day"))

# fitting the global models -----------------------------------------------------

### fit models

# Pr(sales > 0)
m_non0_base <- gam(update(f_base, cbind(non0, n - non0) ~ .), data = train_dat, family = binomial())
train_dat$pred_non0_base <- as.numeric(predict(m_non0_base, newdata = train_dat, type = "response"))
# E(sales | sales > 0)
m_sales_base <- train_dat %>% 
  filter(non0 > 0) %>% 
  gam(update(f_base, sales ~ .), data = ., family = poisson(), offset = log(non0))
train_dat$pred_sales_base <- as.numeric(predict(m_sales_base, newdata = train_dat, type = "response")) * 
  # have to include the offset manually for gams - interesting
  train_dat$non0

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
  mutate(day_raw = day, 
         day = pmin(1941, day)) %>% 
  mutate(lp_non0_base = predict(m_non0_base, newdata = .), 
         lp_sales_base = predict(m_sales_base, newdata = .)) %>% 
  mutate(day = day_raw) %>% 
  select(day, lp_non0_base, lp_sales_base) %>% 
  mutate(across(where(is.numeric), round, digits = 4))
qplot(day, arm::invlogit(lp_non0_base) * exp(lp_sales_base), data = preds_global, geom = 'line') + 
  geom_vline(aes(xintercept = 1941)) + 
  xlim(c(1000, 2000)) + ylim(c(0, 3))


save(m_non0_base, m_sales_base, file = "fitted-models/models-global.RData")
save(preds_global, f_time, f_month, f_weekday, f_snap, f_event, 
     file = "predictions/preds-global.RData")
