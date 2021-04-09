
# notes -------------------------------------------------------------
# still need to figure out aggregating with offsets...
# stuck in month predictions - something to do with factor levels???

library(arm)
library(lme4)
library(Hmisc)
library(tidyverse)
library(broom)
options(stringsAsFactors = F, digits = 3, max.print = 70)

load("train-simple.RData")

### helper funs to fit models

fit_binom <- function(df, f, off) {
  df <- df %>% mutate(o = {{ off }})
  glm(f, data = df, family = binomial(), offset = o)
}

fit_models <- function(data, fun, formula, off, ...) {
  data %>% 
    group_by(...) %>% 
    nest() %>% 
    mutate(model = map(data, fun, f = formula, off = {{ off }}), 
           group_coefs = map(model, tidy)) %>% 
    ungroup() %>% 
    select(..., group_coefs)
}

pred_fun <- function(data, coefs, f, ...) {
  preds <- data %>% 
    group_by(...) %>% 
    nest() %>% 
    left_join(coefs) %>%
    mutate(id = map(data, ~ .x %>% select(id, date)),
           X = map(data, ~ model.matrix(delete.response(terms(f)), 
                                        .x, 
                                        xlev = list(`factor(month)` = 1:12)))) %>%
    mutate(link_pred = map2(X, group_coefs, ~ as.numeric(.x %*% .y$estimate))) %>%
    ungroup() %>%
    select(id, link_pred) %>%
    unnest(cols = c(id, link_pred))
  
  # making sure that they get returned in the right order
  # imputing 0 if missing
  data %>% 
    select(id, date) %>% 
    left_join(preds, by = c("id", "date")) %>% 
    mutate(link_pred = coalesce(link_pred, 0))
}

apply_rttm <- function(y, sd_y, sigma, mu = 0) {
  (y / sd_y^2 + mu / sigma^2) / (1 / sd_y^2 + 1 / sigma^2)
}

# intercept --------------------------------------------------------------------

# no need to overcomplicate this - can just fit a glm
m_0_overall <- train_0 %>% 
  group_by(store_id, item_id) %>% 
  summarise(n = n(), n_0 = sum(sales == 0), .groups = "drop") %>% 
  glm(cbind(n_0, n - n_0) ~ store_id * item_id, data = ., family = binomial())
train_0$lpred_0_overall <- predict(m_0_overall, newdata = train_0)


# time -------------------------------------------------------------------------

# model specification
f_0_time <- formula(cbind(n_0, n - n_0) ~ day_z + I(day_z^2) + I(day_z^3) + I(day_z^4))


# setting up the data and model formula
# this data summarise doesn't do anything because there's one row per id/day
# but I'm doing it for the other variables so I figured why not -\__/-
train_0_time <- train_0 %>% 
  # filter(between(invlogit(lpred_0_overall), .1, .9)) %>% 
  group_by(id, date, store_id, item_id, day_z) %>% 
  summarise(n = n(), n_0 = sum(sales == 0), .groups = "drop") %>% 
  mutate(lpred_0_overall = predict(m_0_overall, newdata = .))

m_0_time_intercept <- glm(f_0_time, data = train_0_time, family = binomial(), offset = lpred_0_overall)
train_0_time$lpred_0_time_intercept <- as.numeric(model.matrix(m_0_time_intercept, train_0_time) %*% coef(m_0_time_intercept))

x <- train_0 %>% 
  filter(between(invlogit(lpred_0_overall), .1, .9)) %>% 
  group_by(date, day_z) %>% 
  summarise(n = n(), 
            n_0 = sum(sales == 0), 
            lpred_0_overall = logit(mean(invlogit(lpred_0_overall))), 
            .groups = "drop") %>% 
  glm(f_0_time, data = ., family = binomial(), offset = lpred_0_overall)



coefs_0_time_store <- train_0_time %>% 
  fit_models(fit_binom, f_0_time, lpred_0_overall + lpred_0_time_intercept, store_id)
coefs_0_time_item <- train_0_time %>% 
  fit_models(fit_binom, f_0_time, lpred_0_overall + lpred_0_time_intercept, item_id) 

train_0_time$lpred_0_time_store <- pred_fun(train_0_time, coefs_0_time_store, f_0_time, store_id)$link_pred
train_0_time$lpred_0_time_item <- pred_fun(train_0_time, coefs_0_time_item, f_0_time, item_id)$link_pred

coefs_0_time_store_item <- train_0_time %>% 
  fit_models(fit_binom, f_0_time, lpred_0_overall + lpred_0_time_intercept + lpred_0_time_store + lpred_0_time_item, store_id, item_id)

train_0$lpred_0_time <- as.numeric(model.matrix(delete.response(terms(f_0_time)), train_0) %*% coef(m_0_time_intercept)) + 
  pred_fun(train_0, coefs_0_time_store, f_0_time, store_id)$link_pred + 
  pred_fun(train_0, coefs_0_time_item, f_0_time, item_id)$link_pred +
  pred_fun(train_0, coefs_0_time_store_item, f_0_time, store_id, item_id)$link_pred

### basic validation

train_0 %>% 
  mutate(prob = invlogit(lpred_0_overall + lpred_0_time)) %>% 
  group_by(bin = cut(prob, breaks = seq(-.05, 1.05, by = .1))) %>% 
  summarise(
    n = n(), 
    pred = mean(prob), 
    act = mean(sales == 0)
  )

train_0 %>% 
  filter(store_id == "WI_2") %>% 
  qplot(date, ifelse(sales == 0, 1, 0), data = ., geom = "smooth") + 
  facet_wrap(~item_id, scales = "free_y") + 
  geom_line(aes(y = invlogit(lpred_0_overall + lpred_0_time)))


# month ------------------------------------------------------------------------

train_0_month <- train_0 %>% filter(between(invlogit(lpred_0_overall + lpred_0_time), .01, .99))
f_0_month <- formula(I(sales == 0) ~ 0 + factor(month))

# fitting the global intercepts for month
m_0_month_intercept <- glm(f_0_month, data = train_0_month, family = binomial(), offset = lpred_0_overall + lpred_0_time)
train_0_month$lpred_0_month_intercept <- as.numeric(model.matrix(m_0_month_intercept, train_0_month) %*% coef(m_0_month_intercept))


### models for store and item

coefs_0_month_store <- train_0_month %>% 
  fit_models(fit_binom, f_0_month, lpred_0_overall + lpred_0_time + lpred_0_month_intercept, store_id)
coefs_0_month_item <- train_0_month %>% 
  fit_models(fit_binom, f_0_month, lpred_0_overall + lpred_0_time + lpred_0_month_intercept, item_id) 

train_0_month$lpred_0_month_store <- pred_fun(train_0_month, coefs_0_month_store, f_0_month, store_id)$link_pred
train_0_month$lpred_0_month_item <- pred_fun(train_0_month, coefs_0_month_item, f_0_month, item_id)$link_pred

coefs_0_month_store_item <- train_0_month %>% 
  fit_models(fit_binom, f_0_month, lpred_0_overall + lpred_0_time + lpred_0_month_intercept + lpred_0_month_store + lpred_0_month_item, store_id, item_id)

train_0$lpred_0_month <- as.numeric(model.matrix(delete.response(terms(f_0_month)), train_0) %*% coef(m_0_month_intercept)) + 
  pred_fun(train_0, coefs_0_month_store, f_0_month, store_id)$link_pred + 
  pred_fun(train_0, coefs_0_month_item, f_0_month, item_id)$link_pred +
  pred_fun(train_0, coefs_0_month_store_item, f_0_month, store_id, item_id)$link_pred

### basic validation

train_0 %>% 
  mutate(prob = invlogit(lpred_0_overall + lpred_0_time + lpred_0_month)) %>% 
  group_by(bin = cut(prob, breaks = seq(-.05, 1.05, by = .1))) %>% 
  summarise(
    n = n(), 
    pred = mean(prob), 
    act = mean(sales == 0)
  )

train_0 %>% 
  mutate(prob = invlogit(lpred_0_overall + lpred_0_time + lpred_0_month)) %>% 
  group_by(store_id, month) %>% 
  summarise(
    n = n(), 
    pred = mean(prob), 
    act = mean(sales == 0)
  ) %>% 
  qplot(month, pred, data = ., colour = I("steelblue")) + 
  geom_jitter(aes(y = act), colour = "red") + 
  facet_wrap(~store_id, scales = "free_y")



# weekday ------------------------------------------------------------------------

train_0_weekday <- train_0 %>% filter(between(invlogit(lpred_0_overall + lpred_0_time + lpred_0_month), .01, .99))
f_0_weekday <- formula(I(sales == 0) ~ 0 + weekday)
m_0_weekday_intercept <- glm(f_0_weekday, data = train_0_weekday, family = binomial(), 
                             offset = lpred_0_overall + lpred_0_time + lpred_0_month)
train_0_weekday$lpred_0_weekday_intercept <- model.matrix(delete.response(terms(m_0_weekday_intercept)), train_0_weekday) %*% coef(m_0_weekday_intercept)
train_0_weekday$lpred_0_weekday_base <- with(train_0_weekday, lpred_0_overall + lpred_0_time + lpred_0_month + lpred_0_weekday_intercept)



coefs_0_weekday_store <- train_0_weekday %>% 
  group_by(store_id) %>% 
  nest() %>% 
  mutate(model = map(data, fit_binom, f = f_0_weekday, offset_name = "lpred_0_weekday_base"), 
         store_coefs = map(model, tidy)) %>% 
  ungroup() %>% 
  select(store_id, store_coefs)
coefs_0_weekday_item <- train_0_weekday %>% 
  group_by(item_id) %>% 
  nest() %>% 
  mutate(model = map(data, fit_binom, f = f_0_weekday, offset_name = "lpred_0_weekday_base"), 
         item_coefs = map(model, tidy)) %>% 
  ungroup() %>% 
  select(item_id, item_coefs)

lpred_0_weekday_item <- train_0_weekday %>% 
  group_by(item_id) %>% 
  nest() %>% 
  left_join(coefs_0_weekday_item, by = "item_id") %>%
  mutate(id = map(data, ~ .x %>% select(id, date)), 
         X = map(data, ~ model.matrix(delete.response(terms(f_0_weekday)), .x))) %>% 
  mutate(lpred_0_weekday_item = map2(X, item_coefs, ~ .x %*% .y$estimate)) %>% 
  ungroup() %>% 
  select(id, lpred_0_weekday_item) %>% 
  unnest(cols = c(id, lpred_0_weekday_item)) %>% 
  mutate(lpred_0_weekday_item = as.numeric(lpred_0_weekday_item))
lpred_0_weekday_store <- train_0_weekday %>% 
  group_by(store_id) %>% 
  nest() %>% 
  left_join(coefs_0_weekday_store, by = "store_id") %>%
  mutate(id = map(data, ~ .x %>% select(id, date)), 
         X = map(data, ~ model.matrix(delete.response(terms(f_0_weekday)), .x))) %>% 
  mutate(lpred_0_weekday_store = map2(X, store_coefs, ~ .x %*% .y$estimate)) %>% 
  ungroup() %>% 
  select(id, lpred_0_weekday_store) %>% 
  unnest(cols = c(id, lpred_0_weekday_store)) %>% 
  mutate(lpred_0_weekday_store = as.numeric(lpred_0_weekday_store))

coefs_0_weekday_store_item <- train_0_weekday %>% 
  left_join(lpred_0_weekday_store, by = c("id", "date")) %>% 
  left_join(lpred_0_weekday_item, by = c("id", "date")) %>% 
  mutate(lpred_0_weekday_store_item = lpred_0_weekday_base + lpred_0_weekday_store + lpred_0_weekday_item) %>% 
  group_by(store_id, item_id) %>% 
  nest() %>% 
  mutate(model = map(data, fit_binom, f = f_0_weekday, offset_name = "lpred_0_weekday_store_item"), 
         store_item_coefs = map(model, tidy)) %>% 
  ungroup() %>% 
  select(item_id, store_id, store_item_coefs)


lpred_0_weekday_store_item <- train_0_weekday %>% 
  group_by(store_id, item_id) %>% 
  nest() %>% 
  left_join(coefs_0_weekday_store_item, by = c("store_id", "item_id")) %>%
  mutate(id = map(data, ~ .x %>% select(id, date)), 
         X = map(data, ~ model.matrix(delete.response(terms(f_0_weekday)), .x))) %>% 
  mutate(lpred_0_weekday_store_item = map2(X, store_item_coefs, ~ as.numeric(.x %*% .y$estimate))) %>% 
  ungroup() %>% 
  select(id, lpred_0_weekday_store_item) %>% 
  unnest(cols = c(id, lpred_0_weekday_store_item))

lpred_0_weekday <- train_0 %>% 
  mutate(lpred_0_weekday_intercept = as.numeric(model.matrix(delete.response(terms(m_0_weekday_intercept)), data = .) %*% coef(m_0_weekday_intercept))) %>% 
  left_join(lpred_0_weekday_item, by = c("id", "date")) %>% 
  left_join(lpred_0_weekday_store, by = c("id", "date")) %>% 
  left_join(lpred_0_weekday_store_item, by = c("id", "date")) %>% 
  mutate(lpred_0_weekday = coalesce(lpred_0_weekday_intercept, 0) + coalesce(lpred_0_weekday_item, 0) + coalesce(lpred_0_weekday_store, 0) + coalesce(lpred_0_weekday_store_item, 0)) %>% 
  select(id, date, lpred_0_weekday)

train_0 <- train_0 %>% 
  left_join(lpred_0_weekday, by = c("id", "date"), suffix = c("_old", ""))


### basic validation
train_0 %>% 
  mutate(prob = invlogit(lpred_0_overall + lpred_0_time + lpred_0_month + lpred_0_weekday)) %>% 
  group_by(bin = cut(prob, breaks = seq(-.05, 1.05, by = .1))) %>% 
  summarise(
    n = n(), 
    pred = mean(prob), 
    act = mean(sales == 0)
  )

train_0 %>% 
  mutate(prob = invlogit(lpred_0_overall + lpred_0_time + lpred_0_month + lpred_0_weekday)) %>% 
  group_by(store_id, weekday) %>% 
  summarise(
    n = n(), 
    pred = mean(prob), 
    act = mean(sales == 0)
  ) %>% 
  qplot(weekday, pred, data = ., colour = I("steelblue")) + 
  geom_jitter(aes(y = act), colour = "red") + 
  facet_wrap(~store_id, scales = "free_y")

# regress coefs ---------------------------------------------------------------

regress_coefs <- function(data, unnest_var, type, ...) {
  data %>% 
    unnest(cols = {{ unnest_var }}) %>% 
    group_by(term) %>% 
    mutate(bt_sd = sqrt(wtd.var(estimate, 1 / std.error^2))) %>% 
    ungroup() %>% 
    mutate(est_regr = apply_rttm(estimate, std.error, bt_sd), 
           type = type) %>% 
    select(type, ..., term, estimate, std.error, bt_sd, est_regr) %>% 
    rename(est_raw = estimate, se = std.error) %>% 
    mutate(across(where(is.numeric), round, digits = 2)) %>% 
    complete(nesting(type, term), ..., fill = list(est_regr = 0)) %>% 
    nest(regr_coefs = c(term, est_raw, se, bt_sd, est_regr))
}

# overall coefs
overall_0 <- tidy(m_0_intercept)
overall_0_store <- tidy(m_0_store)
overall_0_item <- tidy(m_0_item)
overall_0_store_item <- tidy(m_0_store_item)
# time trend coefs
regr_0_time_store <- regress_coefs(coefs_0_time_store, store_coefs, "time", store_id)
regr_0_time_item <- regress_coefs(coefs_0_time_item, item_coefs, "time", item_id)
regr_0_time_store_item <- regress_coefs(coefs_0_time_store_item, store_item_coefs, "time", store_id, item_id)
# monthly coefs
regr_0_month_store <- regress_coefs(coefs_0_month_store, store_coefs, "month", store_id)
regr_0_month_item <- regress_coefs(coefs_0_month_item, item_coefs, "month", item_id)
regr_0_month_store_item <- regress_coefs(coefs_0_month_store_item, store_item_coefs, "month", store_id, item_id)
# weekday coefs
regr_0_weekday_store <- regress_coefs(coefs_0_weekday_store, store_coefs, "weekday", store_id)
regr_0_weekday_item <- regress_coefs(coefs_0_weekday_item, item_coefs, "weekday", item_id)
regr_0_weekday_store_item <- regress_coefs(coefs_0_weekday_store_item, store_item_coefs, "weekday", store_id, item_id)

# generate predictions ---------------------------------------------------------------



train_0_preds <- train_0 %>% 
  mutate(
    lpred0 = as.numeric(predict(m_0_weekday_intercept, newdata = train_0)) + 
      # store/item fixed effects
      # time effects
      pred_fun(train_0, regr_0_time_store, f_0_time, store_id)$lpred + 
      pred_fun(train_0, regr_0_time_item, f_0_time, item_id)$lpred  + 
      pred_fun(train_0, regr_0_time_store_item, f_0_time, store_id, item_id)$lpred + 
      # month effects
      pred_fun(train_0, regr_0_month_store, f_0_month, store_id)$lpred +
      pred_fun(train_0, regr_0_month_item, f_0_month, item_id)$lpred +
      pred_fun(train_0, regr_0_month_store_item, f_0_month, store_id, item_id)$lpred +
      # weekday effects
      pred_fun(train_0, regr_0_weekday_store, f_0_weekday, store_id)$lpred + 
      pred_fun(train_0, regr_0_weekday_item, f_0_weekday, item_id)$lpred + 
      pred_fun(train_0, regr_0_weekday_store_item, f_0_weekday, store_id, item_id)$lpred
  ) %>% 
  select(id:sales, lpred0)



### basic validation

# overall calibration
train_0_preds %>% 
  mutate(prob = invlogit(lpred0)) %>% 
  group_by(bin = cut(prob, breaks = seq(-.05, 1.05, by = .1))) %>% 
  summarise(
    n = n(), 
    pred = mean(prob), 
    act = mean(sales == 0)
  )

# time trends
train_0_preds %>% 
  mutate(prob = invlogit(lpred0)) %>% 
  ggplot(aes(x = day)) + 
  geom_smooth(aes(y = ifelse(sales == 0, 1, 0)), colour = "purple4") + 
  geom_smooth(aes(y = prob), colour = "steelblue") + 
  facet_wrap(~store_id, scales = "free_y")
train_0_preds %>% 
  mutate(prob = invlogit(lpred0)) %>% 
  ggplot(aes(x = day)) + 
  geom_smooth(aes(y = ifelse(sales == 0, 1, 0)), colour = "purple4") + 
  geom_smooth(aes(y = prob), colour = "steelblue") + 
  facet_wrap(~item_id, scales = "free_y")
train_0_preds %>% 
  mutate(prob = invlogit(lpred0)) %>% 
  ggplot(aes(x = day)) + 
  geom_smooth(aes(y = ifelse(sales == 0, 1, 0)), colour = "purple4") + 
  geom_smooth(aes(y = prob), colour = "steelblue") + 
  facet_wrap(~weekday, scales = "free_y")

# month trends
train_0_preds %>% 
  mutate(prob = invlogit(lpred0)) %>% 
  group_by(month) %>% 
  summarise(
    n = n(), 
    pred = mean(prob), 
    act = mean(sales == 0)
  )

# weekday trends
train_0_preds %>% 
  mutate(prob = invlogit(lpred0)) %>% 
  group_by(weekday) %>% 
  summarise(
    n = n(), 
    pred = mean(prob), 
    act = mean(sales == 0)
  )
