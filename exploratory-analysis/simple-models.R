
# notes -------------------------------------------------------------
# variables: price, SNAP, natural disasters, AR(1/7/365), holidays/day of year
# fix 0s(?): are they when store is out of stock, data errors? - FOODS_3_732
# check time results for "FOODS_3_077", FOODS_3_192, HOBBIES_1_369, 
# difficult ones: FOODS_3_441
# better idea: fit an mgcv smoother, then fit polynomials to the predictions from that???
# probably a clever way to fit number of polynomials based on edf of smoother
# TX_1/FOODS_1_218 probably needs a more complicated smoother
# RX_1/FOODS_3_555 why not centered at 0?
# could try a seasonal 1:365 effect rather than monthly
# correlation between non0 and sales effects?  some sort of pooling htere...
# is something off with snaps?  why are store snap=1 pos but negative for items

library(arm)
library(lme4)
library(Hmisc)
library(tidyverse)
library(broom)
options(stringsAsFactors = F, digits = 3, max.print = 50)

load("train-simple.RData")


fit_binom <- function(df, f, offset_name) {
  df$o <- df[[offset_name]]
  glm(f, data = df, family = binomial(), offset = o)
}
apply_rttm <- function(y, sd_y, sigma, mu = 0) {
  (y / sd_y^2 + mu / sigma^2) / (1 / sd_y^2 + 1 / sigma^2)
}

# intercept --------------------------------------------------------------------

train_0_intercept <- train_0 %>% 
  group_by(store_id, item_id) %>% 
  summarise(n = n(), sales = sum(sales == 0), .groups = "drop")

m_0_intercept <- glm(cbind(sales, n - sales) ~ 1, data = train_0_intercept, family = binomial())
train_0_intercept$lpred_0_intercept <- predict(m_0_intercept, newdata = train_0_intercept)

m_0_store <- glm(cbind(sales, n - sales) ~ 0 + store_id, data = train_0_intercept, offset = lpred_0_intercept, family = binomial())
m_0_item <- glm(cbind(sales, n - sales) ~ 0 + item_id, data = train_0_intercept, offset = lpred_0_intercept, family = binomial())

X_0_intercept <- model.matrix(delete.response(terms(m_0_intercept)), train_0_intercept)
X_0_store <- model.matrix(delete.response(terms(m_0_store)), train_0_intercept)
X_0_item <- model.matrix(delete.response(terms(m_0_item)), train_0_intercept)
train_0_intercept$lpred_store_item <- as.numeric(
  X_0_intercept %*% coef(m_0_intercept) + 
    X_0_store %*% coef(m_0_store) + 
    X_0_item %*% coef(m_0_item)
)

m_0_store_item <- glm(cbind(sales, n - sales) ~ 0 + store_id:item_id, 
                      data = train_0_intercept, family = binomial(), offset = lpred_store_item)

train_0$lpred_0_overall <- model.matrix(delete.response(terms(m_0_intercept)), train_0) %*% coef(m_0_intercept) + 
  model.matrix(delete.response(terms(m_0_store)), train_0) %*% coef(m_0_store) + 
  model.matrix(delete.response(terms(m_0_item)), train_0) %*% coef(m_0_item) + 
  model.matrix(delete.response(terms(m_0_store_item)), train_0) %*% coef(m_0_store_item)


# basic validation
m2 <- glm(cbind(sales, n - sales) ~ store_id * item_id, 
          data = train_0_intercept, family = binomial())
train_0 %>% 
  mutate(p = predict(m2, newdata = .)) %>% 
  select(p, lpred_0_overall) %>% 
  cor()

# time -------------------------------------------------------------------------

train_0_time <- train_0
f_0_time <- formula(I(sales == 0) ~ day_z + I(day_z^2) + I(day_z^3) + I(day_z^4))

m_0_time_intercept <- glm(I(sales == 0) ~ day_z + I(day_z^2) + I(day_z^3) + I(day_z^4), 
                          data = train_0_time, family = binomial(), offset = lpred_0_overall)
train_0_time$lpred_0_time_intercept <- model.matrix(delete.response(terms(m_0_time_intercept)), train_0_time) %*% coef(m_0_time_intercept)
train_0_time$lpred_0_time_base <- train_0_time$lpred_0_overall + train_0_time$lpred_0_time_intercept


coefs_0_time_store <- train_0_time %>% 
  group_by(store_id) %>% 
  nest() %>% 
  mutate(model = map(data, fit_binom, f = f_0_time, offset_name = "lpred_0_time_base"), 
         store_coefs = map(model, tidy)) %>% 
  ungroup() %>% 
  select(store_id, store_coefs)
coefs_0_time_item <- train_0_time %>% 
  group_by(item_id) %>% 
  nest() %>% 
  mutate(model = map(data, fit_binom, f = f_0_time, offset_name = "lpred_0_time_base"), 
         item_coefs = map(model, tidy)) %>% 
  ungroup() %>% 
  select(item_id, item_coefs)

lpred_0_time_item <- train_0_time %>% 
  group_by(item_id) %>% 
  nest() %>% 
  left_join(coefs_0_time_item, by = "item_id") %>%
  mutate(id = map(data, ~ .x %>% select(id, date)), 
         X = map(data, ~ model.matrix(delete.response(terms(f_0_time)), .x))) %>% 
  mutate(lpred_0_time_item = map2(X, item_coefs, ~ .x %*% .y$estimate)) %>% 
  ungroup() %>% 
  select(id, lpred_0_time_item) %>% 
  unnest(cols = c(id, lpred_0_time_item)) %>% 
  mutate(lpred_0_time_item = as.numeric(lpred_0_time_item))
lpred_0_time_store <- train_0_time %>% 
  group_by(store_id) %>% 
  nest() %>% 
  left_join(coefs_0_time_store, by = "store_id") %>%
  mutate(id = map(data, ~ .x %>% select(id, date)), 
         X = map(data, ~ model.matrix(delete.response(terms(f_0_time)), .x))) %>% 
  mutate(lpred_0_time_store = map2(X, store_coefs, ~ .x %*% .y$estimate)) %>% 
  ungroup() %>% 
  select(id, lpred_0_time_store) %>% 
  unnest(cols = c(id, lpred_0_time_store)) %>% 
  mutate(lpred_0_time_store = as.numeric(lpred_0_time_store))

coefs_0_time_store_item <- train_0_time %>% 
  left_join(lpred_0_time_store, by = c("id", "date")) %>% 
  left_join(lpred_0_time_item, by = c("id", "date")) %>% 
  mutate(lpred_0_time_store_item = lpred_0_overall + lpred_0_time_intercept + lpred_0_time_store + lpred_0_time_item) %>% 
  group_by(store_id, item_id) %>% 
  nest() %>% 
  mutate(model = map(data, fit_binom, f = f_0_time, offset_name = "lpred_0_time_store_item"), 
         store_item_coefs = map(model, tidy)) %>% 
  ungroup() %>% 
  select(item_id, store_id, store_item_coefs)

lpred_0_time_store_item <- train_0_time %>% 
  group_by(store_id, item_id) %>% 
  nest() %>% 
  left_join(coefs_0_time_store_item, by = c("store_id", "item_id")) %>%
  mutate(id = map(data, ~ .x %>% select(id, date)), 
         X = map(data, ~ model.matrix(delete.response(terms(f_0_time)), .x))) %>% 
  mutate(lpred_0_time_store_item = map2(X, store_item_coefs, ~ as.numeric(.x %*% .y$estimate))) %>% 
  ungroup() %>% 
  select(id, lpred_0_time_store_item) %>% 
  unnest(cols = c(id, lpred_0_time_store_item))

lpred_0_time <- train_0 %>% 
  mutate(lpred_0_time_intercept = as.numeric(model.matrix(delete.response(terms(m_0_time_intercept)), data = .) %*% coef(m_0_time_intercept))) %>% 
  left_join(lpred_0_time_item, by = c("id", "date")) %>% 
  left_join(lpred_0_time_store, by = c("id", "date")) %>% 
  left_join(lpred_0_time_store_item, by = c("id", "date")) %>% 
  mutate(lpred_0_time = lpred_0_time_intercept + lpred_0_time_item + lpred_0_time_store + lpred_0_time_store_item) %>% 
  select(id, date, lpred_0_time)

train_0 <- train_0 %>% 
  left_join(lpred_0_time, by = c("id", "date"), suffix = c("_old", ""))

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
m_0_month_intercept <- glm(I(sales == 0) ~ 0 + factor(month), 
                           data = train_0_month, family = binomial(), offset = lpred_0_overall + lpred_0_time)
train_0_month$lpred_0_month_intercept <- model.matrix(delete.response(terms(m_0_month_intercept)), train_0_month) %*% coef(m_0_month_intercept)
train_0_month$lpred_0_month_base <- with(train_0_month, lpred_0_overall + lpred_0_time + lpred_0_month_intercept)


coefs_0_month_store <- train_0_month %>% 
  group_by(store_id) %>% 
  nest() %>% 
  mutate(model = map(data, fit_binom, f = f_0_month, offset_name = "lpred_0_month_base"), 
         store_coefs = map(model, tidy)) %>% 
  ungroup() %>% 
  select(store_id, store_coefs)
coefs_0_month_item <- train_0_month %>% 
  group_by(item_id) %>% 
  nest() %>% 
  mutate(model = map(data, fit_binom, f = f_0_month, offset_name = "lpred_0_month_base"), 
         item_coefs = map(model, tidy)) %>% 
  ungroup() %>% 
  select(item_id, item_coefs)

lpred_0_month_item <- train_0_month %>% 
  group_by(item_id) %>% 
  nest() %>% 
  left_join(coefs_0_month_item, by = "item_id") %>%
  mutate(id = map(data, ~ .x %>% select(id, date)), 
         X = map(data, ~ model.matrix(delete.response(terms(f_0_month)), .x))) %>% 
  mutate(lpred_0_month_item = map2(X, item_coefs, ~ .x %*% .y$estimate)) %>% 
  ungroup() %>% 
  select(id, lpred_0_month_item) %>% 
  unnest(cols = c(id, lpred_0_month_item)) %>% 
  mutate(lpred_0_month_item = as.numeric(lpred_0_month_item))
lpred_0_month_store <- train_0_month %>% 
  group_by(store_id) %>% 
  nest() %>% 
  left_join(coefs_0_month_store, by = "store_id") %>%
  mutate(id = map(data, ~ .x %>% select(id, date)), 
         X = map(data, ~ model.matrix(delete.response(terms(f_0_month)), .x))) %>% 
  mutate(lpred_0_month_store = map2(X, store_coefs, ~ .x %*% .y$estimate)) %>% 
  ungroup() %>% 
  select(id, lpred_0_month_store) %>% 
  unnest(cols = c(id, lpred_0_month_store)) %>% 
  mutate(lpred_0_month_store = as.numeric(lpred_0_month_store))

coefs_0_month_store_item <- train_0_month %>% 
  left_join(lpred_0_month_store, by = c("id", "date")) %>% 
  left_join(lpred_0_month_item, by = c("id", "date")) %>% 
  mutate(lpred_0_month_store_item = lpred_0_month_base + lpred_0_month_store + lpred_0_month_item) %>% 
  group_by(store_id, item_id) %>% 
  nest() %>% 
  mutate(model = map(data, fit_binom, f = f_0_month, offset_name = "lpred_0_month_store_item"), 
         store_item_coefs = map(model, tidy)) %>% 
  ungroup() %>% 
  select(item_id, store_id, store_item_coefs)


lpred_0_month_store_item <- train_0_month %>% 
  group_by(store_id, item_id) %>% 
  nest() %>% 
  left_join(coefs_0_month_store_item, by = c("store_id", "item_id")) %>%
  mutate(id = map(data, ~ .x %>% select(id, date)), 
         X = map(data, ~ model.matrix(delete.response(terms(f_0_month)), .x))) %>% 
  mutate(lpred_0_month_store_item = map2(X, store_item_coefs, ~ as.numeric(.x %*% .y$estimate))) %>% 
  ungroup() %>% 
  select(id, lpred_0_month_store_item) %>% 
  unnest(cols = c(id, lpred_0_month_store_item))

lpred_0_month <- train_0 %>% 
  mutate(lpred_0_month_intercept = as.numeric(model.matrix(delete.response(terms(m_0_month_intercept)), data = .) %*% coef(m_0_month_intercept))) %>% 
  left_join(lpred_0_month_item, by = c("id", "date")) %>% 
  left_join(lpred_0_month_store, by = c("id", "date")) %>% 
  left_join(lpred_0_month_store_item, by = c("id", "date")) %>% 
  mutate(lpred_0_month = lpred_0_month_intercept + coalesce(lpred_0_month_item, 0) + coalesce(lpred_0_month_store, 0) + coalesce(lpred_0_month_store_item, 0)) %>% 
  select(id, date, lpred_0_month)

train_0 <- train_0 %>% 
  left_join(lpred_0_month, by = c("id", "date"), suffix = c("_old", ""))


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

pred_fun <- function(data, coefs, f, ...) {
  data %>% 
    group_by(...) %>% 
    nest() %>% 
    left_join(coefs) %>%
    mutate(id = map(data, ~ .x %>% select(id, date)),
           X = map(data, ~ model.matrix(delete.response(terms(f)), .x))) %>%
    mutate(lpred = map2(X, regr_coefs, ~ as.numeric(.x %*% .y$est_regr))) %>%
    ungroup() %>%
    select(id, lpred) %>%
    unnest(cols = c(id, lpred))
  
}

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
      # pred_fun(train_0, regr_0_month_store_item, f_0_month, store_id, item_id)$lpred +
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
