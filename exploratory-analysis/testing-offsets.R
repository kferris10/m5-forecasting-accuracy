
# well this didn't work...

library(lme4)
library(arm)
library(tidyverse)
options(stringsAsFactors = F, digits = 3)


load("train-simple.RData")

# these are the same -----------------------------------------------------------

m_base <- glm(I(sales == 0) ~ item_id + store_id, data = train_0, family = binomial())
m_base2 <- train_0 %>% 
  group_by(item_id, store_id) %>% 
  summarise(
    n = n(), 
    n_0 = sum(sales == 0), 
    .groups = "drop"
  ) %>% 
  glm(cbind(n_0, n - n_0) ~ item_id + store_id, data = ., family = binomial())

round(cbind(coef(m_base), coef(m_base2)), 2)

# check ------------------------------------------------------------------------
# can aggregate and then model - results are the same


train_0$lpred_base <- predict(m_base2, newdata = train_0)

m3 <- glm(I(sales == 0) ~ 0 + item_id:store_id, 
          data = train_0, family = binomial(), offset = lpred_base)
m4 <- train_0 %>% 
  group_by(item_id, store_id) %>% 
  summarise(
    n = n(), 
    n_0 = sum(sales == 0), 
    lpred_base3 = mean(lpred_base), 
    .groups = "drop"
  ) %>% 
  mutate(lpred_base2 = predict(m_base2, newdata = .)) %>% 
  glm(cbind(n_0, n - n_0) ~ 0 + item_id:store_id, 
      data = ., family = binomial(), offset = lpred_base3)



cor(coef(m3), coef(m4))

# checking w/day of year effects -----------------------------------------------

train_month <- train_0 %>% 
  select(id:date, month, sales, lpred_base)

m_base3 <- glm(I(sales == 0) ~ store_id * item_id, data = train_month, family = binomial())
train_month$lpred_base3 <- predict(m_base3, newdata = train_month)

m5 <- train_month %>% 
  # filter(between(invlogit(lpred_base3), .01, .99)) %>%
  glm(I(sales == 0) ~ 0 + factor(month), 
          data = ., family = binomial(), offset = lpred_base3)
m6 <- train_month %>% 
  # filter(between(invlogit(lpred_base3), .01, .99)) %>%
  group_by(month) %>% 
  summarise(
    n = n(), 
    n_0 = sum(sales == 0), 
    lpred2 = mean(lpred_base3), 
    lpred_base_agg = logit(mean(invlogit(lpred_base3))),
    .groups = "drop"
  ) %>% 
  # mutate(lpred_base3 = predict(m_base3, newdata = .)) %>% 
  glm(cbind(n_0, n - n_0) ~ 0 + factor(month), 
      data = ., family = binomial(), offset = lpred_base_agg)

arm::display(m5)
arm::display(m6)



train_month %>% 
  # filter(between(invlogit(lpred_base3), .01, .99)) %>%
  mutate(lpred_base_agg = lpred_base3) %>%
  mutate(p5 =  predict(m5, newdata = ., type = "response"), 
         p6 = predict(m6, newdata = ., type = "response")) %>% 
  # summary()
  group_by(month) %>%
  summarise(n = n(), 
            ll5 = sum(dbinom(sales == 0, 1, p5, log = T)), 
            ll6 = sum(dbinom(sales == 0, 1, p6, log = T)), 
            act = mean(sales == 0), 
            # bas3 = invlogit(mean(lpred_base_agg)), 
            base3 = mean(invlogit(lpred_base_agg)), 
            p5 = mean(p5), 
            p6 = mean(p6), 
            ll_delta = ll5 - ll6, 
            b_delta = act - base3) 

train_month %>% 
  mutate(lpred_base_agg = lpred_base3) %>% 
  mutate(p5 = predict(m5, newdata = .), 
         p6 = predict(m6, newdata = .)) %>% 
  select(p5, p6) %>% 
  summary()

train_month %>% 
  group_by(month) %>% 
  summarise(
    n = n(), 
    n_0 = sum(sales == 0), 
    lpred_base_agg = mean(lpred_base3),
    .groups = "drop"
  ) %>% 
  filter(month == 1)

# checing w/ poisson ----------------------------------------------------------

m_base4 <- glm(sales ~ item_id, data = train_0, family = poisson())
train_0$logpred_base4 <- predict(m_base4, newdata = train_0)

m7 <- glm(sales ~ 0 + factor(month), 
          data = train_0, family = poisson(), offset = logpred_base4)
m8 <- train_0 %>% 
  group_by(month) %>% 
  summarise(
    n = n(), 
    sales = sum(sales), 
    logpred_base_agg = log(mean(exp(logpred_base4))),
    .groups = "drop"
  ) %>% 
  # mutate(logpred_base4 = predict(m_base4, newdata = .)) %>%
  glm(sales ~ 0 + offset(log(n)) + factor(month), 
      data = ., family = poisson(), offset = logpred_base_agg)

arm::display(m7)
arm::display(m8)




