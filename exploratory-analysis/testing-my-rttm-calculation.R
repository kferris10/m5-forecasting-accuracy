
# notes ------------------------------------------------

# me doing RTTM by hand doesn't get quite the same estimates as glmer
# but it's pretty close

library(Hmisc)
library(lme4)

train_0_base <- train_0 %>% 
  group_by(store_id, item_id) %>% 
  summarise(n = n(), sales_0 = sum(sales == 0), .groups = "drop")

m0 <- glm(cbind(sales_0, n - sales_0) ~ 1, 
          data = train_0_base, family = binomial())
train_0_base$lpred <- predict(m0, newdata = train_0_base)


m <- glm(cbind(sales_0, n - sales_0) ~ 0 + store_id, 
         data = train_0_base, family = binomial(), offset = lpred)

m_glmer <- glmer(cbind(sales_0, n - sales_0) ~ 1 + (1|store_id), 
                 data = train_0_base, family = binomial())
arm::display(m_glmer)

coef(m)
se.coef(m)
bt_se <- sqrt(wtd.var(coef(m), 1 / se.coef(m)^2))

# results are within 0.1 z-scores of each other
regressed <- (coef(m) / se.coef(m)^2 + 0 / bt_se^2) / (1 / se.coef(m)^2 + 1 / bt_se^2)
round(ranef(m_glmer)$store_id[[1]] / bt_se, 1)
round(regressed / bt_se, 1)
