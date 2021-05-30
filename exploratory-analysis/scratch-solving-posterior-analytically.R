
# notes -----------------------------------------------------------------------

# proof-of-concept showing that you can use matrix algebra to apply a prior to get regressed coefs
# https://stats.stackexchange.com/questions/28744/multivariate-normal-posterior

# setup ------------------------------------------------------------------------

library(feather)
library(tidyverse)
source("production/helper-funs.R")
options(stringsAsFactors = F, digits = 3, mc.cores = 2)

# model specifications I'll be using
f_time <- formula(~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10)

# loading data
load("predictions/preds-global.RData")
load("predictions/preds-time-store.RData")
cal <- read_csv("data/calendar.csv") %>% 
  mutate(day = as.numeric(str_replace_all(d, "d_", "")))
train_raw <- read_feather("data/data-train-wide.feather")
train_raw %>% select(1:20) %>% glimpse()


i <- "FOODS_2_222"

# comparing results ------------------------------------------------------------

### stan model 
m <- stan_glm(update(f_time, I(sales != 0) ~ .), 
              family = binomial(), 
              data = time_mod_data_i %>% filter(day > 500), 
              offset = lp_non0_base, 
              prior = normal(0, 5), 
              chains = getOption("mc.cores"), 
              iter = 1000)
stan_post <- coef(m)

### manually

# fitting the glm - crazy ass coefficients
m_non0_time_i <- glm(update(f_time, I(sales != 0) ~ .), 
                     time_mod_data_i %>% filter(day > 500), 
                     family = binomial(), 
                     offset = lp_non0_base)

# extracting model results and setting prior
x <- coef(m_non0_time_i)
V <- vcov(m_non0_time_i)
n <- length(x)
mu0 <- rep(0, n)
V0 <- diag(5^2, nrow = n, ncol = n)

# applying RTTM
manual_post <- V0 %*% solve(V0 + 1 / n * V) %*% x + 
  1 / n * V %*% solve(V0 + 1 / n * V) %*% mu0
V_post <- V0 %*% solve(V0 + 1 / n * V) %*% V / n

### they're basically the same!
cor(stan_post, manual_post)
cor(se(m), sqrt(diag(V_post)))
