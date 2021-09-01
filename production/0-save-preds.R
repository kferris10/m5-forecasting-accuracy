
library(arm)
library(tidyverse)
options(stringsAsFactors = F, digits = 3)

load("predictions/preds-global.RData")
load("predictions/preds-time-all.RData")
load("predictions/preds-month-all.RData")
load("predictions/preds-weekday-all.RData")
load("predictions/preds-snap-all.RData")

# generating predictions ------------------------------------------------------

# setting up the baseline predictions in wide format for each row
preds_global_wide <- preds_global %>% 
  pivot_wider(names_from = day, 
              values_from = c(lp_non0_base, lp_sales_base)) %>% 
  slice(rep(1, nrow(preds_time))) %>% 
  bind_cols(select(preds_time, ends_with("id")), .)


# generating overal predictions
preds <- bind_cols(
  select(preds_global_wide, ends_with("id")), 
  select(preds_global_wide, -ends_with("id")) + 
    select(preds_time, -ends_with("id")) + 
    select(preds_month, -ends_with("id")) + 
    select(preds_weekday, -ends_with("id")) + 
    select(preds_snap, -ends_with("id"))
) %>% 
  rename_with(str_replace_all, pattern = "_base", replacement = "")

# predictions for the initial training dataset
preds_validation <- preds %>% 
  select(id, lp_non0_1914:lp_non0_1941, lp_sales_1914:lp_sales_1941) %>% 
  mutate(
    F1 = invlogit(lp_non0_1914) * exp(lp_sales_1914), 
    F2 = invlogit(lp_non0_1915) * exp(lp_sales_1915), 
    F3 = invlogit(lp_non0_1916) * exp(lp_sales_1916), 
    F4 = invlogit(lp_non0_1917) * exp(lp_sales_1917), 
    F5 = invlogit(lp_non0_1918) * exp(lp_sales_1918), 
    F6 = invlogit(lp_non0_1919) * exp(lp_sales_1919), 
    F7 = invlogit(lp_non0_1920) * exp(lp_sales_1920), 
    F8 = invlogit(lp_non0_1921) * exp(lp_sales_1921), 
    F9 = invlogit(lp_non0_1922) * exp(lp_sales_1922), 
    F10 = invlogit(lp_non0_1923) * exp(lp_sales_1923), 
    F11 = invlogit(lp_non0_1924) * exp(lp_sales_1924), 
    F12 = invlogit(lp_non0_1925) * exp(lp_sales_1925), 
    F13 = invlogit(lp_non0_1926) * exp(lp_sales_1926), 
    F14 = invlogit(lp_non0_1927) * exp(lp_sales_1927), 
    F15 = invlogit(lp_non0_1928) * exp(lp_sales_1928), 
    F16 = invlogit(lp_non0_1929) * exp(lp_sales_1929), 
    F17 = invlogit(lp_non0_1930) * exp(lp_sales_1930), 
    F18 = invlogit(lp_non0_1931) * exp(lp_sales_1931), 
    F19 = invlogit(lp_non0_1932) * exp(lp_sales_1932), 
    F20 = invlogit(lp_non0_1933) * exp(lp_sales_1933), 
    F21 = invlogit(lp_non0_1934) * exp(lp_sales_1934), 
    F22 = invlogit(lp_non0_1935) * exp(lp_sales_1935), 
    F23 = invlogit(lp_non0_1936) * exp(lp_sales_1936), 
    F24 = invlogit(lp_non0_1937) * exp(lp_sales_1937), 
    F25 = invlogit(lp_non0_1938) * exp(lp_sales_1938), 
    F26 = invlogit(lp_non0_1939) * exp(lp_sales_1939),  
    F27 = invlogit(lp_non0_1940) * exp(lp_sales_1940), 
    F28 = invlogit(lp_non0_1941) * exp(lp_sales_1941)
  ) %>% 
  select(id, F1:F28) %>% 
  # need to be careful here - shouldn't be doing this with NAs
  # but HOUSEHOLD_2_162 is annohing
  mutate(across(F1:F28, ~ coalesce(., 0))) %>% 
  mutate(across(where(is.numeric), round, digits = 1)) %>% 
  mutate(id = str_replace_all(id, "evaluation", "validation"))

# predictions for the final dataset
preds_evaluation <- preds %>% 
  select(id, lp_non0_1942:lp_non0_1969, lp_sales_1942:lp_sales_1969) %>% 
  mutate(
    F1 = invlogit(lp_non0_1942) * exp(lp_sales_1942), 
    F2 = invlogit(lp_non0_1943) * exp(lp_sales_1943), 
    F3 = invlogit(lp_non0_1944) * exp(lp_sales_1944), 
    F4 = invlogit(lp_non0_1945) * exp(lp_sales_1945), 
    F5 = invlogit(lp_non0_1946) * exp(lp_sales_1946), 
    F6 = invlogit(lp_non0_1947) * exp(lp_sales_1947), 
    F7 = invlogit(lp_non0_1948) * exp(lp_sales_1948), 
    F8 = invlogit(lp_non0_1949) * exp(lp_sales_1949), 
    F9 = invlogit(lp_non0_1950) * exp(lp_sales_1950), 
    F10 = invlogit(lp_non0_1951) * exp(lp_sales_1951), 
    F11 = invlogit(lp_non0_1952) * exp(lp_sales_1952), 
    F12 = invlogit(lp_non0_1953) * exp(lp_sales_1953), 
    F13 = invlogit(lp_non0_1954) * exp(lp_sales_1954), 
    F14 = invlogit(lp_non0_1955) * exp(lp_sales_1955), 
    F15 = invlogit(lp_non0_1956) * exp(lp_sales_1956), 
    F16 = invlogit(lp_non0_1957) * exp(lp_sales_1957), 
    F17 = invlogit(lp_non0_1958) * exp(lp_sales_1958), 
    F18 = invlogit(lp_non0_1959) * exp(lp_sales_1959), 
    F19 = invlogit(lp_non0_1960) * exp(lp_sales_1960), 
    F20 = invlogit(lp_non0_1961) * exp(lp_sales_1961), 
    F21 = invlogit(lp_non0_1962) * exp(lp_sales_1962), 
    F22 = invlogit(lp_non0_1963) * exp(lp_sales_1963), 
    F23 = invlogit(lp_non0_1964) * exp(lp_sales_1964), 
    F24 = invlogit(lp_non0_1965) * exp(lp_sales_1965), 
    F25 = invlogit(lp_non0_1966) * exp(lp_sales_1966), 
    F26 = invlogit(lp_non0_1967) * exp(lp_sales_1967), 
    F27 = invlogit(lp_non0_1968) * exp(lp_sales_1968), 
    F28 = invlogit(lp_non0_1969) * exp(lp_sales_1969)
  ) %>% 
  select(id, F1:F28) %>% 
  # need to be careful here - shouldn't be doing this with NAs
  # but HOUSEHOLD_2_162 is annohing
  mutate(across(F1:F28, ~ coalesce(., 0.1))) %>% 
  mutate(across(where(is.numeric), round, digits = 4))

preds_submission <- bind_rows(preds_validation, preds_evaluation)

# saving -----------------------------------------------------------------------

save(preds, file = "predictions/preds-all.RData")
save(preds_submission, file = paste0("predictions/preds-submission-", Sys.Date(), ".RData"))
write.csv(preds_submission, file = "predictions/preds-submission.csv", quote = F, row.names = F)
