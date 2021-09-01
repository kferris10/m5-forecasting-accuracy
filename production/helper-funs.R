

#' regress noisy data using Eq 12.1 from Gelman & Hill
apply_rttm <- function(y, sd_y, sigma, mu = 0) {
  (y / sd_y^2 + mu / sigma^2) / (1 / sd_y^2 + 1 / sigma^2)
}

#' posterior calculation for multivariate normal coefficients
#' from https://stats.stackexchange.com/questions/28744/multivariate-normal-posterior
#' something isn't quite right here, but it's close enough
apply_rttm_mvn <- function(mod, mu, V) {
  # extracting the results from the model
  y <- coef(mod)
  V_y <- vcov(mod)
  n <- length(y)
  
  # analytical posterior calculation
  mu_post <- V %*% solve(V + V_y) %*% y + 
    V_y %*% solve(V + V_y) %*% mu
  V_post <- V %*% solve(V + V_y) %*% (V_y)
  # returning the results
  results <- tibble(
    term = names(coef(mod)), 
    est_raw = y, 
    se_raw = sqrt(diag(V_y)), 
    estimate = as.numeric(mu_post), 
    se = sqrt(diag(V_post))
  )
  results
}

#' helper function to prep raw data for time models
#' note that this requires several objects loaded in the global environment
prep_time_data <- function(data, 
                           off_data = NULL, 
                           off_data_non0_prefix = NULL, 
                           off_data_sales_prefix = NULL) {
  
  df <- data %>% 
    pivot_longer(starts_with("d_"), 
                 names_to = "day", 
                 names_prefix = "d_", 
                 names_transform = list(day = as.numeric), 
                 values_to = "sales", 
                 values_drop_na = TRUE) %>% 
    left_join(preds_global, by = "day") %>% 
    left_join(cal, by = "day") %>% 
    mutate(is_snap = case_when(store_id %in% c("CA_1", "CA_2", "CA_3", "CA_4") & snap_CA == 1 ~ 1, 
                               store_id %in% c("TX_1", "TX_2", "TX_3") & snap_TX == 1 ~ 1, 
                               store_id %in% c("WI_1", "WI_2", "WI_3") & snap_WI == 1 ~ 1, 
                               TRUE ~ 0))
  
  if(!is.null(off_data)) {
    off_data_non0_use <- off_data %>%  
      select(ends_with("_id"), starts_with(off_data_non0_prefix)) %>% 
      pivot_longer(cols = starts_with(off_data_non0_prefix), 
                   names_to = "day", 
                   values_to = "extra_offset_non0", 
                   names_transform = list(day = as.integer), 
                   names_prefix = off_data_non0_prefix)
    off_data_sales_use <- off_data %>%  
      select(ends_with("_id"), starts_with(off_data_sales_prefix)) %>% 
      pivot_longer(cols = starts_with(off_data_sales_prefix), 
                   names_to = "day", 
                   values_to = "extra_offset_sales", 
                   names_transform = list(day = as.integer), 
                   names_prefix = off_data_sales_prefix)
    
    df <- df %>% 
      left_join(off_data_non0_use, by = intersect(names(off_data_non0_use), names(df))) %>% 
      left_join(off_data_sales_use, by = intersect(names(off_data_sales_use), names(df))) %>% 
      mutate(lp_non0_base = lp_non0_base + extra_offset_non0, 
             lp_sales_base = lp_sales_base + extra_offset_sales)
  }
  
  df
}
