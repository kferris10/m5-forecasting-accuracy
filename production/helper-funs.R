

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

#' Extract SEs from a glm model
#' from https://stats.stackexchange.com/questions/332077/glm-standard-errors
extract_se <- function(glm_fit) {
  p1 <- 1L:glm_fit$rank
  Qr <- glm_fit$qr
  coef.p <- glm_fit$coefficients[Qr$pivot[p1]]
  covmat.unscaled <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
  dimnames(covmat.unscaled) <- list(names(coef.p), names(coef.p))
  covmat <- 1 * covmat.unscaled
  var.cf <- diag(covmat)
  sqrt(var.cf)
}

#' helper function to prep raw data for time models
#' note that this requires several objects loaded in the global environment
prep_time_data <- function(data, 
                           off_data = NULL, 
                           off_data_non0_prefix = NULL, 
                           off_data_sales_prefix = NULL) {
  gc()
  df <- data %>% 
    pivot_longer(starts_with("d_"), 
                 names_to = "day", 
                 names_prefix = "d_", 
                 names_transform = list(day = as.numeric), 
                 values_to = "sales", 
                 values_drop_na = TRUE) %>% 
    left_join(pp_global, by = "day") %>% 
    left_join(preds_global, by = "day") %>% 
    left_join(cal, by = "day")
  
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

calc_glm_coefs <- function(data, f, is_pois, offset, pb = NULL) {
  if(!is.null(pb)) pb$tick()
  
  data <- data %>% mutate(off = {{ offset }})
  if(is_pois) {
    fam <- poisson()
    data <- data %>% filter(sales > 0)
  } else {
    fam <- binomial()
    data <- data %>% filter(between(invlogit(off), .001, .999))
  }
  
  gc()
  X <- model.matrix(delete.response(terms(f)), data)
  y <- model.response(model.frame(f, data))
  m <- glm.fit(X, y, offset = data$off, family = fam)
  coefs <- tibble(term = colnames(X), est_raw = coef(m), se = extract_se(m))
  
  coefs
}
