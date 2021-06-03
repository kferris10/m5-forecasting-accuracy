

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
