

#' regress noisy data using Eq 12.1 from Gelman & Hill
apply_rttm <- function(y, sd_y, sigma, mu = 0) {
  (y / sd_y^2 + mu / sigma^2) / (1 / sd_y^2 + 1 / sigma^2)
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
