robustness_threshold <- function(mod, which = 2) {
  ## Gangl2013 -- Partial Identification and Sensitivity Analysis, Figure 18.2
  
  rho_xz <- seq(-1,1, by = 0.01)
  
  ## bounds of rho_yx
  rho_yz_plus <- function(rho_xz) {
    rho_yx * rho_xz + sqrt((1 - rho_yx ^ 2) * (1 - rho_xz ^ 2))
  }
  
  rho_yz_minus <- function(rho_xz) {
    rho_yx * rho_xz - sqrt((1 - rho_yx ^ 2) * (1 - rho_xz ^ 2))
  }
  
  ## robustness thresholds
  rho_yz_threshold <- function(rho_xz) {
    num <- (1 - rho_xz ^ 2) * beta - sd_beta * t.critical
    den <- rho_xz * (1 - rho_xz ^ 2 + sd_beta * t.critical / Rsq)
    num / den
  }
  
  get_pcor <- function(y, x, Z, addIntercept = TRUE) {
    ## y and x are vectors
    ## Z is matrix, not data.frame
    if (addIntercept) {
      Z <- cbind(1, Z)
    }
    cor(residuals(lm.fit(Z, y)), residuals(lm.fit(Z, x)))
  }
  
  rho_yx <-
    get_pcor(
      y = model.response(model.frame(mod)), x = model.matrix(mod)[,which], model.matrix(mod)[,-which, drop =
                                                                                               FALSE], addIntercept = FALSE
    )
  
  beta <- coef(mod)[which]
  sd_beta <- sqrt(diag(vcov(mod)))[which]
  t.critical <- 1.96
  Rsq <- cor(mod$fitted.values, model.response(model.frame(mod))) ^ 2
  
  plot(
    rho_xz, rho_yz_plus(rho_xz), type = "l",
    xlim = c(-1.1,1.1), ylim = c(-1.1, 1.1),
    xlab = "rho(x,z)", ylab = "rho(y,z)"
  )
  lines(rho_xz, rho_yz_minus(rho_xz))
  lines(rho_xz, rho_yz_threshold(rho_xz), col = "red")
  ## add reference lines
  abline(v = seq(-1,1,by = 0.1), lty = 2, col = "grey")
  abline(h = seq(-1,1,by = 0.1), lty = 2, col = "grey")
}

## example
data(Duncan, package = "car")
mod <- lm(prestige ~ income + education, dat = Duncan)
robustness_threshold(mod)
