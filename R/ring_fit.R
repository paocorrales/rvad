
ring_fit <- function(ring, azimuth, elev) {
  nas <- is.na(ring)
  if (sum(nas) == length(ring)) {
    return(list(u  = NA_real_,
                v  = NA_real_,
                r2   = NA_real_,
                rmse = NA_real_))
  }
  fit <- .lm.fit(cbind(1, cos(azimuth*pi/180), sin(azimuth*pi/180))[!nas, , drop = FALSE],
                 ring[!nas])

  rmse <- sd(fit$residuals)
  r2 <- 1 - var(fit$residuals)/var(ring[!nas])
  # if (r2 > 0.8) browser()
  coef_cos <- fit$coefficients[2]
  coef_sin <- fit$coefficients[3]

  v <- coef_cos/cos(elev*pi/180)
  u <- coef_sin/cos(elev*pi/180)

  return(list(u = u,
              v = v,
              r2   = r2,
              rmse = rmse))
}


