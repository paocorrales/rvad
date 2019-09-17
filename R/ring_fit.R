ring_fit <- function(ring, azimuth, elev, outlier_threshold = Inf) {

  nas <- is.na(ring)
  if (sum(nas) == length(ring)) {
    return(list(u  = NA_real_,
                v  = NA_real_,
                r2   = NA_real_,
                rmse = NA_real_))
  }

  n_outliers <- 1
  while(n_outliers > 0) {
    fit <- stats::.lm.fit(cbind(1, cos(azimuth*pi/180), sin(azimuth*pi/180))[!nas, , drop = FALSE],
                          ring[!nas])
    rmse <- stats::sd(fit$residuals)

    outliers <- abs(fit$residuals) >= outlier_threshold*rmse
    n_outliers <- sum(outliers)
    # n_outliers
    ring[!nas][outliers] <- NA
    nas <- is.na(ring)
  }


  r2 <- 1 - stats::var(fit$residuals)/stats::var(ring[!nas])
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


.global_variables <- c(".", "V", "dV", "elevation", "height",
                       "ht", "r2", "rmse", "u", "v", "var", "vr_qc")
if(getRversion() >= "2.15.1")  utils::globalVariables(.global_variables)
