
ring_fit <- function(ring, azimuth, elev) {
  nas <- is.na(ring)
  if (sum(nas) == length(ring)) {
    return(list(spd  = NA_real_,
                dir  = NA_real_,
                r2   = NA_real_,
                rmse = NA_real_))
  }
  fit <- .lm.fit(cbind(1, cos(azimuth*pi/180), sin(azimuth*pi/180))[!nas, , drop = FALSE],
                 ring[!nas])
  rmse <- sd(fit$residuals)
  r2 <- 1 - var(fit$residuals)/var(ring[!nas])
  a <- fit$coefficients[2]
  b <- fit$coefficients[3]

  spd <- sqrt(a^2 + b^2)/cos(elev*pi/180)

  if (b < 0) {
    dir <- pi/2 - atan(a/b)*180/pi
  } else {
    dir <- 3*pi/2 - atan(a/b)*180/pi
  }
  return(list(spd  = spd,
              dir  = dir,
              r2   = r2,
              rmse = rmse))
}
