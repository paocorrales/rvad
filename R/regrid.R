#' Sample VAD to a regular grid
#'
#' @param vad `rvad_vad` object returned by [vad_fit()].
#' @param resolution vertical resolution in meters.
#' @param bandwidth
#' @param ht.out
#'
#' @return
#' A data frame with class `rvad_vad` that has a [plot()] method and contains
#' 7 variables:
#' \describe{
#' \item{height}{height above the radar in meters.}
#' \item{u}{zonal wind in m/s.}
#' \item{v}{meridional wind in m/s.}
#' \item{u_sd}{standar deviation of u in m/s.}
#' \item{v_sd}{standar deviation of v in m/s.}
#' }
#'
#' @description
#'
#' @export
vad_regrid <- function(vad,
                       resolution,
                       bandwidth = resolution,
                       ht.out = NULL) {
  vad <- vad[stats::complete.cases(vad), ]

  if (is.null(ht.out)) {
    if (resolution > bandwidth) {
      stop("resolution must be smaller or equal than bandwitdh")
    }
    half_resolution <- resolution/2
    ht.out <- seq(min(vad$height) + half_resolution,
                  max(vad$height) - half_resolution,
                  by = resolution)
  }

  if (missing(bandwidth)) {
    stop('argument "bandwidth" is missing, with no default')
  }

  grid <- .loess(vad$u, vad$v, vad$height, ht.out, bandwidth, vad$rmse, vad$range)
  vad_grid <- data.frame(grid)
  class(vad_grid) <- c("rvad_vad", class(vad_grid))
  attr(vad_grid, "rvad_raw") <- FALSE
  return(vad_grid)
}


.loess <- function(u, v, ht, ht.out, bandwidth, rmse, range) {
  v_sd <- u_sd <- v_grid <- u_grid <- rep(0, length = length(ht.out))
  half_bandwidth <- bandwidth/2
  # browser()
  for (i in seq_along(ht.out)) {
    sub <- ht >= (ht.out[i] - half_bandwidth) & ht <= (ht.out[i] + half_bandwidth)
    weight_ht <- (1 - abs(ht[sub] - ht.out[i])^3/half_bandwidth^3)^3
    # weight_ht <- weight_ht/max(weight_ht)
    weight_rmse <- 1/(rmse[sub]) / max(1/rmse[sub])
    weight_range <- 1/(range[sub]^2) / max(1/range[sub]^2)
    weight <- weight_rmse * weight_range * weight_ht
    weight <- weight/sum(weight)

    fit <- stats::lm.wfit(x = cbind(1, ht[sub]),
                          y = cbind(u[sub], v[sub]),
                          w = weight)
    u_grid[i] <- fit$coefficients[1, 1] + fit$coefficients[2, 1]*ht.out[i]
    v_grid[i] <- fit$coefficients[1, 2] + fit$coefficients[2, 2]*ht.out[i]

    u_sd[i] <- stats::sd(fit$residuals[, 1])*sqrt(1/length(weight) + (ht.out[i] - mean(ht[sub]))^2/sum((ht[sub] - mean(ht[sub]))^2))
    v_sd[i] <- stats::sd(fit$residuals[, 2])*sqrt(1/length(weight) + (ht.out[i] - mean(ht[sub]))^2/sum((ht[sub] - mean(ht[sub]))^2))
  }

  return(list(height = ht.out,
              u = u_grid,
              v = v_grid,
              u_sd = u_sd,
              v_sd = v_sd
  ))
}
