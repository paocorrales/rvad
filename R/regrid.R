#' Wind profile from VAD
#'
#' Aggregates the result of [vad_fit()] using a modified loess
#' smooth of degree 1 to get a wind profile on a regular (or other
#' user-supplied) grid.
#'
#' @param vad an `rvad_vad` object returned by [vad_fit()].
#' @param layer_width width of the layers in meters (see Details).
#' @param resolution vertical resolution in meters.
#' @param ht_out vector of heights where to evaluate. Overrides `resolution`.
#' @param min_n minimum number of points in each layer.
#'
#' @details
#' The method approximates wind components in a regular grid using weighted local
#' regression at each point in the grid. Unlike [stats::loess()], the layer_width
#' is specified in physical units instead of in ammount of points and thus the
#' value at each gridpoint represents the wind at a layer of thickness
#' `layer_width`. This means that, while the `resolution` parameter determines
#' how many points are used to define the wind profile, the effective resolution
#' is controlled by `layer_width`. Increasing `layer_width` results in more precise
#' estimates (because it's basedon more data points) but reduces the effective
#' resolution.
#'
#' @return
#' A data frame with class `rvad_vad` that has a [plot()] method and contains
#' 7 variables:
#' \describe{
#' \item{height}{height above the radar in meters.}
#' \item{u}{zonal wind in m/s.}
#' \item{v}{meridional wind in m/s.}
#' \item{u_std.error}{standar error of u in m/s.}
#' \item{v_std.error}{standar error of v in m/s.}
#' }
#'
#' @examples
#' VAD <- with(radial_wind, vad_fit(radial_wind, azimuth, range, elevation))
#'
#' # Wind profile with effective resolution of 100
#' plot(vad_regrid(VAD, layer_width = 100, resolution = 100))
#' # The same effective resoution, but sampled at 50m
#' plot(vad_regrid(VAD, layer_width = 100, resolution = 50))
#'
#' # Using too thin layers can cause problems and too many
#' # mising values
#' plot(fine_resolution <- vad_regrid(VAD, layer_width = 10))
#' mean(is.na(fine_resolution$u))
#'
#' @export
vad_regrid <- function(vad,
                       layer_width,
                       resolution = layer_width,
                       ht_out = NULL,
                       min_n = 5) {
  vad <- vad[stats::complete.cases(vad), ]

  if (is.null(ht_out)) {
    if (resolution > layer_width) {
      stop("resolution must be smaller or equal than bandwitdh")
    }
    half_resolution <- resolution/2
    ht_out <- seq(min(vad$height) + half_resolution,
                  max(vad$height) - half_resolution,
                  by = resolution)
  }

  grid <- .loess(vad$u, vad$v, vad$height, ht_out, layer_width,
                 vad$rmse, vad$range, min_n = min_n)
  vad_grid <- data.frame(grid)
  class(vad_grid) <- c("rvad_vad", class(vad_grid))
  attr(vad_grid, "rvad_raw") <- FALSE
  return(vad_grid)
}


.loess <- function(u, v, ht, ht_out, layer_width, rmse, range, min_n) {
  v_sd <- u_sd <- v_grid <- u_grid <- rep(0, length = length(ht_out))
  half_layer_width <- layer_width/2
  # browser()
  for (i in seq_along(ht_out)) {
    sub <- ht >= (ht_out[i] - half_layer_width) & ht <= (ht_out[i] + half_layer_width)
    if (sum(sub) < min_n) {
      u_grid[i] <- NA
      v_grid[i] <- NA
      u_sd[i] <- NA
      v_sd[i] <- NA
    } else{
      # ss <<- sub
      weight_ht <- (1 - abs(ht[sub] - ht_out[i])^3/half_layer_width^3)^3
      weight_rmse <- 1/(rmse[sub]) / max(1/rmse[sub])
      weight_range <- 1/(range[sub]^2) / max(1/range[sub]^2)
      weight <- weight_rmse * weight_range * weight_ht
      weight <- weight/sum(weight)

      fit <- stats::lm.wfit(x = cbind(1, ht[sub]),
                            y = cbind(u[sub], v[sub]),
                            w = weight)
      u_grid[i] <- fit$coefficients[1, 1] + fit$coefficients[2, 1]*ht_out[i]
      v_grid[i] <- fit$coefficients[1, 2] + fit$coefficients[2, 2]*ht_out[i]

      u_sd[i] <- stats::sd(fit$residuals[, 1])*sqrt(1/length(weight) + (ht_out[i] - mean(ht[sub]))^2/sum((ht[sub] - mean(ht[sub]))^2))
      v_sd[i] <- stats::sd(fit$residuals[, 2])*sqrt(1/length(weight) + (ht_out[i] - mean(ht[sub]))^2/sum((ht[sub] - mean(ht[sub]))^2))
    }
  }

  return(list(height = ht_out,
              u = u_grid,
              v = v_grid,
              u_std.error = u_sd,
              v_std.error = v_sd
  ))
}
