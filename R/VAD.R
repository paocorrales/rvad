#' Velocity Azimuth Display
#'
#' Approximates the horizontal components of the wind from radial wind measured
#' by Doppler radar using the Velocity Azimuth Display method from Browning and
#' Wexler (1968).
#'
#' @param vr a vector containing the radial wind.
#' @param azimuth a vector of length = length(vr) containing the azimuthal angle
#' of every vr observation in degrees clockwise from 12 o' clock.
#' @param range a vector of length = length(vr) containing the range (in meters)
#' asociate to the observation.
#' @param elevation a vector of length = length(vr) with the elevation angle of
#' every observation in degrees.
#' @param max_na maximum percentage of missing data in a single ring (defined as
#' the date in every range and elevation angle).
#' @param max_consecutive_na maximun angular gap for a single ring.
#' @param r2_min minimum r squared permitted in each fit.
#'
#' @return
#' A data frame with class `rvad_vad` that has a [plot()] method and contains
#' 7 variables:
#' \describe{
#' \item{height}{height above the radar in meters.}
#' \item{u}{zonal wind in m/s.}
#' \item{v}{meridional wind in m/s.}
#' \item{range}{distance to the radar in meters.}
#' \item{elevation}{elevation angle in degrees.}
#' \item{r2}{r squared of the fit.}
#' \item{rmse}{root mean squeared error calculated as the standar deviation of
#' the residuals.}
#' }
#'
#' @details
#' The algorithm can work with sigle volume of data scanned in PPI (Plan Position
#' Indicator) mode. The radial wind must not have aliasing. Removing the noise
#' and other artifacts is desirable.
#'
#' `vad_fit()` takes vectors of the same length with radial wind, azimuth angle,
#' range and elevation angle and computes a sinusoidal fit for each ring of data
#' (the observation for a particular range and elevation) before doing a simple
#' quality control.
#'
#' First, it checks if the amount of missing data (must be explicit on the data
#' frame) is greater than `max_na`, by default a ring with more than 20% of missing
#' data is descarted. Second, rejects any ring with a gap greater than
#' `max_consecutive_na`. Following Matejka y Srivastava (1991) the default is
#' set as 30 degrees. After the fit, the algorithm rejects rings whose fit has
#' a `r2` less than `r2_min`. It is recommended to define this threshold
#' after exploring the result with `r2_min = 0`.
#'
#' Rings that fail any of the above-mentioned checks return `NA`.
#'
#' @seealso [vad_regrid()] to sample the result into a regular grid.
#'
#' @examples
#' VAD <- with(radial_wind, vad_fit(vr, azimuth, range, elevation))
#' plot(VAD)
#'
#' @export
#' @import data.table
vad_fit <- function(vr, azimuth, range, elevation,
                    max_na = 0.2, max_consecutive_na = 30,
                    r2_min = 0.8) {
  vol <- data.table::data.table(vr = vr, azimuth = azimuth, range = range, elev_ang = elevation)

  vol[, vr_qc := ring_qc(vr, azimuth,
                         max_na = max_na,
                         max_consecutive_na = max_consecutive_na),
      by = .(range, elevation)]

  vad <- vol[, ring_fit(vr_qc, azimuth, elevation),
             by = .(range, elevation)]

  vad[, ht := beam_propagation(vad$range, elev_ang = vad$elev_ang)$ht]

  # 6. Control de calidad sobre el fit
  #   - r2 mayor a un valor
  #   - r2 no NA
  vad <- vad[!fit_qc(vad$r2, r2_min = r2_min),
             c("u", "v", "r2", "rmse") := NA]
  vad <- vad[, .(height = ht, u = u, v = v, range, elevation = elevation, r2, rmse)]

  data.table::setDF(vad)
  class(vad) <- c("rvad_vad", class(vad))
  attr(vad, "rvad_raw") <- TRUE
  return(vad)
}


plot.rvad_vad <- function(x, y, ...) {
  if(!requireNamespace("ggplot2", quietly = TRUE)) {
    stop('ggplot2 package needed. You can install it with `install.packages("ggplot2")')
  }

  if (isTRUE(attr(x, "rvad_raw"))) {
    x <- x[stats::complete.cases(x), ]
    x$elevation <- factor(x$elevation)
    ggplot2::ggplot(x, ggplot2::aes(sqrt(u^2 + v^2), height)) +
      ggplot2::geom_point(ggplot2::aes(color = elevation))
  } else {
    x <- x[stats::complete.cases(x), ]
    x$V <- sqrt(x$u^2 + x$v^2)
    x$dV <- error_prop(x$u, x$v, x$u_sd, x$v_sd)

    ggplot2::ggplot(x, ggplot2::aes(height, V)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = V - 2*dV, ymax = V + 2*dV),
                           alpha = 0.2) +
      ggplot2::coord_flip()
  }



}


error_prop <- function(u, v, du, dv) {

  1/sqrt(u^2 + v^2)*sqrt((u*du)^2 + (v*dv)^2)

}
