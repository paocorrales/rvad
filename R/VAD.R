#' Calculates the horizontal of every range and and elevation using radial wind.
#'
#' From the radial wind measured by a Doppler radar it calculate the horizontal
#' components of the wind using the Velocity Azimuth Display from Browning and
#' Wexler (1968).
#'
#' @param vr a vector containing the radial wind.
#' @param azimuth a vector of length = length(vr) containing the azimuthal angle
#' of every vr observation.
#' @param range a vector of length = length(vr) containing the range (in meters)
#' asociate to the observation.
#' @param elev_ang a vector of length = length(vr) with the elevation angle of
#' every observation.
#' @param max_na maximum percentage of missing data in a single ring (defined as
#' the date in every range and elevation angle).
#' @param max_consecutive_na maximun angular gap for a single ring.
#' @param r2_min minimum r squared permitted in each fit.
#'
#' @return
#' A data frame with class `rvad_vad` containing 7 variables:
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
#' The algorithm can work with sigle volumens of data scanned in PPI (Plan Position
#' Indicator) mode. The radial wind must not have aliasing. Removing the noise
#' and other artifacts is desirable.
#'
#' `vad_fit()` take vectors of the same length with the radial wind, azimuth angle,
#' range and elevation angle and compute a sinusoidal fit for each ring of data
#' (the observation for a particular range and elevation) before doing a simple
#' quality control.
#'
#' First, it check if the amount of missing data (must be explicit on the data
#' frame) is greater than `max_na`, by default a ring with more than 20% of missing
#' data is descarted. Second, reject any ring with a gap greater than
#' `max_consecutive_na`. Following Matejka y Srivastava (1991) the argument is
#' set as an 30 degree angle. After the fit the algorithm check the r squared
#' and reject all the rings with `r2` less than a threshol. It is recommended
#' to define this threshold after exploring the result with `r2 = 1`.
#'
#' The function returns a data frame with the stimated u and v for each ring or
#' a `NA` if the ring was rejected. It has also some quantities of quality of
#' the fit.
#'
#' @seealso [vad_regrid()] to sample the result into a regular grid.
#'
#' @example
#'
#'
#' @export
#' @import data.table
vad_fit <- function(vr, azimuth, range, elev_ang,
                    max_na = 0.2, max_consecutive_na = 30,
                    r2_min = 0.8) {
  vol <- data.table::data.table(vr = vr, azimuth = azimuth, range = range, elev_ang = elev_ang)

  vol[, vr_qc := ring_qc(vr, azimuth,
                         max_na = max_na,
                         max_consecutive_na = max_consecutive_na),
      by = .(range, elev_ang)]

  vad <- vol[, ring_fit(vr_qc, azimuth, elev_ang),
             by = .(range, elev_ang)]

  vad[, ht := beam_propagation(vad$range, elev_ang = vad$elev_ang)$ht]

  # 6. Control de calidad sobre el fit
  #   - r2 mayor a un valor
  #   - r2 no NA
  vad <- vad[!fit_qc(vad$r2, r2_min = r2_min),
             c("u", "v", "r2", "rmse") := NA]
  vad <- vad[, .(height = ht, u = u, v = v, range, elevation = elev_ang, r2, rmse)]

  data.table::setDF(vad)
  class(vad) <- c("rvad_vad", class(vad))
  attr(vad, "rvad_raw") <- TRUE
  return(vad)
}


plot.rvad_vad <- function(x, y, ...) {
  x <- x[complete.cases(x), ]


  ggplot2::ggplot(x, aes(height, sqrt(u^2 + v^2))) +
    ggplot2::geom_point(aes(color = factor(elev_ang), size = r2))
}
