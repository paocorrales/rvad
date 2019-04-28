#' Calculates the horizontal of every range and and elevation using radial wind.
#'
#' The radial wind must not have aliasing. Also, noise and other artifacts are not good for the algorithm.
#' Te funcction can work with sigle volumens of data scanned in PPI (Plan Position Indicator) mode.
#'
#' @param vr a vector containing the viento radial
#' @param azimuth a vector of length = length(vr) containing the azimuthal angle of every
#' vr observation.
#' @param range a vector of length = length(vr) containing the range (in meters) asociate
#' to the observation.
#' @param elev_ang a vector of length = length(vr) with the elevation angle of every observation.
#' @param max_na maximum percentage of missing data in a single ring (defined as the date in every
#' range and elevation angle).
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
#' \item{rmse}{root mean squeared error calculated as the standar deviation of the residuals.}
#' }
#'
#' @import data.table
VAD <- function(vr, azimuth, range, elev_ang,
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
  vad <- vad[, .(range, elev_ang, height = ht, u = u, v = v, r2, rmse)]

  data.table::setDF(vad)
  class(vad) <- c("rvad_vad", class(vad))
  return(vad)
}


# plot.rvad_vad <- function(x, y, ...) {
#   x <- x[complete.cases(x), ]
#
#   ggplot2::ggplot(x, aes(height, speed)) +
#     ggplot2::geom_point(aes(color = factor(elev_ang)))
# }
