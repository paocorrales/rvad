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
