#' @import data.table
VAD <- function(vr, azimuth, range, elev_ang,
                max_na = 0.2, max_consecutive_na = 30,
                r2_min = 0.8) {
  # browser()
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
             c("spd", "dir", "r2", "rmse") := NA]
  vad[, .(range, elev_ang, height = ht, speed = spd, direction = dir, r2, rmse)]

  return(as.list(vad))
}


# Parametros

field  = 'Vda'    #Nombre de la variable en el archivo de datos
angmin = 2        #Ángulo de elevación mínimo expresado como indice y empezando en 0
angmax = 7        #Ángulo de elevación máximo expresado como indice
rint   = 0.3      #Radio interior de la arandala a calcular en Km
rext   = 40.0     #Radio exterior de la arandela a calcular
maxgap = 30       #Máximo gap sin datos permitido, en grados
maxNaN = 72       #Cantidad de de datos faltantes en un anillo
rmin   = 0.8      #R cuadrado mínimo para que el fit del anillo sea válido
lpf    = 'False'  #Usa o no filtro pasa bajo
N      = 12       #Cantidad de datos a usar en el filtro, tiene que ser par!
