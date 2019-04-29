#' Radar beam propagation
#'
#' Calculates the propagation of the radar beam using the 4R/3 approximation.
#'
#' @param range vector with the distance to the radar en meters.
#' @param elevation vector of the same length as `range` containing elevation
#' angles in degrees.
#' @param R radius of Earth in meters.
#' @param Rp aproximation used.
#'
#' @return
#' A data frame conteing 3 variables:
#' \describe{
#' \item{ht}{height above the radar in meters.}
#' \item{rh}{horizontal range in meters.}
#' \item{lea}{local elevation angle in degrees.}
#' }
#'
#' @export


beam_propagation <- function(range, elevation, R = 6371000, Rp = 4*R/3) {

  #Calcula la altura teniendo en cuenta la apróximación
  ht <- sqrt(range^2 + Rp^2 + 2*range*Rp*sin(pi*elevation/180)) - Rp
  #Calcula el rango horizontal
  rh <- range*cos(pi*elevation/180)
  #Calcula el angulo de elevación efectivo teniendo en cuenta la aproximación
  lea <- pi*elevation/180 + atan((range*cos(pi*elevation/180))/(range*sin(pi*elevation/180) + Rp))

  return(data.frame(ht = ht,
              rh = rh,
              lea = lea))
}
