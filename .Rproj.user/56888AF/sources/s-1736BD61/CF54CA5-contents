
ring_qc <- function(ring, azimuth, max_na = 0.2, max_consecutive_na = 30) {
  nas <- is.na(ring)
  n_nas <- sum(nas)
  N <- length(ring)

  if (n_nas/N > max_na) {
    return(rep(NA, N))
  }

  # Assumes (quasi) regular grid
  max_consecutive_na <- max_consecutive_na*length(azimuth)/360

  rle_nas <- rle(is.na(ring))
  max_rle_nas <- suppressWarnings(max(rle_nas$lengths[rle_nas$values == TRUE]))

  if (max_rle_nas >= max_consecutive_na) {
    return(rep(NA, N))
  }

  return(ring)
}
