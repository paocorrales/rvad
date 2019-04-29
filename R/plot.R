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
