regrid <- function(u, v, ht, range, rmse,
                   width, separation = width, ht.out = NULL) {
  has_data <- !is.na(u)

  u <- u[has_data]
  ht <- ht[has_data]
  range <- range[has_data]
  rmse <- rmse[has_data]


  # todo: chequear que la grilla tenga sentido
  half_separation <- separation/2
  half_width <- width/2
  if (is.null(ht.out)) {
    ht.out <- seq(min(ht) + half_separation,
                  max(ht) - half_separation,
                  by = separation)
  }

  v_sd <- u_sd <- v_grid <- u_grid <- rep(0, length = length(ht.out))

  for (i in seq_along(ht.out)) {
    sub <- ht >= (ht.out[i] - half_width) & ht <= (ht.out[i] + half_width)
    weight_ht <- (1 - abs(ht[sub] - ht.out[i])^3/half_width^3)^3
    # weight_ht <- weight_ht/max(weight_ht)

    weight_rmse <- 1/(rmse[sub]) / max(1/rmse[sub])
    weight_range <- 1/(range[sub]) / max(1/range[sub])

    weight <- weight_rmse + weight_range + weight_ht
    weight <- weight/sum(weight)

    fit <- lm.wfit(x = cbind(1, ht[sub]),
                   y = cbind(u[sub], v[sub]),
                   w = weight)
    u_grid[i] <- fit$coefficients[1, 1] + fit$coefficients[2, 1]*ht.out[i]
    v_grid[i] <- fit$coefficients[1, 2] + fit$coefficients[2, 2]*ht.out[i]

    t_val <- qt(0.05/2, length(weight) - 2, lower.tail = FALSE)
    u_sd[i] <- sd(fit$residuals[, 1])*sqrt(1/length(weight) + (ht.out[i] - mean(ht[sub]))^2/sum((ht[sub] - mean(ht[sub]))^2))
    v_sd[i] <- sd(fit$residuals[, 2])*sqrt(1/length(weight) + (ht.out[i] - mean(ht[sub]))^2/sum((ht[sub] - mean(ht[sub]))^2))
  }

  vad_grid <- data.frame(height = ht.out,
                         u  = u_grid,
                         v  = v_grid,
                         u_se = u_sd,
                         v_sd = v_sd)

}

rescale_1 <- function(x) {
  (x - min(x))/(diff(range(x)))
}
