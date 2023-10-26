require(dplyr)
require(ggplot2)


distance_moved <- function(mat, maxd, scale = TRUE, eigenvals = NULL) {
  if (scale) {
    mat <- mat[, 1:maxd] %*% diag(1 / sqrt(eigenvals[1:maxd]))
  }
  N <- nrow(mat)
  V1 <- mat[1:(N / 2), ]
  V2 <- mat[(N / 2 + 1):N, ]
  distances <- rowSums((V1[, 1:maxd] - V2[, 1:maxd]) ^ 2)
  distances_grid <- cbind(Observations = 1:(N/2), Distances = distances) %>%
    as_tibble()
  return(distances_grid)
}

group_distances <- function(distances) {
  distances_grid <- distances %>%
    as_tibble() %>%
    group_by(Group, Components) %>%
    summarise(Distances = mean(Distances))
  return(distances_grid)
}

plot_distances <- function(distances) {
  distances <- distances %>%
    mutate(
      Observations = as.factor(Observations),
      Components = as.factor(Components))
  plot <- ggplot(distances,
      aes(x = Components, y = Observations)) + # nolint: object_usage_linter.
    geom_tile(aes(fill = Distances)) + # nolint: object_usage_linter.
    scale_fill_distiller(direction = 1)
  return(plot)
}

plot_group_distances <- function(distance_grid, d) {
  plot <- ggplot(distance_grid,
      aes(x = Components, y = Group)) + # nolint: object_usage_linter.
    geom_tile(aes(fill = Distances)) + # nolint: object_usage_linter.
    scale_fill_distiller(direction = 1)
  return(plot)
}