require(dplyr)
require(ggplot2)


distance_moved <- function(V, d, l = 2, scale = TRUE, eigenvals = NULL) {
  if (scale) {
    V[,1:d] <- as.matrix(V[,1:d]) %*% # nolint
    diag(1 / sqrt(as.numeric(eigenvals[1:d])), nrow = d)
  }
  V <- V %>% as_tibble()
  n <- nrow(V)
  V1 <- V[1:(n / 2), 1:d]
  V2 <- V[(n / 2 + 1):n, 1:d]

  moved <- sapply(1:(n/2), FUN = function(i){
    dist(rbind(V1[i, ], V2[i, ]), method = "minkowski", p = l)
  })
  return(moved)
}

group_distances <- function(distances, d, groups) {
  distances_grid <- expand.grid(
    Components = 1:d,
    Observations = 1:nrow(distances))
  distances_grid$Group <- as.factor(rep(groups, each = 12))
  distances_grid$Distances <- c(t(distances))
  distances_grid <- distances_grid %>%
    as_tibble() %>%
    group_by(Group, Components) %>%
    summarise(Distances = mean(Distances))
  return(distances_grid)
}

plot_distances <- function(distances, d) {
  distance_plot <- expand.grid(
    Components = 1:d,
    Observations = 1:nrow(distances))
  distance_plot$Distances <- c(t(distances))
  distance_plot <- distance_plot %>% as_tibble()
  plot <- ggplot(distance_plot, aes(x = Components, y = Observations)) +
    geom_tile(aes(fill = Distances)) +
    scale_fill_distiller(direction = 1)
  return(plot)
}

plot_group_distances <- function(distance_grid, d) {
  plot <- ggplot(distance_grid, aes(x = Components, y = Group)) +
    geom_tile(aes(fill = Distances)) +
    scale_fill_distiller(direction = 1)
  return(plot)
}