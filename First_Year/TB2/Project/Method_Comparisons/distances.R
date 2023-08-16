require(dplyr)
require(ggplot2)


dim_distance <- function(V, d, l = 2, scale = TRUE) {
  V <- V %>% as_tibble()
  n <- nrow(V)
  V1 <- V[1:(n / 2), 1:d]
  V2 <- V[(n / 2 + 1):n, 1:d]

  if (scale) {
    V1 <- V1 %>% scale()
    V2 <- V2 %>% scale()
  }
  moved <- sapply(1:(n/2), FUN = function(i){
    dist(rbind(V1[i, ], V2[i, ]), method = "minkowski", p = l)
  })
  return(moved)
}

distance_moved <- function(mat, maxd, scale = TRUE) {
  distances <- apply(
    as.matrix(1:maxd, nrow = 1),
    MARGIN = 1,
    FUN = function(ii) {
        dim_distance(mat, d = ii, scale = scale)
    })
  distances_grid <- expand.grid(
    Components = as.factor(1:maxd),
    Observations = as.factor(1:(nrow(mat) / 2)))
  distances_grid$Distances <- c(t(distances))
  distances_grid <- distances_grid %>%
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

plot_distances <- function(distances, d) {
  distance_plot <- expand.grid(
    Components = 1:d,
    Observations = 1:nrow(distances)) # nolint
  distance_plot$Distances <- c(t(distances))
  distance_plot <- distance_plot %>% as_tibble()
  plot <- ggplot(distance_plot,
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