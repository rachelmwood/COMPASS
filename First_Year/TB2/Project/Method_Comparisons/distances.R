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

plot_distances <- function(distances, d) {
  distance_plot <- expand.grid(
    Components = (1:12),
    Observations = 1:nrow(distances)) # nolint
  distance_plot$Distances <- as.numeric(c(t(distances))) %>%
    as_tibble()
  plot <- ggplot(distance_plot, aes(x = Components)) +
    geom_tile(aes(y = Observations, fill = Distances)) #+
    #scale_fill_distiller(direction = 1)
  return(plot)
}