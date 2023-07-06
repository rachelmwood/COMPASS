require(ggplot2)
require(dplyr)
require(gridExtra)

UASE <- function(similarity, d = 2, population) {

  svd <- svd(similarity)
  U <- svd$u %*% diag(sqrt(svd$d))
  U <- U[, 1:d]
  left <- as.data.frame(cbind(U, population))
  d_char <- as.character(1:d)
  colnames(left) <- c(d_char, "population")
  
  t <- rep(1:2, each = nrow(similarity))
  index <- rep(1:nrow(similarity),2)
  V <- svd$v %*% diag(sqrt(svd$d))
  V <- V[,1:d]
  right <- as.data.frame(cbind(V,as.factor(t),
                               (population),
                               as.factor(index)))
  
  colnames(right) <- c(d_char," discipline", "population", "observation")
  return(list(left = left, right = right, eigenvals = svd$d[1:d]))
}

plot_UASE <- function(UASE, d = 2){
  
  U <- UASE$left %>%
    as_tibble() %>%
    mutate(population = as.factor(population))
  
  V <- UASE$right
  V1 <- V[1:(nrow(V)/2),] %>%
    as_tibble() %>%
    mutate(population = as.factor(population))
  
  
  V2 <- V[-(1:(nrow(V)/2)),] %>%
    as_tibble() %>%
    mutate(population = as.factor(population))
  
  V1_plot <- ggplot(V1, aes(`1`,`2`, color = population)) +
    geom_point(alpha = 0.5) +
    geom_jitter() +
    scale_color_viridis_d(option = "turbo") + 
    theme(legend.position = "none") +
    labs(title = "V Discipline 1")
  
  V2_plot <- ggplot(V2, aes(`1`,`2`, color = population)) +
    geom_point(alpha = 0.5) +
    geom_jitter() +
    scale_color_viridis_d(option = "turbo") + 
    theme(legend.position = "none") +
    labs(title = "V Discipline 2")
  
  U_plot <- ggplot(U, aes(`1`,`2`, color = population)) +
    geom_point(alpha = 0.5) +
    geom_jitter() +
    scale_color_viridis_d(option = "turbo", guide = guide_legend(ncol = 3, label.theme = element_text(size= 8))) +
    labs(title = "U")
  
  lay <- rbind(c(1,2),
               c(3,3))
  grid.arrange(V1_plot, V2_plot, U_plot, layout_matrix = lay)
}

distance_moved <- function(V, d, scale=TRUE, eigenvals = NULL){
  
  if(scale){
    V[,1:d] <-as.matrix(V[,1:d]) %*% diag(as.vector(sqrt(eigenvals[1:d])), nrow = d)
  }
  V <- V %>% as_tibble()
  
  n <- nrow(V) 
  
  V1 <- V[1:(n/2),1:d]
  V2 <- V[(n/2 +1):n, 1:d]
  moved <- sapply(1:(n/2), FUN = function(i){
    dist(rbind(V1[i,], V2[i,]))
  })
  
  return(moved)
}

plot_distances <- function(UASE, d){
  distances <- apply(as.matrix(1:d, nrow = 1), MARGIN = 1, FUN = function(ii){
    distance_moved(mixed_UASE, d = ii)
  })
}
