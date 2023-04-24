require(ggplot2)
require(dplyr)
require(gridExtra)

UASE <- function(sim1, sim2,d = 2){
  Y<- cbind(sim1$Y, sim2$Y)
  
  svd <- svd(Y)
  
  A <- sim1$A
  population <- A %*% 1:ncol(A)
  
  
  U <- svd$u %*% diag(sqrt(svd$d))
  U <- U[, 1:d]
  left <- as.data.frame(cbind(U, as.factor(population)))
  colnames(left) <- c("1", "2", "population")
  
  t <- rep(1:2, each = nrow(A))
  index <- rep(1:nrow(A),2)
  V <- svd$v %*% diag(sqrt(svd$d))
  V <- V[,1:d]
  right <- as.data.frame(cbind(V,as.factor(t), 
                               as.factor(population), 
                               as.factor(index)))
  
  colnames(right) <- c("1", "2"," discipline", "population", "observation")
  
  return(list(left = left, right = right))
}

plot_UASE <- function(UASE){
  
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

distance_moved <- function(UASE){
  V <- UASE$right %>%
    as_tibble()
  n <- nrow(V) 
  
  
  joined <- cbind()
}