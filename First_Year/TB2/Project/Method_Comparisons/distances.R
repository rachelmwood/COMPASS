
distance_moved <- function(V, d, l =2, scale=TRUE, eigenvals = NULL){
  
  if(scale){
    V[,1:d] <-as.matrix(V[,1:d]) %*% diag(1/sqrt(as.numeric(eigenvals[1:d])), nrow = d)
  }
  V <- V %>% as_tibble()
  
  n <- nrow(V) 
  
  V1 <- V[1:(n/2),1:d]
  V2 <- V[(n/2 +1):n, 1:d]
  moved <- sapply(1:(n/2), FUN = function(i){
    dist(rbind(V1[i,], V2[i,]), method = "minkowski", p =l)
  })
  
  return(moved)
}

plot_distances <- function(UASE, d){
  distances <- apply(as.matrix(1:d, nrow = 1), MARGIN = 1, FUN = function(ii){
    distance_moved(mixed_UASE, d = ii)
  })
}

