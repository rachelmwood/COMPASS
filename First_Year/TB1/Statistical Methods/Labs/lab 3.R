library(clusterGeneration)

n <- 10
mu <- runif(n,1,10)

sigma <- genPositiveDefMat(n)$Sigma

D <- diag(eigen(sigma)$values)
U <- diag(eigen(sigma)$vectors)

y <- rnorm(mean = 0, sd =  1/diag(D))
