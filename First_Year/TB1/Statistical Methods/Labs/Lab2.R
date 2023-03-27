e <- rnorm(200, mean = 0, sd=sqrt(0.64))
x <- runif(200)

y <- exp(1.5*x - 1) + e

 lambda <- 1
w_lsr <-
