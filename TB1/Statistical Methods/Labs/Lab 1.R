library(readr)

#download dataset

data <- read.table("/home/ac18826/Documents/COMPASS/First Year/Statistical Methods/Labs/prostate.csv")

#extracting response variable
response <- data[,9]
predictors <- data[,1:8]
test_ind <- data[,10]

X <- t(cbind(predictors, rep(1, len = nrow(predictors))))
w_ls <- solve(X %*% t(X)) %*% X %*% response
w_ls
ls_solver <- function(data, response){
  X <- cbind(data, rep(1, len = row(X)))
}
