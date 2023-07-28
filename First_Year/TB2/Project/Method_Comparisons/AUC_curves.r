library(dplyr)
source("UASE.R")
source("distances.R")
library(ClaritySim)
library(ROCit)
library(ggplot2)

set.seed(123)
trials <- 100
n <- 100
k <- 10
l <- 50
d <- 12
data <- tibble(
    trial = NA,
    K = NA,
    AUC = NA
)
data <- data[-1, ]
for (i in 1:trials){
    original <- simulateCoalescent(n, k, l,
                         sigma0 = 0.01,
                         sigma = 0.01,
                         Amodel = "uniform",
                         alpha = 0,
                         minedge = 0.1)
    mix <- mixCoalescent(
        original,
        fraction = 1,
        transform = FALSE,
        multmin = 1,
        multmax = 1,
        qmin = 0.75
    )
    Y <- cbind(original$Y, mix$Y) # nolint
    groups <- rep(1:10, each = 10)
    uase <- UASE(Y, d = d, groups)
    distances <- distance_moved(uase$right, maxd = d,
        scale = TRUE, eigenvals = uase$eigenvals)
    anomaly <- mix$edges[1]
    distances <- distances %>%
        mutate(
            Anomaly = ifelse(Observations == anomaly, 1, 0)
        )
    for (ii in 1:d){
        current_distances <- distances %>%
            filter(Components == ii)
        current_roc <- rocit(score = current_distances$Distances,
                             class = current_distances$Anomaly)
        data <- data %>%
            add_row(
                trial = i,
                K = ii,
                AUC = current_roc$AUC
            )
    }
}
data <- data %>%
    mutate(
        K = as.factor(K)
    )
pdf("AUC_boxplot.pdf")
ggplot(data = data,
                     aes(x = K, y = AUC, fill = K)) +
    geom_boxplot(notch = TRUE) +
    stat_summary(fun = median,
               geom = "line",
               aes(group = 1)) +
    labs(x = "Number of Components",
         y = "AUC") +
    theme_bw() +
    theme(legend.position = "none")
dev.off()