library(dplyr)
source("distances.R")
source("rocs.R")
library(ClaritySim)
library(Clarity)
library(ROCit)
library(ggplot2)
library(graphstats)
library(doParallel)
library(readr)
library(vegan)


simulate_mixtures_pros <- function(
    # parameters for data simulation
    trials = 100,
    n = 100,
    k = 10,
    l = 300,
    d = 12,
    sigma0 = 0.1,
    sigma = 0.5,
    # parameters for mixCoalescent
    multmin = 1,
    multmax = 1,
    beta = 1,
    fraction = 0.2,
    qmin = 0.8
) {
    data <- tibble(
    trial = NA,
    Method = NA,
    AUC = NA
)
data <- data[-1, ]
for (i in 1:trials){
    set.seed(i)
    original <- simulateCoalescent(n, k, l,
                         sigma0 = sigma0,
                         sigma = sigma,
                         Amodel = "uniform",
                         alpha = 0,
                         minedge = 0.1)
    mix <- mixCoalescent(
        original,
        transform = TRUE,
        multmin = multmin,
        multmax = multmax,
        beta = beta,
        fraction = fraction,
        qmin = qmin
    )
    diff_adj <- original$A - mix$A
    anomalies <- as.numeric(rowSums(abs(diff_adj)) != 0)

    groups <- rep(1:10, each = k)
    # Procustes AUC Computations
    procrustes <- procrustes(original$Y, mix$Y)
    procrustes_res <- (procrustes$Yrot - procrustes$X)^2

    procrustes_scores <- cbind(
        Observations = 1:n,
        Distances = as.vector(procrustes_res),
        Anomaly = as.numeric(anomalies)) %>%
        as_tibble()
    procrustes_auc <- rocit(
            score = procrustes_scores$Distances,
            class = procrustes_scores$Anomaly
        )
    data <- data %>%
        rbind(tibble(
            trial = i,
            Method = "Procrustes",
            AUC = procrustes_auc$AUC
        ))
return(data)

}
}

scaling <- seq(1, 10, 0.5)
sigma0 <- seq(0.1, 1, 0.1)^2
fraction <- c(0.25, 0.5, 0.75, 1)
beta <- c(0.25, 0.5, 0.75, 1)
scaling_data <- tibble(
    Fraction = NA,
    Beta = NA,
    Scaling = NA,
    Method = NA,
    K = NA,
    Average = NA
)
noise_data <- tibble(
    Fraction = NA,
    Beta = NA,
    Noise = NA,
    Method = NA,
    K = NA,
    Average = NA
)


scaling_data <- scaling_data[-1, ]
noise_data <- noise_data[-1, ]

for (i in 1: length(fraction)) {
    for (j in 1: length(beta)) {
        for (k in 1: length(scaling)) {
            sim_move <- simulate_mixtures_pros(multmin = 1 / scaling[k],
                                          multmax = scaling[k],
                                         beta = beta[j],
                                          fraction = fraction[i])
           sim_move <- sim_move %>%
                group_by(Method) %>%
                summarise(Average = mean(AUC)) %>%
                ungroup()

            current_scaling_data <- tibble(
                    Fraction = fraction[i],
                    Beta = beta[j],
                    Scaling = scaling[k],
                    Method = sim_move$Method,
                    Average = sim_move$Average
                )
            write_csv(current_scaling_data, "scaling_data.csv", append = TRUE)
            scaling_data <- scaling_data %>%
                rbind(current_scaling_data)
        }

        for (l in 1: length(sigma0)) {
            sim_move <- simulate_mixtures_pros(sigma0 = sigma0[l],
                                          multmin = 1,
                                          multmax = 1,
                                          beta = beta[j],
                                          fraction = fraction[i])
            sim_move <- sim_move %>%
                group_by(Method) %>%
                summarise(Average = mean(AUC)) %>%
                ungroup()

            current_noise_data <- tibble(
                    Fraction = fraction[i],
                    Beta = beta[j],
                    Noise = sigma0[l],
                    Method = sim_move$Method,
                    K = sim_move$K,
                    Average = sim_move$Average
                )
            write_csv(current_noise_data, "noise_data.csv", append = TRUE)
            noise_data <- noise_data %>%
                rbind(current_noise_data)
        }
    }
    print(i)
}

