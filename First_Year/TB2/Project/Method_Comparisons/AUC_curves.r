library(dplyr)
source("UASE.R")
source("distances.R")
source("rocs.R")
library(ClaritySim)
library(Clarity)
library(ROCit)
library(ggplot2)
library(graphstats)

simulate_mixtures <- function(
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
    # UASE AUC Computations
    Y <- cbind(original$Y, mix$Y)
    uase <- UASE(Y, d = d, groups)
    uase_distances <- distance_moved(uase$right, maxd = d,
        scale = TRUE, eigenvals = uase$eigenvals)
    uase_distances <- uase_distances %>%
        mutate(
            Anomaly = as.numeric(rep(anomalies))
        )
    uase_auc <- rocit(
            score = uase_distances$Distances,
            class = uase_distances$Anomaly
        )
    data <- data %>%
        rbind(tibble(
            trial = i,
            Method = "UASE",
            AUC = uase_auc$AUC
        ))
    
    # OMNI AUC Computations
    omni <- svd(gs.omni(original$Y, mix$Y))

    omni_distances <- distance_moved(omni$u, maxd = d,
        scale = FALSE)
    omni_distances <- omni_distances %>%
        mutate(
            Anomaly = as.numeric(anomalies)
        )
    omni_auc <- rocit(
            score = omni_distances$Distances,
            class = omni_distances$Anomaly
        )
    data <- data %>%
        rbind(tibble(
            trial = i,
            Method = "Omni",
            AUC = omni_auc$AUC
        ))

    # CLARITY AUC Computations
    scan <- Clarity_Scan(original$Y, verbose = FALSE)
    predict <- Clarity_Predict(mix$Y, scan)
    persist <- Clarity_Persistence(predict)
    clarity_scores <- cbind(Observations = 1:n, Distances = persist[, d]) %>%
        as_tibble()

    clarity_scores <- clarity_scores %>%
        mutate(
            Anomaly = as.numeric(anomalies)
        )
    clarity_auc <- rocit(
            score = clarity_scores$Distances,
            class = clarity_scores$Anomaly
        )
    data <- data %>%
        rbind(tibble(
            trial = i,
            Method = "Clarity",
            AUC = clarity_auc$AUC
        ))
}
return(data)

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
            sim_move <- simulate_mixtures(multmin = 1 / scaling[k],
                                          multmax = scaling[k],
                                          beta = beta[j],
                                          fraction = fraction[i])
            sim_move <- sim_move %>%
                group_by(Method) %>%
                summarise(Average = mean(AUC)) %>%
                ungroup()

            scaling_data <- scaling_data %>%
                rbind(tibble(
                    Fraction = fraction[i],
                    Beta = beta[j],
                    Scaling = scaling[k],
                    Method = sim_move$Method,
                    K = sim_move$K,
                    Average = sim_move$Average
                ))
        }

        for (l in 1: length(sigma0)) {
            sim_move <- simulate_mixtures(sigma0 = sigma0[l],
                                          multmin = 1,
                                          multmax = 1,
                                          beta = beta[j],
                                          fraction = fraction[i])
            sim_move <- sim_move %>%
                group_by(Method) %>%
                summarise(Average = mean(AUC)) %>%
                ungroup()

            noise_data <- noise_data %>%
                rbind(tibble(
                    Fraction = fraction[i],
                    Beta = beta[j],
                    Noise = sigma0[l],
                    Method = sim_move$Method,
                    K = sim_move$K,
                    Average = sim_move$Average
                ))
        }
    }
}

ggplot(scaling_data,
       aes(x = Scaling, y = Average, color = Method)) +
    geom_line(linewidth = 1) +
    labs(x = "Scaling",
         y = "Average AUC") +
    ylim(0, 1) +
    facet_wrap(~Fraction + Beta) +
    theme_bw()
ggsave("scalingAUC.pdf")

ggplot(noise_data,
       aes(x = Noise, y = Average, color = Method)) +
    geom_line(linewidth = 1) +
    labs(x = "Noise",
         y = "Average AUC") +
    ylim(0, 1) +
    facet_wrap(~Fraction + Beta) +
    theme_bw()

ggsave("noiseAUC.pdf")