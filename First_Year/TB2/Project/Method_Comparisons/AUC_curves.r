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
    K = NA,
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
            Anomaly = as.numeric(rep(anomalies, each = d))
        )
    uase_auc <- auc(uase_distances, d)
    data <- data %>%
        rbind(tibble(
            trial = i,
            Method = "UASE",
            K = uase_auc$K,
            AUC = uase_auc$AUC
        ))
    # OMNI AUC Computations
    omni <- svd(gs.omni(original$Y, mix$Y))

    omni_distances <- distance_moved(omni$u, maxd = d,
        scale = FALSE)
    omni_distances <- omni_distances %>%
        mutate(
            Anomaly = as.numeric(rep(anomalies, each = d))
        )
    omni_auc <- auc(omni_distances, d)
    data <- data %>%
        rbind(tibble(
            trial = i,
            Method = "Omni",
            K = omni_auc$K,
            AUC = omni_auc$AUC
        ))

    # CLARITY AUC Computations
    scan <- Clarity_Scan(original$Y, verbose = FALSE)
    predict <- Clarity_Predict(mix$Y, scan)
    persist <- Clarity_Persistence(predict)
    clarity_scores <- expand.grid(
        Components = as.factor(1:d),
        Observations = as.factor(1:n))
    clarity_scores$Distances <- c(t(persist[, 1:d]))
    clarity_scores <- clarity_scores %>%
        as_tibble()

    clarity_scores <- clarity_scores %>%
        mutate(
            Anomaly = as.numeric(rep(anomalies, each = d))
        )
    clarity_auc <- auc(clarity_scores, d)
    data <- data %>%
        rbind(tibble(
            trial = i,
            Method = "Clarity",
            K = clarity_auc$K,
            AUC = clarity_auc$AUC
        ))
}
data <- data %>%
    mutate(
        K = as.factor(K)
    )
return(data)

}

scaling <- seq(1, 10, 0.5)
mean_data <- tibble(
    Scaling = NA,
    Method = NA,
    K = NA,
    Average = NA
)
mean_data <- mean_data[-1, ]
for (j in 1:length(scaling)) {
    sim_move <- simulate_mixtures(multmin = 1 / scaling[j],
                                  multmax = scaling[j])
    sim_move <- sim_move %>%
        group_by(Method, K) %>%
        summarise(Average = mean(AUC))

    mean_data <- mean_data %>%
        rbind(tibble(
            Scaling = scaling[j],
            Method = sim_move$Method,
            K = sim_move$K,
            Average = sim_move$Average
        ))
}
