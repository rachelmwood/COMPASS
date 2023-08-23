library(dplyr)
source("UASE.R")
source("distances.R")
source("rocs.R")
library(ClaritySim)
library(ROCit)
library(ggplot2)
library(graphstats)

trials <- 100
n <- 100
k <- 10
l <- 50
d <- 12
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
                         sigma0 = 0.02,
                         sigma = 0.5,
                         Amodel = "uniform",
                         alpha = 0,
                         minedge = 0.1)
    mix <- mixCoalescent(
        original,
        transform = FALSE,
        fraction = 1,
        qmin = 0.8
    )

    # UASE AUC Computations
    Y <- cbind(original$Y, mix$Y)
    groups <- rep(1:10, each = k)
    uase <- UASE(Y, d = d, groups)
    uase_distances <- distance_moved(uase$right, maxd = d,
        scale = TRUE, eigenvals = uase$eigenvals)
    anomaly <- mix$edges[1]
    uase_distances <- uase_distances %>%
        mutate(
            Anomaly = ifelse(Observations == anomaly, 1, 0)
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
            Anomaly = ifelse(Observations == anomaly, 1, 0)
        )
    omni_auc <- auc(omni_distances, d)
    data <- data %>%
        rbind(tibble(
            trial = i,
            Method = "Omni",
            K = omni_auc$K,
            AUC = omni_auc$AUC
        ))
}
data <- data %>%
    mutate(
        K = as.factor(K)
    )

uase_data <- data %>%
    filter(Method == "UASE")
ggplot(data = uase_data,
        aes(x = K, y = AUC, fill = K)) +
    geom_boxplot(notch = TRUE) +
    stat_summary(fun = median,
               geom = "line",
               aes(group = 1)) +
    labs(x = "Number of Components",
         y = "AUC") +
    theme_bw() +
    theme(legend.position = "none")
ggsave("uase_AUC_boxplot.pdf", width = 5, height = 5)

omni_data <- data %>%
    filter(Method == "Omni")
ggplot(data = omni_data,
        aes(x = K, y = AUC, fill = K)) +
    geom_boxplot(notch = TRUE) +
    stat_summary(fun = median,
               geom = "line",
               aes(group = 1)) +
    labs(x = "Number of Components",
         y = "AUC") +
    theme_bw() +
    theme(legend.position = "none")
ggsave("omni_AUC_boxplot.pdf", width = 5, height = 5)