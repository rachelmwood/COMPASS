source("distances.R")
require(ggplot2)
require(ROCit)
require(dplyr)

auc <- function(distances, d) {
    data <- tibble(K = NA, AUC = NA)
    data <- data[-1, ]
    for (ii in 1:d) {
        current_distances <- distances %>%
            filter(Components == ii)
        current_roc <- rocit(
            score = current_distances$Distances,
            class = current_distances$Anomaly
        )
        data <- data %>%
            add_row(
                K = ii,
                AUC = current_roc$AUC
            )
    }
    return(data)
}

auc_boxplot <- function(data, method) {
    plot_data <- data %>%
        filter(Method == method)
    ggplot(data = plot_data,
        aes(x = K, y = AUC, fill = K)) +
    geom_boxplot(notch = TRUE) +
    stat_summary(fun = median,
               geom = "line",
               aes(group = 1)) +
    labs(x = "Number of Components",
         y = "AUC") +
    theme_bw() +
    theme(legend.position = "none")
}