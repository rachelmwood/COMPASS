trials <- 100
n <- 100
    k <- 10
    l <- 300
    d <- 12
    sigma0 <- 0.1
    sigma <- 0.5
    # parameters for mixCoalescent
    multmin <- 0.5
    multmax <- 2
    beta <- 1
    fraction <- 0.2
    qmin <- 0.8

roc_data <- tibble(
    Method = NA,
    TPR = NA,
    FPR = NA
)
roc_data <- roc_data[-1, ]
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
    uase <- UASE(Y, d = d)
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
    
    roc_data <- roc_data %>% 
        rbind(tibble(
            Method = "UASE",
            TPR = uase_auc$TPR,
            FPR = uase_auc$FPR
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
    roc_data <- roc_data %>% 
        rbind(tibble(
            Method = "OMNI",
            TPR = omni_auc$TPR,
            FPR = omni_auc$FPR
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
    roc_data <- roc_data %>%
        rbind(tibble(
            Method = "CLARITY",
            TPR = clarity_auc$TPR,
            FPR = clarity_auc$FPR
        ))
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
    roc_data <- roc_data %>%
        rbind(tibble(
            Method = "Procrustes",
            TPR = procrustes_auc$TPR,
            FPR = procrustes_auc$FPR
        ))
    roc_data <- roc_data %>%
        rbind(tibble(
            Method = "Random",
            TPR = seq(0,1,0.1),
            FPR = seq(0,1,0.1)
        ))
roc_data <- roc_data %>%
    mutate(Method = as.factor(Method))

ggplot(roc_data, aes(x = FPR, y = TPR, color = Method)) +
    geom_line(linewidth = 0.8, aes(linetype = Method)) +
    labs(x = "False Positive Rate",
         y = "True Positive Rate",
         title = "ROC Curve") +
    ylim(0, 1) +
    theme_bw()
ggsave("roc_example.pdf", width = 5, height = 5)
