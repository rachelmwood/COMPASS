library(ggbiplot)
set.seed(9)

n <- 100
k <- 5
l <- 300
d <- 12
sigma0 <- 0.1
sigma <- 1
multmin <- 0.5
multmax <- 2
beta <- 1
fraction <- 0.2
qmin <- 0.8

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

plot(original$tree)
original_sim <- expand.grid(
    X = as.factor(1:n),
    Y = as.factor(1:n)
)
original_sim$Values <- c(original$Y)
original_sim <- as_tibble(original_sim)

mix_sim <- expand.grid(
    X = as.factor(1:n),
    Y = as.factor(1:n)
)
mix_sim$Values <- c(mix$Y)
mix_sim <- as_tibble(mix_sim)

ggplot(original_sim, aes(x = X, y = Y)) +
    stat_summary_2d(aes(z = Values)) +
    theme_bw() +
    scale_fill_distiller() +
    theme(
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
    )


ggsave("simple_sim_mat.pdf", width = 5, height = 5)

ggplot(mix_sim, aes(x = X, y = Y)) +
    stat_summary_2d(aes(z = Values)) +
    theme_bw() +
    scale_fill_distiller() +
    theme(
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
    )
ggsave("mix_sim_mat.pdf", width = 5, height = 5)

original_pca <- prcomp(x = t(original$D), scale. = TRUE)

original_pc_data <- original_pca$rotation[,1:2] %>%
    as_tibble() %>%
    mutate(Group = as.factor(rep(1:5, each =20)))

ggplot(original_pc_data, aes(x = PC1, y = PC2)) +
    geom_point(aes(colour = Group)) +
    theme_bw()