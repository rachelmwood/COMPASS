library(swfscMisc)
library(ClaritySim)
library(ggforce)
library(tidyverse)
set.seed(10)

n <- 20
k <- 5
l <- 10
d <- 12
sigma0 <- 0.05
sigma <- 1
beta <- 1
fraction <- 0.2
qmin <- 0.8

load('original_tree.rds')
load('mix_tree.rds')

tree$edge.length[1] <- 0.01
tree$edge.length[c(2,3,7,8)] <- 0.05
original <- simulateCoalescent(n, k, l,
                sigma0 = sigma0,
                sigma = sigma,
                Amodel = "uniform",
                alpha = 0,
                minedge = 0.1,
                tree = tree)
tree$edge.length <- mix_vec / 50
original$tree <- tree
mix <- mixCoalescent(
                original,
                transform = FALSE,
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

original_pca <- prcomp(x = (original$D), scale. = TRUE)

original_pc_data <- original_pca$x[, 1:2] %>%
    as_tibble() %>%
    mutate(Group = as.factor(rep(1:5, each = 4)))

ggplot(original_pc_data, aes(x = PC1, y = PC2, colour = Group)) +
    geom_point(size = 3) +
    geom_mark_ellipse(aes(fill = Group, label = Group),
        label.fontsize = 14) +
    theme_bw() +
    theme(
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
    )
ggsave("original_pc.pdf", width = 5, height = 5)

groups <- rep(1:5, each = 4)
moved <- rowSums(abs(original$A - mix$A)) != 0
groups[moved] <- "Moved"
groups <- as.factor(groups)

mix_pca <- prcomp(x = (mix$D), scale. = TRUE)
mix_pc_data <- mix_pca$x[, 1:2] %>%
    as_tibble() %>%
    mutate(Group = groups)

ggplot(mix_pc_data, aes(x = PC1, y = PC2, colour = Group)) +
    geom_point(size = 3) +
    geom_mark_ellipse(aes(fill = Group, label = Group),
        label.fontsize = 14) +
    theme_bw() +
    theme(
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
    )
ggsave("mix_pc.pdf", width = 5, height = 5)