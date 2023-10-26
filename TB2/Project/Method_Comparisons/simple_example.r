library(swfscMisc)
library(ClaritySim)
library(ggforce)
library(gridExtra)
library(tidyverse)
library(graphstats)
library(Clarity)
library(ggpubr)
set.seed(10)
source("UASE.R")
source("generate_data.r")
source("distances.R")

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
groups[moved] <- "Mixture"
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

uase <- UASE(cbind(original$Y, mix$Y), d = 2)
scaled_right <- uase$right %*% diag(1/uase$eigenvals) %>%
    as_tibble() %>%
    mutate(Group = c(groups, groups),
    dim = rep(c("1", "2"), each = 20))
colnames(scaled_right) <- c("1", "2", "Group", "dim")
right1 <- scaled_right %>%
    filter(dim == "1") %>%
    select(-dim)
right1_moved <- right1 %>%
    filter(Group == "Mixture") %>%
    mutate(Group = as.factor(Group))

ggplot(right1, aes(x = `1`, y = `2`, color = Group)) +
    geom_point() +
    geom_mark_circle(data = right1_moved,
        aes(x = `1`, y = `2`, color = Group, label = Group),
        expand = unit(1, "mm")) +
    theme_bw() +
    theme(legend.position = "none")
ggsave("uase_ex1.pdf", width = 5, height = 5)

right2 <- scaled_right %>%
    filter(dim == "2") %>%
    select(-dim)
right2_moved <- right2 %>%
    filter(Group == "Mixture") %>%
    mutate(Group = as.factor(Group))

ggplot(right2, aes(x = `1`, y = `2`, color = Group)) +
    geom_point() +
    geom_mark_ellipse(data = right2_moved,
        aes(x = `1`, y = `2`, color = Group, label = Group),
        expand = unit(1, "mm")) +
    theme_bw() +
    theme(legend.position = "none")
ggsave("uase_ex2.pdf", width = 5, height = 5)

#leg <- get_legend(p1, position = "right")

#pdf("uase.pdf", width = 10, height = 5)
#ggarrange(p1, p2, ncol = 2,
#    common.legend = TRUE)

omni <- gs.omni(A1 = original$Y, A2 =mix$Y)
colnames(omni) <- c(as.character(1:40))
plot_omni <- omni[, 1:2] %>%
    as_tibble() %>%
    mutate(dim = rep(c("1", "2"), each = 20))


plot_omni1 <- plot_omni %>%
    filter(dim == "1") %>%
    select(-dim) %>%
    mutate(Group = groups)

plot_omni_moved1 <- plot_omni1 %>%
    filter(Group == "Mixture") %>%
    mutate(Group = as.factor(Group))

plot_omni2 <- plot_omni %>%
    filter(dim == "2") %>%
    select(-dim) %>%
    mutate(Group = groups)
plot_omni_moved2 <- plot_omni2 %>%
    filter(Group == "Mixture")

ggplot(plot_omni1, aes(x = `1`, y = `2`, color = Group)) +
    geom_point() +
    geom_mark_circle(data = plot_omni_moved1,
        aes(x = `1`, y = `2`, color = Group, label = Group),
        expand = unit(1, "mm")) +
    theme_bw() +
    theme(legend.position = "none")
ggsave("omni_ex1.pdf", width = 5, height = 5)
ggplot(plot_omni2, aes(x = `1`, y = `2`, color = Group)) +
    geom_point() +
    geom_mark_ellipse(data = plot_omni_moved2,
        aes(x = `1`, y = `2`, color = Group, label = Group),
        expand = unit(1, "mm")) +
    theme_bw() +
    theme(legend.position = "none")
ggsave("omni_ex2.pdf", width = 5, height = 5)
#leg <- get_legend(p3, position = "right")
#pdf("omni_ex.pdf", width = 10, height = 5)
#ggarrange(p3, p4, ncol = 2,
#    common.legend = TRUE,
#    legend = "right",
#    legend.grob = leg)
#dev.off()

scan <- Clarity_Scan(original$Y, verbose = FALSE)
predict <- Clarity_Predict(mix$Y, scan)
persist <- Clarity_Persistence(predict)

plot_persist <- expand.grid(
    X = as.factor(1:12),
    Y = as.factor(1:n)
)
plot_persist$prediction <- c(t(persist[, 1:12]))

plot_persist <- as_tibble(plot_persist)
persist_p <- ggplot(plot_persist, aes(x = X, y = Y)) +
    stat_summary_2d(aes(z = prediction)) +
    geom_rect(aes(xmin = 0.5, xmax = 12.5, ymin = 16.5, ymax = 17.5),
        fill = "transparent",
        color = "#01012e",
        size = 0.5) +
    theme_bw() +
    scale_fill_distiller(direction = 1) +
    theme(legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

pdf("clarity_ex.pdf", width = 5, height = 5)
persist_p
dev.off()

