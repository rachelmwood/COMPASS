library(readr)
library(dplyr)
library(ggplot2)
library(latex2exp)
library(ggpubr)
scaling_data <- read_csv("scaling_data.csv")
noise_data <- read_csv("noise_data.csv")
colnames(scaling_data) <- c("Alpha", "Beta", "Scaling", "Method", "Average")
colnames(noise_data) <- c("Alpha", "Beta", "Noise", "Method", "Average")


# Scaling plots
ggplot(scaling_data,
       aes(x = Scaling, y = Average, color = Method)) +
    geom_line(linewidth = 0.8) +
    labs(x = "Scaling",
         y = "Average AUC") +
    facet_grid(Alpha ~ Beta, labeller = label_both) +
    ylim(0, 1) +
    theme_bw() +
    theme(legend.position = "bottom")
ggsave("scalingAUC.pdf", width = 10, height = 5)

scaling_data1 <- scaling_data %>%
    filter(Alpha == 0.5 & Beta == 0.5)

scaling_p1 <- ggplot(scaling_data1,
       aes(x = Scaling, y = Average, color = Method)) +
     geom_line(linewidth = 0.8) +
     labs(x = "Scaling",
          y = "Average AUC",
          title = TeX("$\\alpha = 0.5, \\beta = 0.5")) +
     ylim(0, 1) +
     theme_bw()

scaling_data2 <- scaling_data %>%
    filter(Alpha == 0.25 & Beta == 1)

scaling_p2 <- ggplot(scaling_data2,
       aes(x = Scaling, y = Average, color = Method)) +
     geom_line(linewidth = 0.8) +
     labs(x = "Scaling",
          y = "Average AUC",
          title = TeX("$\\alpha = 0.25, \\beta = 1")) +
     ylim(0, 1) +
     theme_bw()

scaling_data3 <- scaling_data %>%
    filter(Alpha == 1 & Beta == 0.25)

scaling_p3 <- ggplot(scaling_data3,
       aes(x = Scaling, y = Average, color = Method)) +
     geom_line(linewidth = 0.8) +
     labs(x = "Scaling",
          y = "Average AUC",
          title = TeX("$\\alpha = 1, \\beta = 0.25")) +
     ylim(0, 1) +
     theme_bw()

scaling_data4 <- scaling_data %>%
    filter(Alpha == 1 & Beta == 0.75)

scaling_p4 <- ggplot(scaling_data4,
       aes(x = Scaling, y = Average, color = Method)) +
     geom_line(linewidth = 0.8) +
     labs(x = "Scaling",
          y = "Average AUC",
          title = TeX("$\\alpha = 1, \\beta = 0.75")) +
     ylim(0, 1) +
     theme_bw()
pdf("scalingAUC_ind.pdf", width = 10, height = 5)
ggarrange(scaling_p3, scaling_p1, scaling_p4, scaling_p2,
          ncol = 4, common.legend = TRUE, legend = "bottom")
dev.off()


ggplot(noise_data,
       aes(x = Noise, y = Average, color = Method)) +
    geom_line(linewidth = 0.8) +
    labs(x = "Noise",
         y = "Average AUC") +
    facet_grid(Alpha ~ Beta, labeller = label_both) +
    ylim(0, 1) +
    theme_bw() +
    theme(legend.position = "bottom")
ggsave("noiseAUC.pdf", width = 10, height = 5)

noise_data1 <- noise_data %>%
    filter(Alpha == 0.5 & Beta == 0.5)

noise_p1 <- ggplot(noise_data1,
       aes(x = Noise, y = Average, color = Method)) +
     geom_line(linewidth = 0.8) +
     labs(x = "Noise",
          y = "Average AUC",
          title = TeX("$\\alpha = 0.5, \\beta = 0.5")) +
     ylim(0, 1) +
     theme_bw()

noise_data2 <- noise_data %>%
     filter(Alpha == 0.25 & Beta == 1)


noise_p2 <- ggplot(noise_data2,
       aes(x = Noise, y = Average, color = Method)) + 
     geom_line(linewidth = 0.8) +
     labs(x = "Noise",
          y = "Average AUC",
          title = TeX("$\\alpha = 0.25, \\beta = 1")) +
     ylim(0, 1) +
     theme_bw()

noise_data3 <- noise_data %>%
     filter(Alpha == 1 & Beta == 0.25)

noise_p3 <- ggplot(noise_data3,
       aes(x = Noise, y = Average, color = Method)) +
     geom_line(linewidth = 0.8) +
     labs(x = "Noise",
          y = "Average AUC",
          title = TeX("$\\alpha = 1, \\beta = 0.25")) +
     ylim(0, 1) +
     theme_bw()

noise_data4 <- noise_data %>%
     filter(Alpha == 1 & Beta == 0.75)

noise_p4 <- ggplot(noise_data4,
       aes(x = Noise, y = Average, color = Method)) +
     geom_line(linewidth = 0.8) +
     labs(x = "Noise",
          y = "Average AUC",
          title = TeX("$\\alpha = 1, \\beta = 0.75")) +
     ylim(0, 1) +
     theme_bw()

pdf("noiseAUC_ind.pdf", width = 10, height = 5)
ggarrange(noise_p3, noise_p1, noise_p4, noise_p2,
          ncol = 4, common.legend = TRUE, legend = "bottom")
dev.off()

pdf("noise_scaling_ind.pdf", width = 10, height = 7.5)
ggarrange(scaling_p3, scaling_p1, scaling_p4, scaling_p2,
          noise_p3, noise_p1, noise_p4, noise_p2,
          ncol = 4, nrow = 2, common.legend = TRUE, legend = "bottom")
dev.off()