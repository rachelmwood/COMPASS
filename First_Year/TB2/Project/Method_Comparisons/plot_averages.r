library(readr)
library(dplyr)
library(ggplot2)
scaling_data <- read_csv("scaling_data.csv")
noise_data <- read_csv("noise_data.csv")

ggplot(scaling_data,
       aes(x = Scaling, y = Average, color = Method)) +
    geom_line(linewidth = 0.8) +
    labs(x = "Scaling",
         y = "Average AUC") +
    facet_grid(Fraction ~ Beta, labeller = label_both) +
    ylim(0, 1) +
    theme_bw() +
    theme(legend.position = "bottom")
ggsave("scalingAUC.pdf")

ggplot(noise_data,
       aes(x = Noise, y = Average, color = Method)) +
    geom_line(linewidth = 0.8) +
    labs(x = "Noise",
         y = "Average AUC") +
    facet_grid(Fraction ~ Beta, labeller = label_both) +
    ylim(0, 1) +
    theme_bw() +
    theme(legend.position = "bottom")
ggsave("noiseAUC.pdf")