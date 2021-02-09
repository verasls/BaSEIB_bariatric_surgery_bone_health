# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(patchwork)
library(ragg)

# Read and prepare data ---------------------------------------------------

files <- list(
  body_weight = here("output/emm_whole_body_total_mass_time.csv"),
  steps = here("output/emm_steps_time.csv"),
  GRF = here("output/emm_gravitational_loading_time.csv"),
  sclerostin = here("output/emm_sclerostin_time.csv")
)

plot_data <- map(files, read_csv) %>%
  map(
    mutate,
    time = recode_factor(
      as.factor(time),
      "1" = "Pre-BS",
      "2" = "1-month post-BS",
      "3" = "6-months post-BS",
      "4" = "12-months post-BS"
    )
  )

# Body weight plot --------------------------------------------------------

body_weight_plot <- ggplot(plot_data$body_weight) +
  geom_point(aes(x = time, y = emmean, size = 4)) +
  geom_line(aes(x = time, y = emmean, group = 1, size = 1)) +
  geom_errorbar(
    aes(x = time, ymin = lower.CL, ymax = upper.CL, size = 1, width = 0.1)
  ) +
  scale_y_continuous(
    breaks = seq(60, 120, 10),
    expand = c(0, 0),
    limits = c(60, 121)
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold")
  ) +
  labs(
    x = "",
    y = quote("Body weight (kg)")
  ) +
  annotate("text", x = 2.07, y = 103, label = "a''") +
  annotate("text", x = 3.12, y = 82, label = "a'' b''") +
  annotate("text", x = 4.15, y = 73.55, label = "a'' b'' c''")

# Steps plot --------------------------------------------------------------

steps_plot <- ggplot(plot_data$steps) +
  geom_point(aes(x = time, y = emmean, size = 4)) +
  geom_line(aes(x = time, y = emmean, group = 1, size = 1)) +
  geom_errorbar(
    aes(x = time, ymin = lower.CL, ymax = upper.CL, size = 1, width = 0.1)
  ) +
  scale_y_continuous(
    breaks = seq(4000, 10000, 1000),
    expand = c(0, 0),
    limits = c(4000, 10200)
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold")
  ) +
  labs(
    x = "",
    y = quote("Steps" ~ (n %.% day^-1))
  ) +
  annotate("text", x = 4.07, y = 8900, label = "b'")

# GRF plot ----------------------------------------------------------------

GRF_plot <- ggplot(plot_data$GRF) +
  geom_point(aes(x = time, y = emmean, size = 4)) +
  geom_line(aes(x = time, y = emmean, group = 1, size = 1)) +
  geom_errorbar(
    aes(x = time, ymin = lower.CL, ymax = upper.CL, size = 1, width = 0.1)
  ) +
  scale_y_continuous(
    breaks = seq(6000, 13000, 1000),
    expand = c(0, 0),
    limits = c(6000, 13300)
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold")
  ) +
  labs(
    x = "",
    y = quote("Volume of ambulatory gravitational loading" ~ (kN %.% day^-1))
  ) +
  annotate("text", x = 2.06, y = 9350, label = "a") +
  annotate("text", x = 3.05, y = 8850, label = "a") +
  annotate("text", x = 4.055, y = 9170, label = "a")

# Sclerostin plot ---------------------------------------------------------

sclerostin_plot <- ggplot(plot_data$sclerostin) +
  geom_point(aes(x = time, y = emmean, size = 4)) +
  geom_line(aes(x = time, y = emmean, group = 1, size = 1)) +
  geom_errorbar(
    aes(x = time, ymin = lower.CL, ymax = upper.CL, size = 1, width = 0.1)
  ) +
  scale_y_continuous(
    breaks = seq(0.400, 0.800, 0.100),
    expand = c(0, 0),
    limits = c(0.400, 0.820),
    labels = scales::number_format(accuracy = 0.001)
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold")
  ) +
  labs(
    x = "",
    y = quote("Sclerostin" ~ ( ng %.% mL^-1))
  ) +
  annotate("text", x = 2.05, y = 0.727, label = "a") +
  annotate("text", x = 3.05, y = 0.618, label = "b") +
  annotate("text", x = 4.06, y = 0.568, label = "b")

# Combine and save plots --------------------------------------------------

plot_grid <- body_weight_plot /
  steps_plot /
  GRF_plot /
  sclerostin_plot +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 18, face = "bold"))

agg_tiff(
  here("figures/fig2.tiff"),
  width = 30, height = 80, units = "cm", res = 300
)
plot(plot_grid)
dev.off()
