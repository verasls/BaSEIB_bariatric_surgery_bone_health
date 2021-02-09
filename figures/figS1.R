# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(patchwork)
library(ragg)

# Read and prepare data ---------------------------------------------------

files <- list(
  SB = here("output/emm_SB_h_time.csv"),
  LPA = here("output/emm_LPA_h_time.csv"),
  MVPA = here("output/emm_MVPA_min_time.csv")
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

# SB plot -----------------------------------------------------------------

SB_plot <- ggplot(plot_data$SB) +
  geom_point(aes(x = time, y = emmean, size = 4)) +
  geom_line(aes(x = time, y = emmean, group = 1, size = 1)) +
  geom_errorbar(
    aes(x = time, ymin = lower.CL, ymax = upper.CL, size = 1, width = 0.1)
  ) +
  scale_y_continuous(
    breaks = seq(5.5, 8.5, 0.5),
    expand = c(0, 0),
    limits = c(5.5, 8.7)
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
    y = quote("Sedentary behavior" ~ (h %.% day^-1))
  ) +
  annotate("text", x = 3.09, y = 6.82, label = "a b") +
  annotate("text", x = 4.09, y = 6.9, label = "a b")

# LPA plot ----------------------------------------------------------------

LPA_plot <- ggplot(plot_data$LPA) +
  geom_point(aes(x = time, y = emmean, size = 4)) +
  geom_line(aes(x = time, y = emmean, group = 1, size = 1)) +
  geom_errorbar(
    aes(x = time, ymin = lower.CL, ymax = upper.CL, size = 1, width = 0.1)
  ) +
  scale_y_continuous(
    breaks = seq(4, 8, 0.5),
    expand = c(0, 0),
    limits = c(4, 8.1)
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
    y = quote("Light physical activity" ~ (h %.% day^-1))
  ) +
  annotate("text", x = 3.04, y = 6.5, label = "b") +   
  annotate("text", x = 4.08, y = 6.97, label = "a b''")

# MVPA plot ---------------------------------------------------------------

MVPA_plot <- ggplot(plot_data$MVPA) +
  geom_point(aes(x = time, y = emmean, size = 4)) +
  geom_line(aes(x = time, y = emmean, group = 1, size = 1)) +
  geom_errorbar(
    aes(x = time, ymin = lower.CL, ymax = upper.CL, size = 1, width = 0.1)
  ) +
  scale_y_continuous(
    breaks = seq(10, 50, 5),
    expand = c(0, 0),
    limits = c(10, 51)
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
    y = quote("Moderate-to-vigorous physical activity" ~ (min %.% day^-1))
  ) +
  annotate("text", x = 4.08, y = 40.3, label = "a b")

# Combine and save plots --------------------------------------------------

plot_grid <- SB_plot /
  LPA_plot /
  MVPA_plot /
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 18, face = "bold"))

agg_tiff(
  here("figures/figS1.tiff"),
  width = 30, height = 60, units = "cm", res = 300
)
plot(plot_grid)
dev.off()
