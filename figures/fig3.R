# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(patchwork)
library(ragg)

# Read and prepare data ---------------------------------------------------

files <- list(
  MVPA = here("output/emm_MVPA_min_time.csv"),
  HI = here("output/emm_high_impacts_time.csv")
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

# HI plot -----------------------------------------------------------------

HI_plot <- ggplot(plot_data$HI) +
  geom_point(aes(x = time, y = emmean, size = 4)) +
  geom_line(aes(x = time, y = emmean, group = 1, size = 1)) +
  geom_errorbar(
    aes(x = time, ymin = lower.CL, ymax = upper.CL, size = 1, width = 0.1)
  ) +
  scale_y_continuous(
    breaks = seq(-2, 12, 2),
    expand = c(0, 0),
    limits = c(-2, 12.5)
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
    y = quote("High gravitational loading impacts" ~ (n %.% day^-1))
  )

# Combine and save plots --------------------------------------------------

plot_grid <- MVPA_plot /
  HI_plot /
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 18, face = "bold"))

agg_tiff(
  here("figures/fig3.tiff"),
  width = 30, height = 40, units = "cm", res = 300
)
plot(plot_grid)
dev.off()
