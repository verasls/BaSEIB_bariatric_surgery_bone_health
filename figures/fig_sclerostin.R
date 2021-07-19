# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(gamlj)
library(lmerTest)
library(patchwork)
library(ragg)

# Read data ---------------------------------------------------------------

df <- read_csv(here("data/df.csv")) %>%
  mutate(
    across(
      c(time, surgery, sex, diabetes, thiazides, smoker, menopause),
      as.factor
    ),
    # Transform from N to kN
    gravitational_loading = gravitational_loading / 1000
  )

# Sclerostin by gravitational loading plot --------------------------------

sclerostin_loading_plot <- gamljMixed(
  formula = sclerostin ~ 1 + gravitational_loading + (1 | subj) + (1 | time),
  data = df,
  showContrastCode = TRUE,
  plotHAxis = gravitational_loading,
  plotRaw = TRUE,
  plotError = "ci",
  eDesc = TRUE,
  simpleVariable = gravitational_loading,
  scaling = list(
    list(
      var = "gravitational_loading",
      type = "none")),
  lrtRandomEffects = TRUE,
  plotRandomEffects = TRUE) %>%
  gamlj_ggplot()

sclerostin_loading_plot <- sclerostin_loading_plot +
  theme(
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold")
  ) +
  labs(
    x = quote("Volume of ambulatory gravitational loading" ~ (kN %.% day^-1)),
    y = quote("Sclerostin" ~ ( ng %.% mL^-1))
  ) +
  annotate(
    "text", x = 500, y = 1.2,
    label = "Sclerostin = 0.666 - 4.736e-6 * gravitational loading",
    hjust = 0
  ) +
  annotate(
    "text", x = 500, y = 1.16,
    label = "paste(italic(p) == .486, \"; \", italic(R)^2 == 0.01)",
    hjust = 0,
    parse = TRUE
  )

# Sclerostin by body mass plot --------------------------------------------

sclerostin_body_mass_plot <- gamljMixed(
  formula = sclerostin ~ 1 + whole_body_total_mass + (1 | subj) + (1 | time),
  data = df,
  showContrastCode = TRUE,
  plotHAxis = whole_body_total_mass,
  plotRaw = TRUE,
  plotError = "ci",
  eDesc = TRUE,
  simpleVariable = whole_body_total_mass,
  scaling = list(
    list(
      var = "whole_body_total_mass",
      type = "none")),
  lrtRandomEffects = TRUE,
  plotRandomEffects = TRUE) %>%
  gamlj_ggplot()

sclerostin_body_mass_plot <- sclerostin_body_mass_plot +
  theme(
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold")
  ) +
  labs(
    x = quote("Weight" ~ (kg)),
    y = quote("Sclerostin" ~ ( ng %.% mL^-1))
  ) +
  annotate(
    "text", x = 50, y = 1.2,
    label = "Sclerostin = 0.653 - 3.764e-4 * weight",
    hjust = 0
  ) +
  annotate(
    "text", x = 50, y = 1.16,
    label = "paste(italic(p) == .828, \"; \", italic(R)^2 == \"0.00\")",
    hjust = 0,
    parse = TRUE
  )

# Combine and save plots --------------------------------------------------

plot_grid <- sclerostin_loading_plot +
  sclerostin_body_mass_plot +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 18, face = "bold"))

agg_tiff(
  here("figures/fig_sclerostin.tiff"),
  width = 60, height = 20, units = "cm", res = 300
)
plot(plot_grid)
dev.off()
