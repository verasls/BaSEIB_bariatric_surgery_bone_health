# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(gamlj)
library(patchwork)
library(ragg)

# Read data ---------------------------------------------------------------

df <- read_csv(here("data/df.csv")) %>%
  mutate(
    across(
      c(time, surgery, sex, diabetes, thiazides, smoker, menopause),
      as.factor
    )
  )

# TH by body weight plot --------------------------------------------------

TH_weight_plot <- gamljMixed(
  formula = TH_BMD ~ 1 + whole_body_total_mass + (1 | subj) + (1 | time),
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

TH_weight_plot <- TH_weight_plot +
  theme(
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold")
  ) +
  labs(
    x = "Weight (kg)",
    y = quote("Total hip bone mineral density" ~ (g %.% cm^-2))
  ) +
  annotate(
    "text", x = 50, y = 1.3,
    label = "BMD = 0.903 + 0.0013 * weight",
    hjust = 0
  ) +
  annotate(
    "text", x = 50, y = 1.28,
    label = "paste(italic(p) == .026, \"; \", italic(R)^2 == 0.06)",
    hjust = 0,
    parse = TRUE
  )

# TH by fat mass plot -----------------------------------------------------

TH_fat_mass_plot <- gamljMixed(
  formula = TH_BMD ~ 1 + whole_body_fat_mass + (1 | subj) + (1 | time),
  data = df,
  showContrastCode = TRUE,
  plotHAxis = whole_body_fat_mass,
  plotRaw = TRUE,
  plotError = "ci",
  eDesc = TRUE,
  simpleVariable = whole_body_fat_mass,
  scaling = list(
    list(
      var = "whole_body_fat_mass",
      type = "none")),
  lrtRandomEffects = TRUE,
  plotRandomEffects = TRUE) %>%
  gamlj_ggplot()

TH_fat_mass_plot <- TH_fat_mass_plot +
  theme(
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold")
  ) +
  labs(
    x = "Fat mass (kg)",
    y = quote("Total hip bone mineral density" ~ (g %.% cm^-2))
  ) +
  annotate(
    "text", x = 15, y = 1.3,
    label = "BMD = 0.952 + 0.0017 * fat mass",
    hjust = 0
  ) +
  annotate(
    "text", x = 15, y = 1.28,
    label = "paste(italic(p) == .031, \"; \", italic(R)^2 == 0.05)",
    hjust = 0,
    parse = TRUE
  )

# FN by body weight plot --------------------------------------------------

FN_weight_plot <- gamljMixed(
  formula = FN_BMD ~ 1 + whole_body_total_mass + (1 | subj) + (1 | time),
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

FN_weight_plot <- FN_weight_plot +
  theme(
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold")
  ) +
  labs(
    x = "Weight (kg)",
    y = quote("Femoral neck bone mineral density" ~ (g %.% cm^-2))
  ) +
  annotate(
    "text", x = 50, y = 1.2,
    label = "BMD = 0.752 + 0.0015 * weight",
    hjust = 0
  ) +
  annotate(
    "text", x = 50, y = 1.18,
    label = "paste(italic(p) == .022, \"; \", italic(R)^2 == 0.12)",
    hjust = 0,
    parse = TRUE
  )

# FN by lean mass plot ----------------------------------------------------

FN_lean_mass_plot <- gamljMixed(
  formula = FN_BMD ~ 1 + whole_body_lean_mass + (1 | subj) + (1 | time),
  data = df,
  showContrastCode = TRUE,
  plotHAxis = whole_body_lean_mass,
  plotRaw = TRUE,
  plotError = "ci",
  eDesc = TRUE,
  simpleVariable = whole_body_lean_mass,
  scaling = list(
    list(
      var = "whole_body_lean_mass",
      type = "none")),
  lrtRandomEffects = TRUE,
  plotRandomEffects = TRUE) %>%
  gamlj_ggplot()

FN_lean_mass_plot <- FN_lean_mass_plot +
  theme(
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold")
  ) +
  labs(
    x = "Lean mass (kg)",
    y = quote("Femoral neck bone mineral density" ~ (g %.% cm^-2))
  ) +
  annotate(
    "text", x = 34, y = 1.2,
    label = "BMD = 0.730 + 0.0033 * lean mass",
    hjust = 0
  ) +
  annotate(
    "text", x = 34, y = 1.18,
    label = "paste(italic(p) == .010, \"; \", italic(R)^2 == 0.14)",
    hjust = 0,
    parse = TRUE
  )

# Combine and save plots --------------------------------------------------

plot_grid <- TH_weight_plot +
  TH_fat_mass_plot +
  FN_weight_plot +
  FN_lean_mass_plot +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 18, face = "bold"))

agg_tiff(
  here("figures/figS2.tiff"),
  width = 60, height = 40, units = "cm", res = 300
)
plot(plot_grid)
dev.off()
