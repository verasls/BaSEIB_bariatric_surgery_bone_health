# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(gamlj)
library(lmerTest)
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

agg_tiff(
  here("figures/fig_sclerostin.tiff"),
  width = 30, height = 20, units = "cm", res = 300
)
plot(sclerostin_loading_plot)
dev.off()
