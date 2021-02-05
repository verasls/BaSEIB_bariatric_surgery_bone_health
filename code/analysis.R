# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)

# Read data ---------------------------------------------------------------

df <- read_csv(here("data/df.csv")) %>%
  mutate(
    across(
      c(time, surgery, sex, diabetes, thiazides, smoker, menopause),
      as.factor
    )
  )

# Descriptive statistics --------------------------------------------------

df_baseline <- df %>%
  filter(time == 1) %>%
  select(age, sex, BMI, diabetes, thiazides, smoker, menopause)

# Numeric vars
df_baseline %>%
  summarise(
    across(where(is.double), list(mean = mean, sd = sd))
  )

# Categorical vars
cat_vars <- df_baseline %>%
  select(where(is.factor)) %>%
  names()

map(cat_vars, ~ prop.table(table(df_baseline[[.x]]))) %>%
  set_names(cat_vars)
