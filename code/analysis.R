# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(lmerTest)
library(emmeans)
source(here("code/functions/utils.R"))

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

# Model BMD change throughout time ----------------------------------------

BMD_vars <- c("pc_TH_BMD", "pc_FN_BMD", "pc_LS_BMD", "pc_TR_BMD")
BMD_formula <- map(BMD_vars, ~ as.formula(paste0(.x, " ~ time + (1 | subj)")))
BMD_models <- map(BMD_formula, lmer, data = df)  %>% set_names(BMD_vars)
BMD_fixed_effects <- map(BMD_models, anova, type = 3, test = "F")
BMD_time_emm <- map(BMD_models, ~ emmeans(.x, ~ time))
BMD_pairwise <- map(BMD_time_emm, pairs, adjust = "holm")
# Write estimated marginal means to a file
list(
  data = map(BMD_time_emm, as.data.frame),
  name = map(BMD_vars, ~ paste0("emm_", .x, "_time.csv"))
) %>%
  pwalk(write_output)

# Model biochemical variables throughout time -----------------------------

bio_vars <- c("PTH", "vitD", "sclerostin")
bio_formula <- map(bio_vars, ~ as.formula(paste0(.x, " ~ time + (1 | subj)")))
bio_models <- map(bio_formula, lmer, data = df) %>% set_names(bio_vars)
bio_fixed_effects <- map(bio_models, anova, type = 3, test = "F")
bio_time_emm <- map(bio_models, ~ emmeans(.x, ~ time))
bio_pairwise <- map(bio_time_emm, pairs, adjust = "holm")
# Write estimated marginal means to a file
list(
  data = map(bio_time_emm, as.data.frame),
  name = map(bio_vars, ~ paste0("emm_", .x, "_time.csv"))
) %>%
  pwalk(write_output)
