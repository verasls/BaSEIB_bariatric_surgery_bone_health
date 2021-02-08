# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(lmerTest)
library(emmeans)
library(lvmisc)
source(here("code/functions/utils.R"))

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

# Model body composition variables throughout time ------------------------

BC_vars <- c(
  "whole_body_total_mass", "whole_body_fat_mass",
  "whole_body_lean_mass", "whole_body_BMC"
)
BC_formula <- map(BC_vars, ~ as.formula(paste0(.x, " ~ time + (1 | subj)")))
BC_models <- map(BC_formula, lmer, data = df) %>% set_names(BC_vars)
BC_fixed_effects <- map(BC_models, anova, type = 3, test = "F")
BC_time_emm <- map(BC_models, ~ emmeans(.x, ~ time))
BC_pairwise <- map(BC_time_emm, pairs, adjust = "holm")
# Write estimated marginal means to a file
list(
  data = map(BC_time_emm, as.data.frame),
  name = map(BC_vars, ~ paste0("emm_", .x, "_time.csv"))
) %>%
  pwalk(write_output)

# Model physical activity variables throughout time -----------------------

PA_vars <- c("steps", "SB_h", "LPA_h", "MVPA_min")
PA_formula <- map(PA_vars, ~ as.formula(paste0(.x, " ~ time + (1 | subj)")))
PA_models <- map(PA_formula, lmer, data = df) %>% set_names(PA_vars)
PA_fixed_effects <- map(PA_models, anova, type = 3, test = "F")
PA_time_emm <- map(PA_models, ~ emmeans(.x, ~ time))
PA_pairwise <- map(PA_time_emm, pairs, adjust = "holm")
# Write estimated marginal means to a file
list(
  data = map(PA_time_emm, as.data.frame),
  name = map(PA_vars, ~ paste0("emm_", .x, "_time.csv"))
) %>%
  pwalk(write_output)

# Model gravitational loading variables throughout time -------------------

GL_vars <- c("gravitational_loading", "high_impacts")
GL_formula <- map(GL_vars, ~ as.formula(paste0(.x, " ~ time + (1 | subj)")))
GL_models <- map(GL_formula, lmer, data = df) %>% set_names(GL_vars)
GL_fixed_effects <- map(GL_models, anova, type = 3, test = "F")
GL_time_emm <- map(GL_models, ~ emmeans(.x, ~ time))
GL_pairwise <- map(GL_time_emm, pairs, adjust = "holm")
# Write estimated marginal means to a file
list(
  data = map(GL_time_emm, as.data.frame),
  name = map(GL_vars, ~ paste0("emm_", .x, "_time.csv"))
) %>%
  pwalk(write_output)

# Prediction of BMD changes -----------------------------------------------

# Scale and center variables
vars <- c(
  "TH_BMD", "FN_BMD", "LS_BMD",
  "whole_body_total_mass", "whole_body_fat_mass",
  "whole_body_lean_mass", "gravitational_loading"
)
centered_vars <- map(vars, ~ scale(df[[.x]])) %>%
  set_names(paste0(vars, "_centered")) %>%
  map(as.vector) %>%
  as_tibble
df <- cbind(df, centered_vars) %>% as_tibble

predictors <- c(
  "whole_body_total_mass_centered", "whole_body_fat_mass_centered",
  "whole_body_lean_mass_centered", "gravitational_loading_centered"
)

# Total hip
TH_formula <- map(
  predictors,
  ~ as.formula(paste0("TH_BMD_centered ~ ", .x, " + (1 | subj) + (1 | time)"))
)
TH_models <- map(TH_formula, lmer, data = df) %>%
  set_names(predictors)
TH_estimates <- map(TH_models, summary) %>% map(coefficients)
TH_r2 <- map(TH_models, r2)

# Femoral neck
FN_formula <- map(
  predictors,
  ~ as.formula(paste0("FN_BMD_centered ~ ", .x, " + (1 | subj) + (1 | time)"))
)
FN_models <- map(FN_formula, lmer, data = df) %>%
  set_names(predictors)
FN_estimates <- map(FN_models, summary) %>% map(coefficients)
FN_r2 <- map(FN_models, r2)

# Lumbar spine
LS_formula <- map(
  predictors,
  ~ as.formula(paste0("LS_BMD_centered ~ ", .x, " + (1 | subj) + (1 | time)"))
)
LS_models <- map(LS_formula, lmer, data = df) %>%
  set_names(predictors)
LS_estimates <- map(LS_models, summary) %>% map(coefficients)
LS_r2 <- map(LS_models, r2)
