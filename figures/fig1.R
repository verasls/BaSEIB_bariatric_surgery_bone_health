# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(ragg)

# Read and prepare data ---------------------------------------------------

files <- list.files(here("output/"), full.names = TRUE)
files <- files[str_detect(files, "BMD")]

BMD_plot_data <- map_dfr(files, read_csv) %>%
  mutate(
    site = rep(str_extract(files, "[:alpha:]{2}_BMD"), each = 4),
    time = recode_factor(
      as.factor(time),
      "1" = "Pre-BS",
      "2" = "1-month post-BS",
      "3" = "6-months post-BS",
      "4" = "12-months post-BS"
    ),
    site = recode_factor(
      as.factor(site),
      "TH_BMD" = "Total hip",
      "FN_BMD" = "Femoral neck",
      "LS_BMD" = "Lumbar spine",
      "TR_BMD" = "1/3 radius"
    )
  )

# Plot --------------------------------------------------------------------

dodge <- position_dodge(0.3)

BMD_plot <- ggplot(BMD_plot_data) +
  geom_point(
    aes(x = time, y = emmean, shape = site),
    position = dodge, size = 4
  ) +
  geom_line(
    aes(x = time, y = emmean, linetype = site, group = site),
    position = dodge, size = 1
  ) +
  geom_errorbar(
    aes(x = time, ymin = lower.CL, ymax = upper.CL, group = site),
    position = dodge, size = 1, width = 0.1
  ) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_y_continuous(
    breaks = seq(-12, 3, 3),
    expand = c(0, 0),
    limits = c(-12, 3)
  ) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.position = "top",
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold")
  ) +
  labs(
    x = "",
    y = quote(
      "% Bone mineral density change during
      the first year after bariatric surgery"
    )
  ) +
  annotate("text", x = 2.925, y = - 1.200, label = "a'") +
  annotate("text", x = 2.925, y = - 1.700, label = "b'") +
  annotate("text", x = 3.940, y = - 5.500, label = "a''") +
  annotate("text", x = 3.940, y = - 6.000, label = "b''") +
  annotate("text", x = 3.940, y = - 6.450, label = "c''") +
  annotate("text", x = 2.010, y = - 2.200, label = "a") +
  annotate("text", x = 3.010, y = - 3.960, label = "a'") +
  annotate("text", x = 4.010, y = - 7.450, label = "a''") +
  annotate("text", x = 4.010, y = - 7.950, label = "b''") +
  annotate("text", x = 4.010, y = - 8.400, label = "c' ") +
  annotate("text", x = 3.080, y = - 1.500, label = "a'") +
  annotate("text", x = 3.080, y = - 2.000, label = "b'") +
  annotate("text", x = 4.090, y = - 3.950, label = "a''") +
  annotate("text", x = 4.090, y = - 4.450, label = "b''") +
  annotate("text", x = 4.090, y = - 4.900, label = "c' ")

# Save plot ---------------------------------------------------------------

agg_tiff(
  here("figures/fig1.tiff"),
  width = 30, height = 20, units = "cm", res = 300
)
plot(BMD_plot)
dev.off()
