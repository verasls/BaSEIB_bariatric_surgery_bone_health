# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(ragg)

# Read and prepare data ---------------------------------------------------

files <- list(
  fat_mass = here("output/emm_whole_body_fat_mass_time.csv"),
  lean_mass = here("output/emm_whole_body_lean_mass_time.csv")
)

plot_data <- map_dfr(files, read_csv) %>%
  mutate(
    tissue = rep(c("Fat mass", "Lean mass"), each = 4)
  )

# Plot --------------------------------------------------------------------

BC_plot <- ggplot(plot_data) +
  geom_area(aes(x = time, y = emmean, fill = tissue)) +
  scale_x_continuous(
    breaks = 1:4,
    label = c(
      "Pre-BS",
      "1-month\npost-BS",
      "6-months\npost-BS",
      "12-months\npost-BS"
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, 120, 10),
    expand = c(0, 0),
    limits = c(0, 120)
  ) +
  scale_fill_manual(values = c("gray70", "gray40")) +
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
      "Body composition changes during the
           first year after bariatric surgery (kg)            "
    )
  ) +
  annotate("text", x = 1, y = 55.82401, label = "I") +
  annotate("text", x = 2, y = 49.40289, label = "I") +
  annotate("text", x = 3, y = 46.96103, label = "I") +
  annotate("text", x = 4, y = 45.91563, label = "I") +
  annotate("text", x = 1, y = 111.1138, label = "I") +
  annotate("text", x = 2, y = 99.16971, label = "I") +
  annotate("text", x = 3, y = 78.48622, label = "I") +
  annotate("text", x = 4, y = 69.77669, label = "I") +
  annotate("text", x = 2.01, y = 54, label = "a''") +
  annotate("text", x = 3, y = 51, label = "a'' b'")  +
  annotate("text", x = 3.995, y = 50, label = "a'' b''") +
  annotate("text", x = 2.01, y = 103, label = "a''") +
  annotate("text", x = 3.01, y = 82, label = "a'' b''")  +
  annotate("text", x = 3.995, y = 75, label = "a'' b'' c''")

# Save plot ---------------------------------------------------------------

agg_tiff(
  here("figures/fig4.tiff"),
  width = 30, height = 20, units = "cm", res = 300
)
plot(BC_plot)
dev.off()
