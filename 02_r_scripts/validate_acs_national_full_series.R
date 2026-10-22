library(tidyverse)

rm(list = ls())

# Project paths ------------------------------------------------------------
wd <- list(
  project = getwd(),
  data_input = file.path(getwd(), "00_data_input"),
  data_output = file.path(getwd(), "01_data_output"),
  plots = file.path(getwd(), "03_plots"),
  r_scripts = file.path(getwd(), "02_r_scripts")
)

dir.create(wd$plots, recursive = TRUE, showWarnings = FALSE)

source(file.path(wd$r_scripts, "acs_release_config.R"))
source(file.path(wd$r_scripts, "functions.R"))

# File to validate ---------------------------------------------------------
target_file <- acs_output_file(wd$data_output, "national", acs_series_years)


# Read data ----------------------------------------------------------------
acs_national <- read_csv(target_file, show_col_types = FALSE)


# Percent-change scan across all rows -------------------------------------
sentinel_values <- c(-888888888, -555555555)

check_tb_long <- acs_national |>
  arrange(Year) |>
  select(any_of(c("Year", "Source")), ends_with("_EST")) |>
  pivot_longer(
    cols = ends_with("_EST"),
    names_to = "variable",
    values_to = "curr_release"
  ) |>
  group_by(variable) |>
  mutate(
    prev_release = lag(curr_release),
    pct_change = if_else(
      curr_release %in% sentinel_values |
        prev_release %in% sentinel_values |
        is.na(prev_release) |
        prev_release == 0,
      NA_real_,
      (curr_release - prev_release) / prev_release * 100
    ),
    variable = paste0(variable, "_pct_change")
  ) |>
  ungroup() |>
  filter(!is.na(pct_change))

big_changes <- check_tb_long |>
  filter(pct_change > 50 | pct_change < -20) |>
  arrange(desc(abs(pct_change)))

zero_changes <- check_tb_long |>
  filter(pct_change == 0)


# Console output -----------------------------------------------------------
cat(
  "\nTarget file:", target_file,
  "\nRows:", nrow(acs_national),
  "\n"
)

cat("\nVariables with large percent changes (> 50% or < -20%)\n")
print(big_changes, n = nrow(big_changes))

cat("\nVariables with zero percent change\n")
print(zero_changes, n = nrow(zero_changes))


# Histogram ----------------------------------------------------------------

write_csv(
  check_tb_long,
  file.path(wd$plots, "acs_national_pct_change_distribution_raw_data.csv")
)

pct_change_histogram <- ggplot(check_tb_long, aes(x = pct_change)) +
  geom_vline(xintercept = 0, linewidth = 0.4, color = "gray45") +
  geom_histogram(
    bins = 30,
    fill = "#2F6F9F",
    color = "white",
    linewidth = 0.25,
    alpha = 0.9
  ) +
  labs(
    x = "Percent change (%)",
    y = "Number of estimate-year changes"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "gray15"),
    plot.subtitle = element_text(size = 11, color = "gray35"),
    plot.caption = element_text(size = 9, color = "gray40", hjust = 0),
    axis.title = element_text(face = "bold", color = "gray20"),
    axis.text = element_text(color = "gray25"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray88", linewidth = 0.3),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

print(pct_change_histogram)

ggsave(
  filename = file.path(wd$plots, "acs_national_pct_change_distribution.png"),
  plot = pct_change_histogram,
  width = 7,
  height = 4.5,
  dpi = 300,
  bg = "white"
)

check_tb_long_without_tmr_discontinuity <- check_tb_long |>
  filter(
    !(Year %in% c(2020, 2021) & str_starts(variable, "TMR_")),
    variable != "TwoRaces_Hisp_Pop_EST_pct_change"
  )

write_csv(
  check_tb_long_without_tmr_discontinuity,
  file.path(wd$plots, "acs_national_pct_change_distribution_without_tmr_discontinuity_raw_data.csv")
)

pct_change_histogram_without_tmr_discontinuity <- ggplot(
  check_tb_long_without_tmr_discontinuity,
  aes(x = pct_change)
) +
  geom_vline(xintercept = 0, linewidth = 0.4, color = "gray45") +
  geom_histogram(
    bins = 30,
    fill = "#2F6F9F",
    color = "white",
    linewidth = 0.25,
    alpha = 0.9
  ) +
  labs(
    x = "Percent change (%)",
    y = "Number of estimate-year changes"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "gray15"),
    plot.subtitle = element_text(size = 11, color = "gray35"),
    plot.caption = element_text(size = 9, color = "gray40", hjust = 0),
    axis.title = element_text(face = "bold", color = "gray20"),
    axis.text = element_text(color = "gray25"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray88", linewidth = 0.3),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

print(pct_change_histogram_without_tmr_discontinuity)

ggsave(
  filename = file.path(wd$plots, "acs_national_pct_change_distribution_without_tmr_discontinuity.png"),
  plot = pct_change_histogram_without_tmr_discontinuity,
  width = 7,
  height = 4.5,
  dpi = 300,
  bg = "white"
)
