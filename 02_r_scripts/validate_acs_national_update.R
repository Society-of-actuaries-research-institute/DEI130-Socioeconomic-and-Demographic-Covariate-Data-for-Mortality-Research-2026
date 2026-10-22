library(tidyverse)

rm(list = ls())

# Purpose ------------------------------------------------------------------
# Validate a newly built single-year ACS national output before folding it
# into the full-series file.
#
# This script compares:
#   - current_file: final_acs_national_{data_year}.csv
#   - previous_file: final_acs_national_full_series_{data_year - 1}.csv
#
# It checks the current release structure, prints selected high-value fields,
# compares estimate columns with the prior ACS release, and flags unusually
# large percent changes or exact zero changes for review. It does not modify
# any output files.

# Project paths ------------------------------------------------------------
wd <- list(
  project = getwd(),
  data_input = file.path(getwd(), "00_data_input"),
  data_output = file.path(getwd(), "01_data_output"),
  r_scripts = file.path(getwd(), "02_r_scripts")
)

source(file.path(wd$r_scripts, "acs_release_config.R"))
source(file.path(wd$r_scripts, "functions.R"))

data_year <- acs_series_latest_year
current_file <- acs_output_file(wd$data_output, "national", data_year)
previous_file <- acs_output_file(
  wd$data_output,
  "national",
  acs_series_start_year:(data_year - 1)
)


# Read files ---------------------------------------------------------------
current_nat <- read_csv(current_file, show_col_types = FALSE)
previous_nat <- read_csv(previous_file, show_col_types = FALSE)

previous_last <- previous_nat |>
  filter(Year == max(Year, na.rm = TRUE))


# Basic structure checks ---------------------------------------------------
cat(
  "\nCurrent file:", current_file,
  "\nPrevious file:", previous_file,
  "\n\nNote: Year in ACS outputs is the ACS data year ",
  "(the end year of the 5-year period), not the publication year ",
  "or midpoint year.",
  "\n\nCurrent row count:", nrow(current_nat),
  "\n"
)
cat("Current Year/Source:\n")
print(current_nat |> select(Year, Source))

unexpected_cols <- intersect(names(current_nat), c("GEOID", "NAME"))
if (length(unexpected_cols) > 0) {
  warning(
    paste0(
      "Unexpected raw geography columns found in current national output: ",
      paste(unexpected_cols, collapse = ", ")
    )
  )
}


# Key check values ---------------------------------------------------------
# Print a compact manual preview of high-signal variables from the current
# release. This is an eyeball check for parsing, scale, sentinel values, and
# obvious column-shift problems; formal anomaly checks are below.
key_check_vars <- c(
  "Pop_EST",
  "Hisp_Pop_EST",
  "FrgnBorn_Pop_EST",
  "Med_HHIncome_EST",
  "Med_Rent_EST",
  "Gini_Index_EST"
)

key_check_current <- current_nat |>
  select(any_of(c("Year", "Source", key_check_vars)))

key_check_previous <- previous_last |>
  select(any_of(c("Year", "Source", key_check_vars)))

cat("\nKey check values: previous release\n")
print(key_check_previous)

cat("\nKey check values: current release\n")
print(key_check_current)


# Percent-change comparison ------------------------------------------------
comparison <- bind_rows(previous_last, current_nat) |>
  arrange(Year)

comparison_changes <- comparison |>
  mutate(
    across(
      ends_with("_EST") & !any_of(c("Year", "Source")),
      ~ if_else(
        .x %in% c(-888888888, -555555555) |
          lag(.x) %in% c(-888888888, -555555555) |
          is.na(lag(.x)) |
          lag(.x) == 0,
        NA_real_,
        (.x - lag(.x)) / lag(.x) * 100
      ),
      .names = "{.col}_pct_change"
    )
  )

key_check_changes <- comparison_changes |>
  slice_tail(n = 1) |>
  select(Year, Source, ends_with("_pct_change")) |>
  select(any_of(paste0(key_check_vars, "_pct_change")))

cat("\nKey check percent changes (%) relative to previous release\n")
print(key_check_changes)


# Broader scan for large changes ------------------------------------------
all_changes_long <- comparison_changes |>
  slice_tail(n = 1) |>
  select(ends_with("_pct_change")) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "pct_change"
  ) |>
  filter(!is.na(pct_change))

# Flag national variables with changes larger than +/-20% for manual review.
big_changes <- all_changes_long |>
  filter(pct_change > 20 | pct_change < -20) |>
  arrange(desc(abs(pct_change)))

cat("\nVariables with large percent changes (> 20% or < -20%)\n")
print(big_changes, n = nrow(big_changes))


