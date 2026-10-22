library(tidyverse)

rm(list = ls())

# Purpose ------------------------------------------------------------------
# Validate a newly built single-year ACS states output before folding it into
# the full-series file.
#
# This script compares:
#   - current_file: final_acs_states_{data_year}.csv
#   - previous_file: final_acs_states_full_series_{data_year - 1}.csv
#   - national_file: final_acs_national_{data_year}.csv
#
# It checks state row uniqueness, prints selected high-value fields, compares
# estimate columns with the prior ACS release, flags unusually large percent
# changes, and compares state sums with the national file for selected
# additive variables. It does not modify any output files.

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
current_file <- acs_output_file(wd$data_output, "states", data_year)
previous_file <- acs_output_file(
  wd$data_output,
  "states",
  acs_series_start_year:(data_year - 1)
)
national_file <- acs_output_file(wd$data_output, "national", data_year)


# Read files ---------------------------------------------------------------
current_states <- read_csv(current_file, show_col_types = FALSE)
previous_states <- read_csv(previous_file, show_col_types = FALSE)
current_national <- read_csv(national_file, show_col_types = FALSE)

previous_last <- previous_states |>
  group_by(State_FIPS) |>
  filter(Year == max(Year, na.rm = TRUE)) |>
  ungroup()


# Basic structure checks ---------------------------------------------------
cat(
  "\nCurrent file:", current_file,
  "\nPrevious file:", previous_file,
  "\nNational file:", national_file,
  "\n\nNote: Year in ACS outputs is the ACS data year ",
  "(the end year of the 5-year period), not the publication year ",
  "or midpoint year.",
  "\n\nCurrent row count:", nrow(current_states),
  "\n"
)

duplicate_states <- current_states |>
  count(State_FIPS) |>
  filter(n > 1)

cat("\nDuplicate State_FIPS rows in current release\n")
print(duplicate_states, n = nrow(duplicate_states))

cat("\nCurrent Year/Source sample\n")
print(current_states |> select(Year, Source, State, State_FIPS, State_USPS) |> slice_head(n = 10))


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

cat("\nKey check values: current release sample\n")
print(
  current_states |>
    select(any_of(c("State", "State_FIPS", key_check_vars))) |>
    slice_head(n = 10)
)


# Percent-change comparison ------------------------------------------------
comparison <- previous_last |>
  select(any_of(c("State", "State_FIPS", all_of(key_check_vars))), everything()) |>
  full_join(
    current_states,
    by = "State_FIPS",
    suffix = c("_old", "_new")
  )

estimate_cols <- names(current_states)[stringr::str_detect(names(current_states), "_EST$")]

state_change_long <- map_dfr(estimate_cols, function(var) {
  old_col <- paste0(var, "_old")
  new_col <- paste0(var, "_new")
  
  tibble(
    State = comparison$State_new %||% comparison$State_old,
    State_FIPS = comparison$State_FIPS,
    variable = paste0(var, "_pct_change"),
    prev_release = comparison[[old_col]],
    curr_release = comparison[[new_col]],
    pct_change = if_else(
      is.na(comparison[[old_col]]) |
        comparison[[old_col]] %in% c(-888888888, -555555555) |
        is.na(comparison[[new_col]]) |
        comparison[[new_col]] %in% c(-888888888, -555555555) |
        comparison[[old_col]] == 0,
      NA_real_,
      (comparison[[new_col]] - comparison[[old_col]]) / comparison[[old_col]] * 100
    )
  )
}) |>
  filter(!is.na(pct_change))

key_check_changes <- state_change_long |>
  filter(variable %in% paste0(key_check_vars, "_pct_change"))

cat("\nLargest key check changes by state\n")
print(
  key_check_changes |>
    arrange(desc(abs(pct_change))) |>
    slice_head(n = 20),
  n = 20
)

big_changes <- state_change_long |>
  filter(
    prev_release >= 10,
    pct_change > 50 | pct_change < -20
  ) |>
  arrange(desc(abs(pct_change)))


big_changes_by_variable <- big_changes |>
  count(variable, sort = TRUE)

big_changes_by_state <- big_changes |>
  count(State, sort = TRUE)

big_changes_by_prefix <- big_changes |>
  mutate(prefix = sub("_.*", "", variable)) |>
  count(prefix, sort = TRUE)

cat("\nLarge changes by variable\n")
print(big_changes_by_variable, n = min(nrow(big_changes_by_variable), 30))

cat("\nLarge changes by state\n")
print(big_changes_by_state, n = min(nrow(big_changes_by_state), 30))

cat("\nLarge changes by variable prefix\n")
print(big_changes_by_prefix, n = nrow(big_changes_by_prefix))

cat("\nSample of large state-level percent changes with prev_release >= 10 (> 50% or < -20%)\n")
print(big_changes, n = min(nrow(big_changes), 50))


# State-to-national aggregation sanity ------------------------------------
aggregate_large_diff_threshold <- 1
aggregate_check_vars <- acs_additive_estimate_vars(
  current_states,
  current_national
)

state_sums <- current_states |>
  summarize(
    across(
      any_of(aggregate_check_vars),
      acs_aggregate_sum
    )
  )

national_row <- current_national |>
  slice_tail(n = 1)

aggregate_comparison <- tibble(
  variable = aggregate_check_vars,
  state_sum = map_dbl(aggregate_check_vars, ~ state_sums[[.x]]),
  national_value = map_dbl(aggregate_check_vars, ~ national_row[[.x]]),
  pct_diff = acs_aggregate_pct_diff(state_sum, national_value)
) |>
  filter(
    acs_valid_reported_value(national_value),
    !is.na(state_sum)
  ) |>
  arrange(desc(abs(pct_diff)))

aggregate_large_diff <- aggregate_comparison |>
  filter(abs(pct_diff) >= aggregate_large_diff_threshold) |>
  arrange(desc(abs(pct_diff)))

cat(
  "\nState-sum versus national comparison",
  "\nAdditive estimate variables checked:", length(aggregate_check_vars),
  "\nLarge discrepancies shown: abs(pct_diff) >=",
  aggregate_large_diff_threshold,
  "\n"
)
print(aggregate_large_diff, n = nrow(aggregate_large_diff))
