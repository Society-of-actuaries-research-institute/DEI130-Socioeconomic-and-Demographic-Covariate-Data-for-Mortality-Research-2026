library(tidyverse)

rm(list = ls())

# Purpose ------------------------------------------------------------------
# Validate a newly built single-year ACS counties output before folding it
# into the full-series file.
#
# This script compares:
#   - current_file: final_acs_counties_{data_year}.csv
#   - previous_file: final_acs_counties_full_series_{data_year - 1}.csv
#   - states_file: final_acs_states_{data_year}.csv
#   - national_file: final_acs_national_{data_year}.csv
#
# It checks county row uniqueness and geography parsing, compares county and
# state labels with the prior ACS release, prints selected high-value fields,
# flags unusually large percent changes, and compares county sums with state
# and national files for selected additive variables. It does not modify any
# output files.

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
current_file <- acs_output_file(wd$data_output, "counties", data_year)
previous_file <- acs_output_file(
  wd$data_output,
  "counties",
  acs_series_start_year:(data_year - 1)
)
states_file <- acs_output_file(wd$data_output, "states", data_year)
national_file <- acs_output_file(wd$data_output, "national", data_year)


# Read files ---------------------------------------------------------------
current_counties <- read_csv(current_file, show_col_types = FALSE, progress = FALSE)
previous_counties <- read_csv(previous_file, show_col_types = FALSE, progress = FALSE)
current_states <- read_csv(states_file, show_col_types = FALSE)
current_national <- read_csv(national_file, show_col_types = FALSE)

# previous_last keeps the latest available row for each County_FIPS, so it can
# mix years when county-equivalent definitions change. Use it for value-change
# comparisons where each current FIPS should be compared with its latest prior
# observation.
previous_last <- previous_counties |>
  group_by(County_FIPS) |>
  filter(Year == max(Year, na.rm = TRUE)) |>
  ungroup()

# previous_release keeps only the immediately prior ACS release as a whole.
# Use it for release-to-release geography checks, such as added/removed FIPS
# codes and county/state name changes.
previous_release <- previous_counties |>
  filter(Year == max(Year, na.rm = TRUE))


# Basic structure checks ---------------------------------------------------
cat(
  "\nCurrent file:", current_file,
  "\nPrevious file:", previous_file,
  "\nStates file:", states_file,
  "\nNational file:", national_file,
  "\n\nNote: Year in ACS outputs is the ACS data year ",
  "(the end year of the 5-year period), not the publication year ",
  "or midpoint year.",
  "\n\nCurrent row count:", nrow(current_counties),
  "\nDistinct counties:", n_distinct(current_counties$County_FIPS),
  "\n"
)

duplicate_counties <- current_counties |>
  count(County_FIPS) |>
  filter(n > 1)

cat("\nDuplicate County_FIPS rows in current release\n")
print(duplicate_counties, n = nrow(duplicate_counties))

# Preview one county per state so we can quickly confirm Year/Source labels
# and basic county/state parsing across multiple geographies.
sample_counties_across_states <- current_counties |>
  arrange(State_FIPS, County_FIPS) |>
  group_by(State_FIPS) |>
  slice_head(n = 1) |>
  ungroup() |>
  slice_head(n = 10)

cat("\nCurrent Year/Source sample across states\n")
print(
  sample_counties_across_states |>
    select(Year, Source, State, State_FIPS, County, County_FIPS)
)

# Compare county/state labels between the current release and the immediately
# prior release in the existing full-series file. This includes both name
# changes on matched County_FIPS values and FIPS codes that only appear in one
# of the two releases.
county_name_changes <- previous_release |>
  select(
    Year_old = Year,
    County_FIPS,
    State_old = State,
    County_old = County
  ) |>
  inner_join(
    current_counties |>
      select(
        Year_new = Year,
        County_FIPS,
        State_new = State,
        County_new = County
      ),
    by = "County_FIPS"
  ) |>
  filter(
    State_old != State_new |
      County_old != County_new
  ) |>
  mutate(change_type = "name_changed_on_matched_fips") |>
  arrange(State_new, County_FIPS)

county_fips_removed <- previous_release |>
  select(
    Year_old = Year,
    County_FIPS,
    State_old = State,
    County_old = County
  ) |>
  anti_join(
    current_counties |>
      select(County_FIPS),
    by = "County_FIPS"
  ) |>
  mutate(
    Year_new = NA_real_,
    State_new = NA_character_,
    County_new = NA_character_,
    change_type = "fips_only_in_previous_release"
  )

county_fips_added <- current_counties |>
  select(
    Year_new = Year,
    County_FIPS,
    State_new = State,
    County_new = County
  ) |>
  anti_join(
    previous_release |>
      select(County_FIPS),
    by = "County_FIPS"
  ) |>
  mutate(
    Year_old = NA_real_,
    State_old = NA_character_,
    County_old = NA_character_,
    change_type = "fips_only_in_current_release"
  )

county_geography_changes <- bind_rows(
  county_name_changes,
  county_fips_removed,
  county_fips_added
) |>
  select(
    change_type,
    Year_old,
    Year_new,
    County_FIPS,
    State_old,
    County_old,
    State_new,
    County_new
  ) |>
  arrange(change_type, County_FIPS)

cat(
  "\nCounty geography/name changes across releases",
  "\nPrevious release year:", max(previous_release$Year, na.rm = TRUE),
  "\nCurrent release year:", max(current_counties$Year, na.rm = TRUE),
  "\n"
)
print(county_geography_changes, n = min(nrow(county_geography_changes), 50))


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
  sample_counties_across_states |>
    select(any_of(c("State", "County", "County_FIPS", key_check_vars))) |>
    slice_head(n = 10)
)


# Percent-change comparison ------------------------------------------------
# Compare each county in the new file with the latest available row for the
# same County_FIPS in the prior county output.
comparison <- previous_last |>
  select(any_of(c("State", "County", "County_FIPS", all_of(key_check_vars))), everything()) |>
  full_join(
    current_counties,
    by = "County_FIPS",
    suffix = c("_old", "_new")
  )

estimate_cols <- names(current_counties)[stringr::str_detect(names(current_counties), "_EST$")]

county_change_long <- map_dfr(estimate_cols, function(var) {
  old_col <- paste0(var, "_old")
  new_col <- paste0(var, "_new")

  tibble(
    State = comparison$State_new %||% comparison$State_old,
    County = comparison$County_new %||% comparison$County_old,
    County_FIPS = comparison$County_FIPS,
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

key_check_changes <- county_change_long |>
  filter(variable %in% paste0(key_check_vars, "_pct_change"))

cat("\nLargest key check changes by county\n")
print(
  key_check_changes |>
    arrange(desc(abs(pct_change))) |>
    slice_head(n = 20),
  n = 20
)

big_changes <- county_change_long |>
  filter(pct_change > 50 | pct_change < -20) |>
  arrange(desc(abs(pct_change)))

big_changes_by_variable <- big_changes |>
  count(variable, sort = TRUE)

big_changes_by_state <- big_changes |>
  count(State, sort = TRUE)

big_changes_by_county <- big_changes |>
  count(State, County, County_FIPS, sort = TRUE)

big_changes_by_prefix <- big_changes |>
  mutate(prefix = sub("_.*", "", variable)) |>
  count(prefix, sort = TRUE)

cat("\nLarge changes by variable\n")
print(big_changes_by_variable, n = min(nrow(big_changes_by_variable), 30))

cat("\nLarge changes by state\n")
print(big_changes_by_state, n = min(nrow(big_changes_by_state), 30))

cat("\nCounties with the most large changes\n")
print(big_changes_by_county, n = min(nrow(big_changes_by_county), 30))

cat("\nLarge changes by variable prefix\n")
print(big_changes_by_prefix, n = nrow(big_changes_by_prefix))

cat("\nSample of large county-level percent changes (> 50% or < -20%)\n")
print(big_changes, n = min(nrow(big_changes), 50))


# County-to-state aggregation sanity --------------------------------------
# Sum counties within each state and compare with the corresponding state file.
aggregate_large_diff_threshold <- 1
aggregate_check_vars <- acs_additive_estimate_vars(
  current_counties,
  current_states,
  current_national
)

county_state_sums <- current_counties |>
  group_by(State_FIPS) |>
  summarize(
    across(
      any_of(aggregate_check_vars),
      acs_aggregate_sum
    ),
    .groups = "drop"
  )

state_subset <- current_states |>
  select(any_of(c("State", "State_FIPS", aggregate_check_vars)))

county_to_state_comparison <- county_state_sums |>
  inner_join(state_subset, by = "State_FIPS", suffix = c("_county_sum", "_state")) |>
  pivot_longer(
    cols = -c(State_FIPS, State),
    names_to = c("variable", ".value"),
    names_pattern = "(.*)_(county_sum|state)"
  ) |>
  filter(
    acs_valid_reported_value(state),
    !is.na(county_sum)
  ) |>
  mutate(
    pct_diff = acs_aggregate_pct_diff(county_sum, state)
  ) |>
  arrange(desc(abs(pct_diff)))

county_to_state_large_diff <- county_to_state_comparison |>
  filter(abs(pct_diff) >= aggregate_large_diff_threshold) |>
  arrange(desc(abs(pct_diff)))

cat(
  "\nCounty-sum versus state comparison",
  "\nAdditive estimate variables checked:", length(aggregate_check_vars),
  "\nLarge discrepancies shown: abs(pct_diff) >=",
  aggregate_large_diff_threshold,
  "\n"
)
print(
  county_to_state_large_diff,
  n = min(nrow(county_to_state_large_diff), 50)
)


# County-to-national aggregation sanity -----------------------------------
# Sum all counties and compare with the national file as a coarse consistency check.
county_sums <- current_counties |>
  summarize(
    across(
      any_of(aggregate_check_vars),
      acs_aggregate_sum
    )
  )

national_row <- current_national |>
  slice_tail(n = 1)

county_to_national_comparison <- tibble(
  variable = aggregate_check_vars,
  county_sum = map_dbl(aggregate_check_vars, ~ county_sums[[.x]]),
  national_value = map_dbl(aggregate_check_vars, ~ national_row[[.x]]),
  pct_diff = acs_aggregate_pct_diff(county_sum, national_value)
) |>
  filter(
    acs_valid_reported_value(national_value),
    !is.na(county_sum)
  ) |>
  arrange(desc(abs(pct_diff)))

county_to_national_large_diff <- county_to_national_comparison |>
  filter(abs(pct_diff) >= aggregate_large_diff_threshold) |>
  arrange(desc(abs(pct_diff)))

cat(
  "\nCounty-sum versus national comparison",
  "\nLarge discrepancies shown: abs(pct_diff) >=",
  aggregate_large_diff_threshold,
  "\n"
)
print(county_to_national_large_diff)
