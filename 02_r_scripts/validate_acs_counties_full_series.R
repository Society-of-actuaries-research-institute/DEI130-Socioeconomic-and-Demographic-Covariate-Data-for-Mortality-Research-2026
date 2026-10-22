library(tidyverse)

rm(list = ls())

# Project paths ------------------------------------------------------------
wd <- list(
  project = getwd(),
  data_input = file.path(getwd(), "00_data_input"),
  data_output = file.path(getwd(), "01_data_output"),
  r_scripts = file.path(getwd(), "02_r_scripts")
)

source(file.path(wd$r_scripts, "acs_release_config.R"))
source(file.path(wd$r_scripts, "functions.R"))

# Files to validate --------------------------------------------------------
target_file <- acs_output_file(wd$data_output, "counties", acs_series_years)
states_file <- acs_output_file(wd$data_output, "states", acs_series_years)
national_file <- acs_output_file(wd$data_output, "national", acs_series_years)


# Read data ----------------------------------------------------------------
acs_counties <- read_csv(target_file, show_col_types = FALSE, progress = FALSE)
acs_states <- read_csv(states_file, show_col_types = FALSE)
acs_national <- read_csv(national_file, show_col_types = FALSE)


# Basic structure checks ---------------------------------------------------
cat(
  "\nTarget file:", target_file,
  "\nStates file:", states_file,
  "\nNational file:", national_file,
  "\nRows:", nrow(acs_counties),
  "\nDistinct counties:", n_distinct(acs_counties$County_FIPS),
  "\nDistinct years:", n_distinct(acs_counties$Year),
  "\n"
)

duplicate_keys <- acs_counties |>
  count(Year, County_FIPS) |>
  filter(n > 1)

cat("\nDuplicate Year-County_FIPS rows\n")
print(duplicate_keys, n = nrow(duplicate_keys))

# County geography changes across ACS releases ----------------------------
# Track added/removed County_FIPS values and name changes from one ACS data
# year to the next. This explains why the full series can contain more unique
# county/county-equivalent FIPS codes than the latest release alone.
county_years <- sort(unique(acs_counties$Year))
latest_counties <- acs_counties |>
  filter(Year == max(Year, na.rm = TRUE))

county_geography_changes <- map_dfr(county_years[-1], function(current_year) {
  previous_year <- max(county_years[county_years < current_year])
  
  previous_counties <- acs_counties |>
    filter(Year == previous_year) |>
    select(County_FIPS, State_old = State, County_old = County)
  
  current_counties <- acs_counties |>
    filter(Year == current_year) |>
    select(County_FIPS, State_new = State, County_new = County)
  
  county_fips_added <- current_counties |>
    anti_join(
      previous_counties |> select(County_FIPS),
      by = "County_FIPS"
    ) |>
    mutate(
      change_type = "fips_only_in_current_release",
      from_year = previous_year,
      to_year = current_year,
      effective_release_year = current_year,
      State_old = NA_character_,
      County_old = NA_character_
    )
  
  county_fips_removed <- previous_counties |>
    anti_join(
      current_counties |> select(County_FIPS),
      by = "County_FIPS"
    ) |>
    mutate(
      change_type = "fips_only_in_previous_release",
      from_year = previous_year,
      to_year = current_year,
      effective_release_year = current_year,
      State_new = NA_character_,
      County_new = NA_character_
    )
  
  county_name_changes <- previous_counties |>
    inner_join(current_counties, by = "County_FIPS") |>
    filter(
      State_old != State_new |
        County_old != County_new
    ) |>
    mutate(
      change_type = "name_changed_on_matched_fips",
      from_year = previous_year,
      to_year = current_year,
      effective_release_year = current_year
    )
  
  bind_rows(
    county_fips_added,
    county_fips_removed,
    county_name_changes
  ) |>
    select(
      change_type,
      effective_release_year,
      from_year,
      to_year,
      County_FIPS,
      State_old,
      County_old,
      State_new,
      County_new
    )
}) |>
  arrange(from_year, change_type, County_FIPS)

historical_counties_not_latest <- acs_counties |>
  distinct(County_FIPS, State, County) |>
  anti_join(
    latest_counties |> distinct(County_FIPS),
    by = "County_FIPS"
  ) |>
  arrange(State, County_FIPS)

cat(
  "\nCounty geography changes across ACS releases",
  "\nDistinct County_FIPS in full series:", n_distinct(acs_counties$County_FIPS),
  "\nDistinct County_FIPS in latest release:",
  n_distinct(latest_counties$County_FIPS),
  "\nHistorical County_FIPS not in latest release:",
  n_distinct(historical_counties_not_latest$County_FIPS),
  "\n"
)

cat("\nCounty geography changes by transition/state/type\n")
print(
  county_geography_changes |>
    mutate(State = coalesce(State_new, State_old)) |>
    count(from_year, to_year, State, change_type, sort = FALSE) |>
    arrange(from_year, State, change_type),
  n = nrow(county_geography_changes)
)

cat("\nCounty geography change details\n")
print(county_geography_changes, n = nrow(county_geography_changes))

# Preview one county per state so we can quickly confirm the long file has -----
# sensible geography parsing and ACS year/source labels.
sample_counties_across_states <- acs_counties |>
  arrange(Year, State_FIPS, County_FIPS) |>
  group_by(State_FIPS) |>
  slice_head(n = 1) |>
  ungroup() |>
  slice_head(n = 10)

cat("\nCounty sample across states\n")
print(
  sample_counties_across_states |>
    select(Year, Source, State, State_FIPS, County, County_FIPS)
)


# Percent-change scan within each county ----------------------------------
series_changes <- find_acs_series_changes(
  acs_counties,
  group_cols = "County_FIPS",
  id_cols = c("Year", "Source", "State", "State_FIPS", "County", "County_FIPS"),
  large_increase_threshold = 100,
  large_decrease_threshold = -20,
  min_prev_release = 10
)

big_changes <- series_changes$big_changes
zero_changes <- series_changes$zero_changes

big_changes_by_variable <- big_changes |>
  count(variable, sort = TRUE)

big_changes_by_state <- big_changes |>
  count(State, sort = TRUE)

big_changes_by_county <- big_changes |>
  count(State, County, County_FIPS, sort = TRUE)

big_changes_by_prefix <- big_changes |>
  mutate(prefix = sub("_.*", "", variable)) |>
  count(prefix, sort = TRUE)


# Console output -----------------------------------------------------------
cat("\nLarge changes by variable\n")
print(big_changes_by_variable, n = min(nrow(big_changes_by_variable), 30))

cat("\nLarge changes by state\n")
print(big_changes_by_state, n = min(nrow(big_changes_by_state), 30))

cat("\nCounties with the most large changes\n")
print(big_changes_by_county, n = min(nrow(big_changes_by_county), 30))

cat("\nLarge changes by variable prefix\n")
print(big_changes_by_prefix, n = nrow(big_changes_by_prefix))

cat("\nSample of county-level large percent changes with prev_release >= 10 (> 100% or < -20%)\n")
print(big_changes, n = min(nrow(big_changes), 50))

cat("\nCounty-level variables with zero percent change\n")
print(zero_changes, n = min(nrow(zero_changes), 100))


# County-to-state aggregation sanity --------------------------------------
# Sum counties within each state-year and compare with the state file.
aggregate_large_diff_threshold <- 1
aggregate_check_vars <- acs_additive_estimate_vars(
  acs_counties,
  acs_states,
  acs_national
)

county_state_sums <- acs_counties |>
  group_by(Year, State_FIPS) |>
  summarize(
    across(
      any_of(aggregate_check_vars),
      acs_aggregate_sum
    ),
    .groups = "drop"
  )

state_subset <- acs_states |>
  select(any_of(c("Year", "State", "State_FIPS", aggregate_check_vars)))

county_to_state_comparison <- county_state_sums |>
  inner_join(state_subset, by = c("Year", "State_FIPS"), suffix = c("_county_sum", "_state")) |>
  pivot_longer(
    cols = -c(Year, State_FIPS, State),
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
# Sum all counties within each year and compare with the national file.
county_sums <- acs_counties |>
  group_by(Year) |>
  summarize(
    across(
      any_of(aggregate_check_vars),
      acs_aggregate_sum
    ),
    .groups = "drop"
  )

national_subset <- acs_national |>
  select(any_of(c("Year", aggregate_check_vars)))

county_to_national_comparison <- county_sums |>
  inner_join(national_subset, by = "Year", suffix = c("_county_sum", "_national")) |>
  pivot_longer(
    cols = -Year,
    names_to = c("variable", ".value"),
    names_pattern = "(.*)_(county_sum|national)"
  ) |>
  filter(
    acs_valid_reported_value(national),
    !is.na(county_sum)
  ) |>
  mutate(
    pct_diff = acs_aggregate_pct_diff(county_sum, national)
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
print(county_to_national_large_diff, n = nrow(county_to_national_large_diff))
