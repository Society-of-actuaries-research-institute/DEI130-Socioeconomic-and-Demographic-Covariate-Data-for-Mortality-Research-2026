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

# File to validate ---------------------------------------------------------
target_file <- acs_output_file(wd$data_output, "states", acs_series_years)
national_file <- acs_output_file(wd$data_output, "national", acs_series_years)


# Read data ----------------------------------------------------------------
acs_states <- read_csv(target_file, show_col_types = FALSE)
acs_national <- read_csv(national_file, show_col_types = FALSE)


# Basic structure checks ---------------------------------------------------
cat(
  "\nTarget file:", target_file,
  "\nNational file:", national_file,
  "\nRows:", nrow(acs_states),
  "\nDistinct states:", n_distinct(acs_states$State_FIPS),
  "\nDistinct years:", n_distinct(acs_states$Year),
  "\n"
)

duplicate_keys <- acs_states |>
  count(Year, State_FIPS) |>
  filter(n > 1)

cat("\nDuplicate Year-State_FIPS rows\n")
print(duplicate_keys, n = nrow(duplicate_keys))


# Percent-change scan within each state -----------------------------------
series_changes <- find_acs_series_changes(
  acs_states,
  group_cols = "State_FIPS",
  id_cols = c("Year", "Source", "State", "State_FIPS"),
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

big_changes_by_prefix <- big_changes |>
  mutate(prefix = sub("_.*", "", variable)) |>
  count(prefix, sort = TRUE)


# Console output -----------------------------------------------------------
cat("\nLarge changes by variable\n")
print(big_changes_by_variable, n = min(nrow(big_changes_by_variable), 30))

cat("\nLarge changes by state\n")
print(big_changes_by_state, n = min(nrow(big_changes_by_state), 30))

cat("\nLarge changes by variable prefix\n")
print(big_changes_by_prefix, n = nrow(big_changes_by_prefix))

cat("\nSample of state-level large percent changes with prev_release >= 10 (> 100% or < -20%)\n")
print(big_changes, n = min(nrow(big_changes), 50))

cat("\nState-level variables with zero percent change\n")
print(zero_changes, n = min(nrow(zero_changes), 100))


# State-to-national aggregation sanity ------------------------------------
aggregate_large_diff_threshold <- 1
aggregate_check_vars <- acs_additive_estimate_vars(acs_states, acs_national)

state_sums <- acs_states |>
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

aggregate_comparison <- state_sums |>
  inner_join(national_subset, by = "Year", suffix = c("_state_sum", "_national")) |>
  pivot_longer(
    cols = -Year,
    names_to = c("variable", ".value"),
    names_pattern = "(.*)_(state_sum|national)"
  ) |>
  filter(
    acs_valid_reported_value(national),
    !is.na(state_sum)
  ) |>
  mutate(
    pct_diff = acs_aggregate_pct_diff(state_sum, national)
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
