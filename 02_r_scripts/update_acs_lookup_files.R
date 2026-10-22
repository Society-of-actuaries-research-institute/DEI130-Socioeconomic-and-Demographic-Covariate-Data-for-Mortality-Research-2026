library(tidyverse)
library(tidycensus)

rm(list = ls())

# Refresh local ACS 5-year variable lookup files from tidycensus/Census.
#
# Run this after updating acs_series_latest_year in acs_release_config.R, or
# whenever Census variable metadata needs to be refreshed. The lookup CSVs are
# used by the ACS build/mapping scripts to translate project variables to the
# correct Census variable IDs for each ACS data year.

# Project paths ------------------------------------------------------------
wd <- list(
  project = getwd(),
  data_input = file.path(getwd(), "00_data_input"),
  data_output = file.path(getwd(), "01_data_output"),
  r_scripts = file.path(getwd(), "02_r_scripts")
)

source(file.path(wd$r_scripts, "acs_release_config.R"))


# Load API key -------------------------------------------------------------
# Expects a one-value CSV containing the Census API key. tidycensus stores the
# key for the current R session so load_variables() can query Census metadata.
census_data_api <- read_csv(
  file.path(wd$data_input, "census_data_api.csv"),
  col_names = FALSE,
  show_col_types = FALSE
) |>
  pull()

census_api_key(census_data_api)


# Save ACS variable lookup files ------------------------------------------
# acs_lookup_years is defined in acs_release_config.R and usually tracks the
# full ACS data-year series. Each output file is named acs5_{year}.csv.
walk(
  acs_lookup_years,
  ~ load_variables(.x, "acs5") |>
    write_csv(file.path(wd$data_input, "acs_lookup", paste0("acs5_", .x, ".csv")))
)

cat("Refreshed ACS lookup files for years:", paste(acs_lookup_years, collapse = ", "), "\n")
