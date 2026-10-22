# 1. Libraries ------------------------------------------------------------
library(tidyverse)

rm(list = ls())

# Purpose ------------------------------------------------------------------
# Append validated single-year ACS outputs to the previous full-series files.
#
# Run this after:
#   1. 02_r_scripts/acs.R builds final_acs_{geo}_{latest_year}.csv
#   2. The validate_acs_*_update.R scripts pass review
#
# This script writes:
#   final_acs_{geo}_full_series_{latest_year}.csv

# Project paths ------------------------------------------------------------
wd <- list(
  project = getwd(),
  data_input = file.path(getwd(), "00_data_input"),
  data_output = file.path(getwd(), "01_data_output"),
  r_scripts = file.path(getwd(), "02_r_scripts")
)

source(file.path(wd$r_scripts, "functions.R"))
source(file.path(wd$r_scripts, "acs_release_config.R"))


# 2. User settings ---------------------------------------------------------
# Choose which geography-level full-series files to update.
run_national <- TRUE
run_states <- TRUE
run_counties <- TRUE

data_year <- acs_series_latest_year

if (data_year <= acs_series_start_year) {
  stop("Cannot append to a previous full series when the latest year is the start year.")
}


# 3. Update helper ---------------------------------------------------------
update_acs_full_series <- function(geo, arrange_cols) {
  current_file <- acs_output_file(wd$data_output, geo, data_year)
  previous_full_series_file <- acs_output_file(
    wd$data_output,
    geo,
    acs_series_start_year:(data_year - 1)
  )
  output_file <- acs_output_file(
    wd$data_output,
    geo,
    acs_series_start_year:data_year
  )
  
  if (!file.exists(current_file)) {
    stop("Current release file does not exist: ", current_file)
  }
  
  current_release <- read_csv(
    current_file,
    show_col_types = FALSE,
    progress = FALSE
  )
  
  full_series <- append_acs_release_to_full_series(
    current_release = current_release,
    previous_full_series_file = previous_full_series_file,
    current_year = data_year,
    arrange_cols = arrange_cols
  )
  
  write_csv(full_series, output_file)
  
  cat(
    "\nUpdated ", geo, " full series",
    "\n  Current release: ", current_file,
    "\n  Previous series: ", previous_full_series_file,
    "\n  Output series:   ", output_file,
    "\n  Rows written:    ", nrow(full_series),
    "\n",
    sep = ""
  )
}


# 4. Write selected full-series files -------------------------------------
if (run_national) {
  update_acs_full_series(
    geo = "national",
    arrange_cols = "Year"
  )
}

if (run_states) {
  update_acs_full_series(
    geo = "states",
    arrange_cols = c("Year", "State_FIPS")
  )
}

if (run_counties) {
  update_acs_full_series(
    geo = "counties",
    arrange_cols = c("Year", "State_FIPS", "County_FIPS")
  )
}
