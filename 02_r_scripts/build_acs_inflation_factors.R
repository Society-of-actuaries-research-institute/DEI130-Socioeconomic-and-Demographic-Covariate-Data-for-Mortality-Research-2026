library(tidyverse)
library(readxl)

rm(list = ls())

# Project paths ------------------------------------------------------------
wd <- list(
  project = getwd(),
  data_input = file.path(getwd(), "00_data_input"),
  data_output = file.path(getwd(), "01_data_output"),
  r_scripts = file.path(getwd(), "02_r_scripts")
)

source(file.path(wd$r_scripts, "acs_release_config.R"))

source_candidates <- list.files(
  wd$data_input,
  pattern = "^r-cpi-u-rs-allitems-1977-[0-9]{4}\\.xlsx$",
  full.names = TRUE
)

if (length(source_candidates) == 0) {
  stop("No R-CPI-U-RS workbook matching 'r-cpi-u-rs-allitems-1977-YYYY.xlsx' was found in 00_data_input.")
}

source_years <- source_candidates |>
  basename() |>
  stringr::str_extract("[0-9]{4}(?=\\.xlsx$)") |>
  as.integer()

source_file <- source_candidates[which.max(source_years)]

target_year <- acs_series_latest_year
output_file <- file.path(
  wd$data_output,
  paste0("acs_inflation_factors_to_", target_year, ".csv")
)


# Read annual average R-CPI-U-RS values -----------------------------------
rcpi_annual <- read_excel(
  source_file,
  sheet = "All items",
  skip = 5
) |>
  rename_with(tolower) |>
  filter(str_detect(as.character(year), "^[0-9]{4}$")) |>
  transmute(
    year = as.integer(year),
    rcpi_u_rs_avg = as.numeric(avg)
  ) |>
  filter(!is.na(year), !is.na(rcpi_u_rs_avg))

to_index <- rcpi_annual |>
  filter(year == target_year) |>
  pull(rcpi_u_rs_avg)

if (length(to_index) != 1) {
  stop("Could not find a unique AVG value for the target year in the source workbook.")
}


# Build ACS 5-year factor table -------------------------------------------
acs_inflation_factors <- tibble(from_year = 2009:(target_year - 1)) |>
  left_join(rcpi_annual, by = c("from_year" = "year")) |>
  mutate(
    from_period = paste(from_year - 4, from_year, sep = "-"),
    to_year = target_year,
    to_index_avg = to_index,
    factor = to_index_avg / rcpi_u_rs_avg,
    index_series = "R-CPI-U-RS annual average",
    source_file = basename(source_file),
    source_column = "AVG",
    usage_notes = paste0(
      "Multiply ",
      from_year,
      "-dollar ACS estimates by this factor to express them in ",
      to_year,
      " dollars."
    )
  ) |>
  select(
    from_period,
    from_year,
    to_year,
    from_index_avg = rcpi_u_rs_avg,
    to_index_avg,
    factor,
    index_series,
    source_file,
    source_column,
    usage_notes
  )


# Write output -------------------------------------------------------------
write_csv(
  acs_inflation_factors,
  output_file
)

cat("Wrote:", output_file, "\n")
