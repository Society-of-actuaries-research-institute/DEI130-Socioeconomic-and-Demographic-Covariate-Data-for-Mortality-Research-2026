library(tidyverse)

rm(list = ls())

# Project paths ------------------------------------------------------------
wd <- list(
  project = getwd(),
  data_input = file.path(getwd(), "00_data_input"),
  data_output = file.path(getwd(), "01_data_output"),
  r_scripts = file.path(getwd(), "02_r_scripts")
)

wd$acs_lookup <- file.path(wd$data_input, "acs_lookup")


# Helpers ------------------------------------------------------------------
expand_range_columns <- function(df) {
  range_cols <- names(df)[grepl("^\\d{4}-\\d{4}$", names(df))]

  expand_single_range <- function(data, col_name) {
    years <- as.integer(strsplit(col_name, "-")[[1]])
    year_names <- as.character(seq(years[1], years[2]))

    data[year_names] <- rep(list(data[[col_name]]), length(year_names))
    data
  }

  for (col_name in range_cols) {
    df <- expand_single_range(df, col_name)
  }

  df
}

load_acs_lookup_files <- function(lookup_dir) {
  lookup_files <- list.files(
    path = lookup_dir,
    pattern = "^acs5_\\d{4}\\.csv$",
    full.names = TRUE
  )

  if (length(lookup_files) == 0) {
    stop("No lookup files named like 'acs5_YYYY.csv' were found in 00_data_input/acs_lookup.")
  }

  map_dfr(lookup_files, function(path) {
    year <- str_extract(basename(path), "\\d{4}") |> as.integer()

    read_csv(path, show_col_types = FALSE) |>
      transmute(
        year = year,
        ID = name,
        lookup_label = label,
        lookup_concept = concept
      )
  })
}


# Load mapping and lookups -------------------------------------------------
mapping_wide <- read_csv(
  file.path(wd$data_input, "acs_vars_wide.csv"),
  show_col_types = FALSE
)

mapping_long <- mapping_wide |>
  expand_range_columns() |>
  select(-matches("^\\d{4}-\\d{4}$")) |>
  pivot_longer(
    cols = -name,
    names_to = "year",
    values_to = "ID"
  ) |>
  mutate(year = as.integer(year)) |>
  filter(!is.na(ID), ID != "")

lookup_long <- load_acs_lookup_files(wd$acs_lookup)

lookup_years <- sort(unique(lookup_long$year))
mapped_years <- sort(unique(mapping_long$year))
latest_lookup_year <- max(lookup_years)
latest_mapped_year <- max(mapped_years)

# Review newest release against last mapped year ---------------------------
review_base_year <- max(mapped_years[mapped_years <= latest_lookup_year])

latest_release_review <- mapping_long |>
  filter(year == review_base_year) |>
  select(name, current_ID = ID) |>
  left_join(
    lookup_long |>
      filter(year == review_base_year) |>
      select(
        ID,
        current_lookup_label = lookup_label,
        current_lookup_concept = lookup_concept
      ),
    by = c("current_ID" = "ID")
  ) |>
  left_join(
    lookup_long |>
      filter(year == latest_lookup_year) |>
      select(
        ID,
        latest_lookup_label = lookup_label,
        latest_lookup_concept = lookup_concept
      ),
    by = c("current_ID" = "ID")
  ) |>
  mutate(
    current_year = review_base_year,
    candidate_year = latest_lookup_year,
    status = case_when(
      current_year == candidate_year & !is.na(latest_lookup_label) ~ "already_mapped_to_latest_year",
      is.na(latest_lookup_label) ~ "missing_in_candidate_year",
      current_lookup_label != latest_lookup_label |
        current_lookup_concept != latest_lookup_concept ~ "definition_changed_review_needed",
      TRUE ~ "exists_same_label_concept"
    )
  ) |>
  select(
    name,
    current_year,
    current_ID,
    current_lookup_label,
    current_lookup_concept,
    candidate_year,
    latest_lookup_label,
    latest_lookup_concept,
    status
  ) |>
  arrange(name)

latest_release_summary <- latest_release_review |>
  count(status) |>
  arrange(status)


# Write outputs ------------------------------------------------------------
write_csv(
  latest_release_review,
  file.path(wd$data_output, "acs_var_mapping_latest_release_review.csv")
)


# Console summary ----------------------------------------------------------
cat("\nACS variable mapping check complete.\n")
cat("Lookup years found:", paste(lookup_years, collapse = ", "), "\n")
cat("Latest mapped year:", latest_mapped_year, "\n")
cat("Latest lookup year:", latest_lookup_year, "\n")
cat("Latest release review:", file.path(wd$data_output, "acs_var_mapping_latest_release_review.csv"), "\n")
cat("\n")
cat("Latest release summary:\n")
print(latest_release_summary, n = nrow(latest_release_summary))
