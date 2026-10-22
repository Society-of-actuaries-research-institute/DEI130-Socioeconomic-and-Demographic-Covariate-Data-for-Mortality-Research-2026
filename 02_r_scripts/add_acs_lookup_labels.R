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
get_column_lookup_year <- function(column_name, lookup_years) {
  if (str_detect(column_name, "^\\d{4}-\\d{4}$")) {
    bounds <- str_split(column_name, "-", simplify = TRUE) |>
      as.integer()
    start_year <- bounds[1]
    end_year <- bounds[2]
  } else {
    start_year <- as.integer(column_name)
    end_year <- as.integer(column_name)
  }

  candidates <- lookup_years[lookup_years >= start_year & lookup_years <= end_year]

  if (length(candidates) == 0) {
    NA_integer_
  } else {
    max(candidates)
  }
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
    lookup_year <- str_extract(basename(path), "\\d{4}") |>
      as.integer()

    read_csv(path, show_col_types = FALSE) |>
      transmute(
        lookup_year = lookup_year,
        variable = name,
        lookup_label = label,
        lookup_concept = concept
      )
  })
}

normalize_lookup_text <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("\\s+", " ") |>
    str_replace_all("\\s*!!\\s*", "!!") |>
    str_replace_all("\\s*\\|\\s*", "|") |>
    str_replace_all(":+", ":") |>
    str_replace_all(":($|!!|\\|)", "\\1") |>
    str_squish()
}


# Load mapping and lookups -------------------------------------------------
mapping_wide <- read_csv(
  file.path(wd$data_input, "acs_vars_wide.csv"),
  show_col_types = FALSE
)

mapping_cols <- names(mapping_wide)[-1]

lookup_long <- load_acs_lookup_files(wd$acs_lookup)
lookup_years <- sort(unique(lookup_long$lookup_year))

column_lookup_years <- tibble(column = mapping_cols) |>
  mutate(
    lookup_year = map_int(
      column,
      get_column_lookup_year,
      lookup_years = lookup_years
    )
  )

missing_lookup_years <- column_lookup_years |>
  filter(is.na(lookup_year))

if (nrow(missing_lookup_years) > 0) {
  stop(
    "No ACS lookup file is available for these acs_vars_wide.csv columns: ",
    paste(missing_lookup_years$column, collapse = ", ")
  )
}


# Add lookup labels and concepts ------------------------------------------
mapping_with_lookup_long <- mapping_wide |>
  pivot_longer(
    cols = all_of(mapping_cols),
    names_to = "column",
    values_to = "variable"
  ) |>
  left_join(column_lookup_years, by = "column") |>
  left_join(lookup_long, by = c("lookup_year", "variable")) |>
  mutate(
    lookup_label = if_else(is.na(variable) | variable == "", NA_character_, lookup_label),
    lookup_concept = if_else(is.na(variable) | variable == "", NA_character_, lookup_concept),
    description = case_when(
      is.na(lookup_concept) & is.na(lookup_label) ~ NA_character_,
      is.na(lookup_concept) ~ lookup_label,
      is.na(lookup_label) ~ lookup_concept,
      TRUE ~ paste(lookup_concept, lookup_label, sep = " | ")
    ),
    description_normalized = normalize_lookup_text(description)
  )

missing_labels <- mapping_with_lookup_long |>
  filter(!is.na(variable), variable != "", is.na(lookup_label) | is.na(lookup_concept)) |>
  distinct(name, column, variable, lookup_year) |>
  arrange(column, name)

if (nrow(missing_labels) > 0) {
  warning(
    nrow(missing_labels),
    " mapped ACS variable(s) were not found in their selected lookup year. ",
    "See 01_data_output/acs_vars_wide_missing_lookup_labels.csv."
  )

  write_csv(
    missing_labels,
    file.path(wd$data_output, "acs_vars_wide_missing_lookup_labels.csv")
  )
}

description_status <- mapping_with_lookup_long |>
  filter(!is.na(variable), variable != "") |>
  group_by(name) |>
  summarise(
    description = description[which(!is.na(description))[1]],
    description_status = case_when(
      any(is.na(description)) ~ "missing_lookup",
      n_distinct(description_normalized, na.rm = TRUE) == 1 ~ "same",
      TRUE ~ "changed_review_needed"
    ),
    .groups = "drop"
  )

description_variant_details <- mapping_with_lookup_long |>
  inner_join(
    description_status |> filter(description_status == "changed_review_needed") |> select(name),
    by = "name"
  ) |>
  filter(!is.na(description_normalized)) |>
  group_by(name, description_normalized) |>
  arrange(lookup_year, .by_group = TRUE) |>
  summarise(
    first_lookup_year = min(lookup_year),
    columns = paste(column, collapse = ", "),
    variables = if_else(
      n_distinct(variable) == 1,
      first(variable),
      paste(paste0(column, "=", variable), collapse = ", ")
    ),
    description = description[n()],
    .groups = "drop"
  ) |>
  arrange(name, first_lookup_year) |>
  group_by(name) |>
  mutate(description_variant_count = n()) |>
  ungroup() |>
  mutate(
    variant = paste0(
      columns,
      " [",
      variables,
      "]: ",
      description
    )
  )

description_variants <- description_variant_details |>
  group_by(name) |>
  summarise(
    description_variants = paste(variant, collapse = " || "),
    description_variant_count = first(description_variant_count),
    .groups = "drop"
  )

description_summary <- description_status |>
  left_join(description_variants, by = "name")

output <- mapping_wide |>
  left_join(description_summary, by = "name")

write_csv(
  output,
  file.path(wd$data_input, "acs_vars_wide_with_labels.csv")
)


# Console summary ----------------------------------------------------------
cat("\nAdded ACS lookup descriptions.\n")
cat("Input:", file.path(wd$data_input, "acs_vars_wide.csv"), "\n")
cat("Output:", file.path(wd$data_input, "acs_vars_wide_with_labels.csv"), "\n")
cat("Lookup years found:", paste(lookup_years, collapse = ", "), "\n")
cat("\nDescription status summary:\n")
print(count(output, description_status), n = Inf)
cat("\nColumn-to-lookup-year mapping:\n")
print(column_lookup_years, n = nrow(column_lookup_years))
