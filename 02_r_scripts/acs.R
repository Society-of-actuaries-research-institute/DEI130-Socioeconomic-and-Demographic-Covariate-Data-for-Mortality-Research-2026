# 1. Libraries --------------------------------------------------------
library(tidyverse)  # data wrangling
library(tidycensus) # ACS API

rm(list = ls())

# working directory
wd <- list()

# commonly used paths in my working directory
wd$data_input <- paste0(getwd(), "/00_data_input")
wd$data_output <- paste0(getwd(), "/01_data_output")
wd$r_scripts <- paste0(getwd(), "/02_r_scripts")

source(file.path(wd$r_scripts, "functions.R"))
source(file.path(wd$r_scripts, "acs_release_config.R"))


# load your API key
census_data_api <- read_csv(file.path(wd$data_input, "census_data_api.csv"), col_names = FALSE) |> 
  pull()

census_api_key(census_data_api)

# 2. User settings ---------------------------------------------------------
# Choose the ACS 5-year release year(s) to fetch.
#
# Each year is the END year of an ACS 5-year file:
#   2024 = 2020-2024 ACS 5-year
#   2023 = 2019-2023 ACS 5-year
#
# Common choices:
#   years <- max(acs_series_years)  # fetch only the latest configured release
#   years <- 2024                   # fetch one specific release
#   years <- 2019:2024              # fetch a multi-year series from scratch
#
# The available year range is set in 02_r_scripts/acs_release_config.R.
years <- max(acs_series_years)

# Choose which geography-level files to build.
#
# Running counties takes the longest because the script fetches counties
# state-by-state. To rebuild all final ACS outputs, run this script once with
# all three settings TRUE, or run it separately for each geography.
run_national <- FALSE
run_states <- FALSE
run_counties <- FALSE

# Output naming:
#   one year:       final_acs_counties_2024.csv
#   multiple years: final_acs_counties_full_series_2024.csv

national_output_file <- acs_output_file(wd$data_output, "national", years)
states_output_file <- acs_output_file(wd$data_output, "states", years)
counties_output_file <- acs_output_file(wd$data_output, "counties", years)

# 3. Read lookup tables ---------------------------------------------------
# To refresh ACS lookup files, run:
# 02_r_scripts/update_acs_lookup_files.R

acs_vars_raw <- read_csv(file.path(wd$data_input, "acs_vars_wide.csv"))

acs_vars_expanded <- expand_range_columns(acs_vars_raw)

# Remove original range columns
range_cols <- names(acs_vars_expanded)[grepl("^\\d{4}-\\d{4}$", names(acs_vars_expanded))]
acs_vars_wide <- acs_vars_expanded |> 
  select(-all_of(range_cols))


acs_vars <- acs_vars_wide |> 
  pivot_longer(cols = -name,
               names_to = "year",
               values_to = "ID") |> 
  filter(!is.na(ID)) |> 
  arrange(year, ID)


source(file.path(wd$r_scripts, "read_decile_aggregate.R"))

state_codes <- states$State_FIPS

# 4. Fetch ACS data helper ------------------------------------------------
fetch_acs_set <- function(yr, var_ids) {
  rng <- paste(yr - 4, yr, sep = "-")  # Obtain the the 5-year range
  
  # National
  nat <- if (run_national) {
    get_acs(
      geography = "us", variables = var_ids,
      year = yr, output = "wide"
    ) |>
      transmute(
        GEOID, NAME,
        Year   = yr,
        Source = paste(rng, "ACS"),
        across(-c(GEOID, NAME))
      )
  } else {
    tibble()
  }
  
  # States
  st <- if (run_states) {
    get_acs(
      geography = "state", variables = var_ids,
      year = yr, output = "wide"
    ) |>
      transmute(GEOID, NAME, Year = yr, Source = paste(rng, "ACS"), across(-c(GEOID, NAME)))
  } else {
    tibble()
  }
  
  # Counties
  ct <- if (run_counties) {
    map_dfr(
      state_codes,
      ~ get_acs(
        geography = "county", variables = var_ids,
        year = yr, state = .x, output = "wide"
      )
    ) |>
      transmute(GEOID, NAME, Year = yr, Source = paste(rng, "ACS"), across(-c(GEOID, NAME)))
  } else {
    tibble()
  }
  
  list(national = nat, states = st, counties = ct)
  
}

# 5. Loop selected ACS release years --------------------------------------

acs_nat_list <- vector("list", length(years))
acs_st_list  <- vector("list", length(years))
acs_ct_list  <- vector("list", length(years))

for (i in seq_along(years)) {
  yr <- years[i]
  ids <- acs_vars |> 
    filter(year == yr) |> 
    pull(ID) |> 
    set_names(pull(acs_vars, name)[acs_vars$year == yr])
  
  out <- fetch_acs_set(yr, ids)
  
  acs_nat_list[[i]] <- out$national
  acs_st_list[[i]]  <- out$states
  acs_ct_list[[i]]  <- out$counties
}

acs_national_tmp <- bind_rows(acs_nat_list)
acs_states_tmp   <- bind_rows(acs_st_list)
acs_counties_tmp <- bind_rows(acs_ct_list)

# 6. Summarize education categories ---------------------------------------
less_hs   <- c("NoSchooling", "Nursery_to_4th","4th_and_6th",
               "7th_and_8th","9th","10th","11th","12th_NoDiploma")
some_coll <- c("SomeCollege_Less_1yr","SomeCollege_More_1yr","Asciate_Degree")
bach_plus <- c("Bachelor_Degree","Master_Degree","Professional_Degree","Doctorate_Degree")


# c(less_hs, some_coll, bach_plus) |> length()

summarize_edu <- function(df) {
  cols_to_remove <- with(expand.grid(gender = c("M_", "F_"),
                                     edu = c(less_hs, some_coll, bach_plus),
                                     suffix = c("E", "M")),
                         paste0(gender, edu, suffix))
  
  df |> mutate(
    M_LessHSE      = rowSums(pick(all_of(paste0("M_", less_hs, "E"))),   na.rm = TRUE),
    M_SomeColOrAscE= rowSums(pick(all_of(paste0("M_", some_coll, "E"))), na.rm = TRUE),
    M_BachOrHigherE= rowSums(pick(all_of(paste0("M_", bach_plus, "E"))), na.rm = TRUE),
    F_LessHSE      = rowSums(pick(all_of(paste0("F_", less_hs, "E"))),   na.rm = TRUE),
    F_SomeColOrAscE= rowSums(pick(all_of(paste0("F_", some_coll, "E"))), na.rm = TRUE),
    F_BachOrHigherE= rowSums(pick(all_of(paste0("F_", bach_plus, "E"))), na.rm = TRUE),
    M_LessHSM_ADJ      = safe_moe_sum_vec(
      pick(all_of(paste0("M_", less_hs, "M"))),
      pick(all_of(paste0("M_", less_hs, "E")))
    ),
    M_SomeColOrAscM_ADJ= safe_moe_sum_vec(
      pick(all_of(paste0("M_", some_coll, "M"))),
      pick(all_of(paste0("M_", some_coll, "E")))
    ),
    M_BachOrHigherM_ADJ= safe_moe_sum_vec(
      pick(all_of(paste0("M_", bach_plus, "M"))),
      pick(all_of(paste0("M_", bach_plus, "E")))
    ),
    F_LessHSM_ADJ      = safe_moe_sum_vec(
      pick(all_of(paste0("F_", less_hs, "M"))),
      pick(all_of(paste0("F_", less_hs, "E")))
    ),
    F_SomeColOrAscM_ADJ= safe_moe_sum_vec(
      pick(all_of(paste0("F_", some_coll, "M"))),
      pick(all_of(paste0("F_", some_coll, "E")))
    ),
    F_BachOrHigherM_ADJ= safe_moe_sum_vec(
      pick(all_of(paste0("F_", bach_plus, "M"))),
      pick(all_of(paste0("F_", bach_plus, "E")))
    )
  ) |>
    select(-all_of(cols_to_remove))
  
  
}

acs_states   <- if (run_states) summarize_edu(acs_states_tmp) else tibble()
acs_counties <- if (run_counties) summarize_edu(acs_counties_tmp) else tibble()
acs_national <- if (run_national) summarize_edu(acs_national_tmp) else tibble()



# 7. Fill NA values for MOE and estimates --------------------------------
fill_na_moe <- function(df) {
  moe_cols <- grep("M(_ADJ)?$", names(df), value = TRUE)
  for (mc in moe_cols) {
    ec <- sub("M(_ADJ)?$", "E", mc)
    df[[mc]] <- case_when(
      is.na(df[[mc]]) & is.na(df[[ec]]) ~ -888888888L,
      is.na(df[[mc]]) & !is.na(df[[ec]]) ~ -555555555L,
      df[[mc]] == 0 ~ -555555555L,
      TRUE ~ df[[mc]]
    )
  }
  est_cols <- grep("E$", names(df), value = TRUE)
  df |> mutate(across(all_of(est_cols), ~ replace_na(.x, -888888888L)))
}

acs_states   <- if (run_states) fill_na_moe(acs_states) else tibble()
acs_counties <- if (run_counties) fill_na_moe(acs_counties) else tibble()
acs_national <- if (run_national) fill_na_moe(acs_national) else tibble()

add_spouse_vars <- function(df) {
  has_combined_est <- "Sp_InHHE" %in% names(df)
  has_combined_moe <- "Sp_InHHM" %in% names(df)
  
  safe_spouse_moe_sum <- function(same_moe, opp_moe, same_est, opp_est) {
    invalid_codes <- c(-888888888, -555555555)
    spouse_moe <- c(same_moe, opp_moe)
    spouse_est <- c(same_est, opp_est)
    
    unusable <- is.na(spouse_moe) | spouse_moe %in% invalid_codes |
      is.na(spouse_est) | spouse_est %in% invalid_codes
    
    if (all(unusable)) {
      return(-888888888)
    }
    
    safe_moe_sum(
      moe = spouse_moe[!unusable],
      estimate = spouse_est[!unusable]
    )
  }
  
  df <- df |>
    mutate(
      Sp_InHHE = if (has_combined_est) {
        if_else(Sp_InHHE == -888888888, SameSexSp_InHHE + OppSexSp_InHHE, Sp_InHHE)
      } else {
        SameSexSp_InHHE + OppSexSp_InHHE
      }
    )
  
  df |>
    rowwise() |>
    mutate(
      Sp_InHHM_ADJ = if (has_combined_moe) {
        if_else(
          Sp_InHHM != -888888888,
          -888888888,
          safe_spouse_moe_sum(
            SameSexSp_InHHM,
            OppSexSp_InHHM,
            SameSexSp_InHHE,
            OppSexSp_InHHE
          )
        )
      } else {
        safe_spouse_moe_sum(
          SameSexSp_InHHM,
          OppSexSp_InHHM,
          SameSexSp_InHHE,
          OppSexSp_InHHE
        )
      }
    ) |>
    ungroup()
}

select_ordered_cols <- function(df, ordered_cols, label = "dataset") {
  ordered_present <- intersect(ordered_cols, names(df))
  extra_cols <- setdiff(names(df), ordered_cols)
  
  if (length(extra_cols) > 0) {
    warning(
      paste0(
        "Columns not listed in ordering template for ",
        label,
        ": ",
        paste(extra_cols, collapse = ", ")
      )
    )
  }
  
  df |> select(all_of(ordered_present), any_of(extra_cols))
}

# 8. Assemble final ACS tables --------------------------------------------

master_col_order <- read_csv(file.path(wd$data_input, "acs_var_order.csv")) |>
  mutate(
    suffixes = case_when(
      moe_type == "adj" ~ list(c("E", "M_ADJ")),
      moe_type == "both" ~ list(c("E", "M", "M_ADJ")),
      moe_type == "no" ~ list(""),  # empty string for no suffix
      TRUE ~ list(c("E", "M"))  # for NA or other values
    )
  ) |>
  rowwise() |>
  mutate(var_names = list(paste0(var_name, suffixes))) |>
  pull(var_names) |>
  unlist()


county_col_order <- master_col_order

state_col_order <- master_col_order[master_col_order %ni% c("County", "County_FIPS", "Decile", "Aggregates")]

nation_col_order <- master_col_order[master_col_order %ni% c("State", "State_USPS", "State_FIPS", "County", "County_FIPS", "Decile", "Aggregates")]

# States
final_acs_states <- if (run_states) {
  acs_states |>
    add_spouse_vars() |>
    rename(State_FIPS = GEOID, State = NAME) |>
    left_join(select(states, State_FIPS, State_USPS), by = "State_FIPS") |>
    filter(State_USPS != "PR") |>
    select_ordered_cols(state_col_order, "ACS states") |> 
    arrange(Year, State_FIPS)
} else {
  tibble()
}


# Counties
final_acs_counties <- if (run_counties) {
  acs_counties |>
    add_spouse_vars() |>
    separate(NAME, into = c("County", "State"), sep = ",") |>
    rename(County_FIPS = GEOID) |> 
    mutate(
      State_FIPS    = substr(County_FIPS, 1, 2),
      State         = str_to_title(trimws(State)),
      County        = str_to_title(trimws(County))
    ) |>
    left_join(select(states, State_FIPS, State_USPS), by = "State_FIPS") |>
    filter(State_USPS != "PR") |>
    left_join(deciles,   by = "County_FIPS") |>
    left_join(aggregates,by = "County_FIPS") |>
    select_ordered_cols(county_col_order, "ACS counties") |> 
    arrange(Year, State_FIPS, County_FIPS)
} else {
  tibble()
}

# National
final_acs_national <- if (run_national) {
  acs_national |>
    add_spouse_vars() |>
    select(-any_of(c("GEOID", "NAME"))) |>
    select_ordered_cols(nation_col_order, "ACS national") |>
    arrange(Year)
} else {
  tibble()
}

## change suffix: M -> _MOE, M_ADJ -> _MOE_ADJ, E -> _EST ----
rename_acs_suffixes <- function(df) {
  df |> 
    rename_with(~ case_when(
      str_ends(.x, "M_ADJ") ~ str_replace(.x, "M_ADJ$", "_MOE_ADJ"),
      str_ends(.x, "M") ~ str_replace(.x, "M$", "_MOE"),
      str_ends(.x, "E") ~ str_replace(.x, "E$", "_EST"),
      TRUE ~ .x
    ))
}
final_acs_states_renamed <- if (run_states) rename_acs_suffixes(final_acs_states) else tibble()
final_acs_counties_renamed <- if (run_counties) rename_acs_suffixes(final_acs_counties) else tibble()
final_acs_national_renamed <- if (run_national) rename_acs_suffixes(final_acs_national) else tibble()

# 9. Write outputs --------------------------------------------------------
if (run_states) {
  write_csv(final_acs_states_renamed,
            states_output_file)
}

if (run_counties) {
  write_csv(final_acs_counties_renamed,
            counties_output_file)
}

if (run_national) {
  write_csv(final_acs_national_renamed,  
            national_output_file)
}
