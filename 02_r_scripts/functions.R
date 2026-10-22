`%ni%` <- Negate(`%in%`)

# Build the standard ACS output file path.
#
# Naming convention:
#   - single ACS release: final_acs_{geo}_{year}.csv
#   - full ACS series:    final_acs_{geo}_full_series_{latest_year}.csv
#
# Arguments:
#   data_output: output directory, usually wd$data_output.
#   geo: geography label used in the file name, e.g. "national", "states",
#        or "counties".
#   years: one ACS end year for a single-release file, or multiple ACS end
#          years for a full-series file. The largest year is used in the
#          file suffix.
acs_output_file <- function(data_output, geo, years) {
  latest_year <- max(years)
  
  file_name <- if (length(years) == 1) {
    paste0("final_acs_", geo, "_", latest_year, ".csv")
  } else {
    paste0("final_acs_", geo, "_full_series_", latest_year, ".csv")
  }
  
  file.path(data_output, file_name)
}

# Identify ACS estimate columns that should aggregate across geographies.
#
# This keeps count-like estimate columns shared by all supplied data frames and
# excludes medians, rates, ratios, indexes, and other non-additive statistics.
acs_additive_estimate_vars <- function(...,
                                       exclude_patterns = c(
                                         "^Med_",
                                         "_Med_",
                                         "Median",
                                         "Mean",
                                         "Avg",
                                         "Average",
                                         "Rate",
                                         "Pct",
                                         "Percent",
                                         "Ratio",
                                         "Index",
                                         "Gini"
                                       )) {
  dfs <- list(...)
  if (length(dfs) == 0) {
    return(character())
  }
  
  shared_cols <- Reduce(intersect, lapply(dfs, names))
  estimate_cols <- shared_cols[grepl("_EST$", shared_cols)]
  
  non_additive <- grepl(
    paste(exclude_patterns, collapse = "|"),
    estimate_cols,
    ignore.case = TRUE
  )
  
  estimate_cols[!non_additive]
}

# Sum ACS estimates for geographic consistency checks while excluding Census
# annotation/sentinel values from the lower-level geography totals.
acs_aggregate_sum <- function(x, sentinel_values = c(-888888888, -555555555)) {
  x <- as.numeric(x)
  valid <- !is.na(x) & !(x %in% sentinel_values)
  
  if (!any(valid)) {
    return(NA_real_)
  }
  
  sum(x[valid])
}

# Identify reported values that are valid denominators for geographic
# consistency checks.
acs_valid_reported_value <- function(x,
                                     sentinel_values = c(-888888888, -555555555)) {
  !is.na(x) & !(x %in% sentinel_values) & x != 0
}

# Percent difference for aggregation checks. Sentinel, missing, and zero
# reported values are skipped because they are not valid denominators.
acs_aggregate_pct_diff <- function(aggregate_value,
                                   reported_value,
                                   sentinel_values = c(-888888888, -555555555)) {
  aggregate_value <- as.numeric(aggregate_value)
  reported_value <- as.numeric(reported_value)
  
  invalid <- is.na(aggregate_value) |
    aggregate_value %in% sentinel_values |
    !acs_valid_reported_value(reported_value, sentinel_values)
  
  pct_diff <- (aggregate_value - reported_value) / reported_value * 100
  pct_diff[invalid] <- NA_real_
  pct_diff
}

# Append a newly built single ACS release to the previous full-series file.
#
# The current release is removed from the previous file before appending so the
# function can be rerun without duplicating rows.
append_acs_release_to_full_series <- function(current_release,
                                              previous_full_series_file,
                                              current_year,
                                              arrange_cols = "Year") {
  if (!file.exists(previous_full_series_file)) {
    stop("Previous full-series file does not exist: ", previous_full_series_file)
  }
  
  previous_full_series <- read_csv(
    previous_full_series_file,
    show_col_types = FALSE,
    progress = FALSE
  )
  
  bind_rows(
    previous_full_series |> filter(Year != current_year),
    current_release
  ) |>
    arrange(across(any_of(arrange_cols)))
}

# Find large and zero year-to-year ACS estimate changes without reshaping the
# full data set to long format.
#
# This is used by the full-series validation scripts. County-level ACS files
# can have tens of millions of geography-year-variable combinations after
# pivot_longer(), so this helper keeps the estimate values in matrices,
# calculates lagged values and percent changes by geography, and only converts
# the flagged cells to tidy output.
#
# Arguments:
#   df: ACS output data frame containing Year, geography identifiers, and
#       estimate columns.
#   group_cols: columns that define one time series, e.g. "County_FIPS" or
#               "State_FIPS". Use character() for a single national series.
#   id_cols: columns to carry into the returned flagged-row tables.
#   estimate_cols: numeric estimate columns to scan. Defaults to columns ending
#                  in "_EST".
#   sentinel_values: ACS annotation values to exclude from percent-change
#                    calculations.
#   large_increase_threshold: percent-change threshold for positive changes.
#   large_decrease_threshold: percent-change threshold for negative changes.
#   min_prev_release: optional minimum previous-release estimate required for
#                     large-change flags. Use NULL to apply no minimum.
#
# Returns:
#   A list with two tibbles:
#   - big_changes: rows where pct_change exceeds the supplied thresholds.
#   - zero_changes: rows where pct_change is exactly zero.
#   Both include id_cols, variable, prev_release, curr_release, and pct_change.
find_acs_series_changes <- function(df,
                                    group_cols,
                                    id_cols,
                                    estimate_cols = names(df)[grepl("_EST$", names(df))],
                                    sentinel_values = c(-888888888, -555555555),
                                    large_increase_threshold = 100,
                                    large_decrease_threshold = -20,
                                    min_prev_release = 10) {
  ordered_df <- df |>
    arrange(across(all_of(c(group_cols, "Year"))))
  
  current <- as.matrix(ordered_df[estimate_cols])
  storage.mode(current) <- "numeric"
  
  previous <- current
  previous[-1, ] <- current[-nrow(current), , drop = FALSE]
  
  group_start <- if (length(group_cols) == 0) {
    seq_len(nrow(ordered_df)) == 1
  } else {
    !duplicated(ordered_df[group_cols])
  }
  previous[group_start, ] <- NA_real_
  
  current_sentinel <- matrix(current %in% sentinel_values, nrow = nrow(current))
  previous_sentinel <- matrix(previous %in% sentinel_values, nrow = nrow(previous))
  
  invalid <- current_sentinel |
    previous_sentinel |
    is.na(previous) |
    previous == 0
  
  pct_change <- (current - previous) / previous * 100
  pct_change[invalid] <- NA_real_
  
  large_change_mask <- !is.na(pct_change) &
    (
      pct_change > large_increase_threshold |
        pct_change < large_decrease_threshold
    )
  
  if (!is.null(min_prev_release)) {
    large_change_mask <- large_change_mask & previous >= min_prev_release
  }
  
  zero_change_mask <- !is.na(pct_change) & pct_change == 0
  
  make_change_tbl <- function(mask) {
    idx <- which(mask, arr.ind = TRUE)
    
    if (nrow(idx) == 0) {
      return(
        ordered_df[0, id_cols] |>
          mutate(
            variable = character(),
            prev_release = numeric(),
            curr_release = numeric(),
            pct_change = numeric()
          )
      )
    }
    
    bind_cols(
      ordered_df[idx[, "row"], id_cols, drop = FALSE],
      tibble(
        variable = paste0(estimate_cols[idx[, "col"]], "_pct_change"),
        prev_release = previous[idx],
        curr_release = current[idx],
        pct_change = pct_change[idx]
      )
    )
  }
  
  list(
    big_changes = make_change_tbl(large_change_mask) |>
      arrange(desc(abs(pct_change))),
    zero_changes = make_change_tbl(zero_change_mask)
  )
}


# Calculate an approximate MOE for a sum using the ACS accuracy guidance.
# Reference:
# U.S. Census Bureau. "A Compass for Understanding and Using American Community
# Survey Data: What General Data Users Need to Know" / ACS multiyear accuracy
# guidance. When covariance terms are unavailable, the Bureau recommends
# approximating the SE of a sum as the square root of the sum of squared SEs.
# For multiple zero estimates, include only one MOE from the zero-estimate
# components to avoid inflating the result.
# https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/MultiyearACSAccuracyofData2015.pdf
safe_moe_sum <- function(moe, estimate = NULL, na.rm = FALSE) {
  moe <- as.numeric(moe)
  
  if (!is.null(estimate)) {
    estimate <- as.numeric(estimate)
    zeros <- estimate == 0
    
    if (any(zeros, na.rm = TRUE)) {
      max_zero_moe <- max(moe[zeros], na.rm = TRUE)
      forcalc <- c(max_zero_moe, moe[!zeros])
    } else {
      forcalc <- moe
    }
  } else {
    forcalc <- moe
  }
  
  sqrt(sum(forcalc^2, na.rm = na.rm))
}

# Vectorized version of safe_moe_sum() for row-wise grouped sums.
#
# Arguments:
#   moe:      data frame, tibble, or matrix of MOE columns.
#   estimate: optional data frame, tibble, or matrix of matching estimate
#             columns. If supplied, it must have the same dimensions/order as
#             moe.
#   na.rm:    passed to rowSums(); defaults to FALSE to match safe_moe_sum().
#
# For each row, this reproduces safe_moe_sum():
#   - combine MOEs as sqrt(sum(moe^2));
#   - if any matching estimates are zero, include only the largest MOE among
#     those zero-estimate components, plus the MOEs for nonzero components;
#   - preserve the original NA behavior, including NA estimates.
#
# Use this instead of rowwise() + c_across() when aggregating many rows.
safe_moe_sum_vec <- function(moe, estimate = NULL, na.rm = FALSE) {
  moe <- as.matrix(moe)
  storage.mode(moe) <- "numeric"
  
  if (is.null(estimate)) {
    return(sqrt(rowSums(moe^2, na.rm = na.rm)))
  }
  
  estimate <- as.matrix(estimate)
  storage.mode(estimate) <- "numeric"
  
  zeros <- estimate == 0
  zeros_true <- zeros
  zeros_true[is.na(zeros_true)] <- FALSE
  
  has_zero <- rowSums(zeros_true) > 0
  has_zero_mat <- matrix(has_zero, nrow = nrow(moe), ncol = ncol(moe))
  
  selected_sq <- moe^2
  selected_sq[zeros_true & has_zero_mat] <- 0
  selected_sq[is.na(zeros) & has_zero_mat] <- NA_real_
  
  max_zero_mat <- moe
  max_zero_mat[!zeros_true] <- -Inf
  max_zero_mat[is.na(max_zero_mat)] <- -Inf
  max_zero_moe <- do.call(pmax, as.data.frame(max_zero_mat))
  
  base <- sqrt(rowSums(moe^2, na.rm = na.rm))
  adjusted <- sqrt(rowSums(selected_sq, na.rm = na.rm) + max_zero_moe^2)
  
  base[has_zero] <- adjusted[has_zero]
  base
}


# detect and expand range columns ----
expand_range_columns <- function(df) {
  # Find columns with range patterns (e.g., "2019-2021")
  range_cols <- names(df)[grepl("^\\d{4}-\\d{4}$", names(df))]
  
  # Function to expand a single range column
  expand_single_range <- function(df, col_name) {
    # Extract start and end years
    years <- as.numeric(strsplit(col_name, "-")[[1]])
    start_year <- years[1]
    end_year <- years[2]
    
    # Create individual year columns
    year_sequence <- start_year:end_year
    new_cols <- setNames(rep(list(df[[col_name]]), length(year_sequence)), 
                         as.character(year_sequence))
    
    # Add new columns to dataframe
    df[names(new_cols)] <- new_cols
    return(df)
  }
  
  # Apply to all range columns
  for (col in range_cols) {
    df <- expand_single_range(df, col)
  }
  
  return(df)
}
