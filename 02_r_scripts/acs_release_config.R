# Shared ACS release settings used by build, update, and validation scripts.
#
# Year is the ACS data year: the end year of the ACS 5-year period. For
# example, 2024 refers to the 2020-2024 ACS 5-year release, not the
# publication year or midpoint year.

# Update this when adding a new ACS release.
acs_series_latest_year <- 2024

# First ACS data year retained in the full-series output files. This should
# usually stay fixed; annual ACS updates should only change
# acs_series_latest_year.
acs_series_start_year <- 2009

# Full range of ACS data years expected in the combined series files.
acs_series_years <- acs_series_start_year:acs_series_latest_year

# ACS lookup years to refresh/use for variable mappings. This currently tracks
# the full data series so variable definitions can vary across releases.
acs_lookup_years <- acs_series_years
