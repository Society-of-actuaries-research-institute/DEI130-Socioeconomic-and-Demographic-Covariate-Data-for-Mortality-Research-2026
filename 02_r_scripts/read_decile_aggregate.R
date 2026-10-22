

deciles <- read_csv(
  file.path(wd$data_input, "deciles.csv"),
  col_types = cols(FIPS = col_character())
) |> 
  transmute(
    County_FIPS = FIPS,
    Decile      = `2000 Decile`
  )

aggregates <- read_csv(
  file.path(wd$data_input, "aggregates.csv"),
  col_types = cols(FIPS = col_character())
) |> 
  mutate(County_FIPS = str_pad(FIPS, 5, "left", "0")) |> 
  select(-FIPS)


states <- read_csv(file.path(wd$data_input, "state_fips.csv")) |>
  rename(
    State      = stname,
    State_FIPS = st,
    State_USPS = stusps
  )
