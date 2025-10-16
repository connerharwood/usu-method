
library(tidyverse)
library(sf)

# ==== APPEND MAP UNIT POLYGONS ================================================

# All map unit polygons shapefiles
mu_polys_files = list.files(
  path = "Data/Raw/SSURGO",
  pattern = "^soilmu_a_.*\\.shp$",
  recursive = TRUE,
  full.names = TRUE
)

# Read and append all map unit shapefiles into one
mu_polys = mu_polys_files |>
  map(~ st_read(
    .x, quiet = TRUE
  ) |>
    st_make_valid() |>
    select(
      mukey = MUKEY,
      survey_area = AREASYMBOL,
      geometry
    ) |> 
    mutate(mukey = as.character(mukey))
  ) |>
  bind_rows()

# Save appended SSURGO polygons as layer in .gpkg file
st_write(mu_polys, "Data/Raw/SSURGO/ssurgo_ut.gpkg", layer = "mu_polys", delete_layer = TRUE)

# ==== APPEND COMPONENT DATA ===================================================

# All map unit component files
comp_files = list.files(
  path = "Data/Raw/SSURGO",
  pattern = "^comp\\.txt$",
  recursive = TRUE,
  full.names = TRUE
)

# Files with no data
comp_missing = comp_files[map_lgl(comp_files, ~ {
  df = read_delim(
    .x,
    delim = "|",
    col_names = FALSE,
    quote = "\"",
    trim_ws = TRUE,
    show_col_types = FALSE
  )
  nrow(df) == 0
})]

# Files to append
comp_files = setdiff(comp_files, comp_missing)

# Read and append map unit component data
comp = comp_files |>
  map(~ read_delim(
    .x,
    delim = "|",
    col_names = FALSE,
    trim_ws = TRUE,
    quote = "\""
  ) |> 
    transmute(
      mukey = as.character(X108),
      cokey = as.character(X109),
      hydro_condition = as.character(X22),
      hydro_group = as.character(X80),
      co_pct = X2 / 100
    ) |> 
    # Not all components add up to 100, so manually calculate weights
    group_by(mukey) |>
    mutate(co_pct_sum = sum(co_pct, na.rm = TRUE)) |>
    ungroup() |>
    mutate(co_pct = ifelse(co_pct_sum < 1, co_pct / co_pct_sum, co_pct)) |> 
    select(-co_pct_sum)
  ) |>
  bind_rows()

# Save appended map unit components as layer in .gpkg file
st_write(comp, "Data/Raw/SSURGO/ssurgo_ut.gpkg", layer = "comp", delete_layer = TRUE)

# ==== APPEND COMPONENT RESTRICTIONS DATA ======================================

# All component restrictions files
co_restrictions_files = list.files(
  path = "Data/Raw/SSURGO",
  pattern = "^crstrcts\\.txt$",
  recursive = TRUE,
  full.names = TRUE
)

# Files with no data
co_restrictions_missing = co_restrictions_files[map_lgl(co_restrictions_files, ~ {
  df = read_delim(
    .x,
    delim = "|",
    col_names = FALSE,
    quote = "\"",
    trim_ws = TRUE,
    show_col_types = FALSE
  )
  nrow(df) == 0
})]

# Files to append
co_restrictions_files = setdiff(co_restrictions_files, co_restrictions_missing)

# Read and append all component restrictions data
co_restrictions = co_restrictions_files |>
  map(~ read_delim(
    .x,
    delim = "|",
    col_names = FALSE,
    trim_ws = TRUE,
    quote = "\""
  ) |>
    transmute(
      cokey = as.character(X12),
      restrictive_layer_cm = X4
    )
  ) |>
  bind_rows()

# Save appended component restrictions data as layer in .gpkg file
st_write(co_restrictions, "Data/Raw/SSURGO/ssurgo_ut.gpkg", layer = "co_restrictions", delete_layer = TRUE)

# ==== APPEND MAP UNIT AGGATT DATA =============================================

# All map unit aggregated attribute files
mu_aggatt_files = list.files(
  path = "Data/Raw/SSURGO",
  pattern = "^muaggatt\\.txt$",
  recursive = TRUE,
  full.names = TRUE
)

# Files with no data
mu_aggatt_missing = mu_aggatt_files[map_lgl(mu_aggatt_files, ~ {
  df = read_delim(
    .x,
    delim = "|",
    col_names = FALSE,
    quote = "\"",
    trim_ws = TRUE,
    show_col_types = FALSE
  )
  nrow(df) == 0
})]

# Files to append
mu_aggatt_files = setdiff(mu_aggatt_files, mu_aggatt_missing)

# Read and append all map unit aggatt data
mu_aggatt = mu_aggatt_files |>
  map(~ read_delim(
    .x,
    delim = "|",
    col_names = FALSE,
    trim_ws = TRUE,
    quote = "\""
  ) |>
    transmute(
      mukey = as.character(X40),
      aws_cm = X15,
      water_table_cm = X8
    )
  ) |>
  bind_rows()

# Save appended map unit aggatt data as layer in .gpkg file
st_write(mu_aggatt, "Data/Raw/SSURGO/ssurgo_ut.gpkg", layer = "mu_aggatt", delete_layer = TRUE)
