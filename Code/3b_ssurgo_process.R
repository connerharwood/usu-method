
library(tidyverse)
library(sf)
library(data.table)
library(readxl)

# ==== LOAD ====================================================================

# Load fields panel
fields = st_read("Data/Clean/Fields/fields_panel.gpkg") |> 
  # Filter to 2024 fields
  filter(year == 2024) |> 
  # Transform to NAD83 for spatial operations
  st_transform(crs = 26912)

# Load map unit polygons
mu_polys = st_read("Data/Raw/SSURGO/ssurgo_ut.gpkg", layer = "mu_polys") |> 
  # Transform to NAD83 for spatial operations
  st_transform(crs = 26912)

# Load component tabular data
comp = st_read("Data/Raw/SSURGO/ssurgo_ut.gpkg", layer = "comp")

# Load component restrictions
co_restrictions = st_read("Data/Raw/SSURGO/ssurgo_ut.gpkg", layer = "co_restrictions")

# Load map unit aggregated attribute data
mu_aggatt = st_read("Data/Raw/SSURGO/ssurgo_ut.gpkg", layer = "mu_aggatt")

# Load curve numbers
curve_numbers = read_xlsx("Data/Misc/curve_numbers.xlsx")

# ==== PROCESS =================================================================

# Intersect SSURGO map units with area of interest
ssurgo_aoi = st_intersection(mu_polys, fields)

# Calculate component-weighted mean depth to restrictive layer for each map unit in inches
mu_restrictions = co_restrictions |> 
  # Join component keys with map unit keys and component percents
  left_join(comp, by = "cokey") |>
  # Calculate component-weighted mean depth to restrictive layer for each map unit in inches
  group_by(mukey) |> 
  summarize(
    restrictive_layer_in = weighted.mean(restrictive_layer_cm, w = co_pct, na.rm = FALSE) / 2.54,
    .groups = "drop"
  )

# Calculate mean curve number for each hydrologic group
cn_mean = curve_numbers |> 
  group_by(hydro_group) |> 
  summarize(mean_cn = mean(curve_number, na.rm = TRUE), .groups = "drop")
  
cn_mean = cn_mean |> 
  bind_rows(
    tibble(
      hydro_group = c("A/D", "B/D", "C/D"),
      mean_cn = c(
        mean(cn_mean$mean_cn[cn_mean$hydro_group %in% c("A","D")]),
        mean(cn_mean$mean_cn[cn_mean$hydro_group %in% c("B","D")]),
        mean(cn_mean$mean_cn[cn_mean$hydro_group %in% c("C","D")])
      )
    )
  )

# Calculate component-weighted hydrologic condition and group
mu_hydro = comp |>
  group_by(mukey) |> 
  summarize(
    hydro_condition = if_else(all(is.na(hydro_condition)), NA, paste(unique(na.omit(hydro_condition)), collapse = ", ")),
    hydro_group = hydro_group[which.max(co_pct)],
    .groups = "drop"
  ) |> 
  left_join(cn_mean, by = "hydro_group")

ssurgo_stats = ssurgo_aoi |> 
  # Join map units with their AWS and depth to water table
  left_join(mu_aggatt, by = "mukey", relationship = "many-to-one") |> 
  # Join map units with their depth to restrictive layer
  left_join(mu_restrictions, by = "mukey", relationship = "many-to-one") |> 
  # Join map units with their hydrologic condition and group
  left_join(mu_hydro, by = "mukey", relationship = "many-to-one") |> 
  mutate(
    # Compute area in acres of each map unit polygon
    mu_acres = as.numeric(st_area(geom)) / 4046.8564224,
    # Convert depth from cm to in
    water_table_in = water_table_cm / 2.54,
    # Convert available water storage from cm to in
    aws_in = aws_cm / 2.54
  )

ssurgo = ssurgo_stats |> 
  st_drop_geometry() |> 
  group_by(id) |> 
  summarize(
    # Calculate area-weighted mean AWS
    aws_in = weighted.mean(aws_in, w = mu_acres, na.rm = TRUE),
    # Calculate usable soil water storage (40% of AWS)
    usws_in = 0.4 * aws_in,
    # Calculate available water capacity
    awc_in_in = aws_in / 59,
    # Calculate soil water storage factor
    swsf = 0.531747 + 0.295164*usws_in - 0.057697*usws_in^2 + 0.003804*usws_in^3,
    # Calculate area-weighted mean depth to water table
    water_table_in = weighted.mean(water_table_in, w = mu_acres, na.rm = TRUE),
    # Calculate area-weighted mean depth to restrictive layer
    restrictive_layer_in = weighted.mean(restrictive_layer_in, w = mu_acres, na.rm = TRUE),
    
    hydro_condition = if_else(all(is.na(hydro_condition)), NA, paste(unique(na.omit(hydro_condition)), collapse = ", ")),
    hydro_group = hydro_group[which.max(mu_acres)],
    curve_number = weighted.mean(mean_cn, w = mu_acres, na.rm = TRUE),
    
    .groups = "drop"
  ) |> 
  mutate(
    # Max rooting depth possible based on water table or restrictive layer
    max_rz_in = pmin(water_table_in, restrictive_layer_in, na.rm = TRUE),
    # Convert NaN values to NA
    across(c(awc_in_in, swsf, max_rz_in), ~na_if(., NaN))
  ) |> 
  # Join each field with its SSURGO data
  right_join(fields, by = "id") |> 
  # Select needed variables
  select(id, awc_in_in, swsf, max_rz_in, hydro_condition, hydro_group, curve_number) |> 
  # Set as data table for faster processing
  setDT()

# ==== SAVE ====================================================================

save(ssurgo, file = "Data/Clean/Input Data/ssurgo.rda")
