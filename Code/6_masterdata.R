
library(tidyverse)
library(sf)

# ==== LOAD ====================================================================

# Load 2017-2024 Utah fields panel
fields_panel = st_read("Data/Clean/Fields/fields_panel.gpkg") |> 
  st_drop_geometry()

load("Data/Clean/Input Data/ssurgo.rda")
load("Data/Clean/Input Data/prism_daily.rda")
load("Data/Clean/Input Data/openet_eemetric.rda")

# ==== EFFECTIVE PRECIP ========================================================

peff_daily = left_join(
  prism_daily,
  ssurgo,
  by = "id",
  relationship = "many-to-one"
) |> 
  mutate(
    # Calculate max water retention after runoff initiation
    max_retention_in = (1000 / curve_number) - 10,
    # Calculate abstractions
    initial_abstractions_in = 0.2 * max_retention_in,
    # Calculate runoff using SCS Curve Number Method
    runoff_in = if_else(
      prcp_in <= initial_abstractions_in, 
      0,
      (prcp_in - initial_abstractions_in)^2 / (prcp_in - initial_abstractions_in + max_retention_in)
    ),
    # Calculate daily effective precipitation for non-irrigated condition
    peff_in = prcp_in - runoff_in
  )

# Aggregate daily effective precip to monthly
peff_monthly = peff_daily |> 
  group_by(id, water_year, year, month) |> 
  summarize(
    peff_in = sum(peff_in, na.rm = FALSE),
    .groups = "drop"
  )

# Calculate total winter effective precip for each field
peff_winter = peff_monthly |> 
  # Filter to November through March
  filter(month %in% c(11, 12, 1, 2, 3)) |> 
  group_by(id, water_year) |> 
  summarize(peff_win_in = sum(peff_in, na.rm = FALSE), .groups = "drop")

peff = peff_monthly |> 
  left_join(peff_winter, by = c("id", "water_year"))

# Free up memory
rm(prism_daily, peff_daily, peff_monthly, peff_winter)
gc()

# ==== CARRYOVER SOIL MOISTURE =================================================

smco_rz = peff |> 
  left_join(
    fields_panel |> select(id, year, acres, rz_in),
    by = c("id", "water_year" = "year"),
    relationship = "many-to-one"
  ) |> 
  left_join(
    openet_eemetric,
    by = c("id", "water_year", "year", "month"),
    relationship = "one-to-one"
  ) |> 
  left_join(
    ssurgo,
    by = "id",
    relationship = "many-to-one"
  ) |> 
  # Ensure that rooting depth doesn't exceed water table or bedrock
  mutate(rz_in = case_when(
    is.na(rz_in) ~ rz_in, # Keep NA as is
    !is.na(rz_in) & rz_in > max_rz_in ~ max_rz_in, # Set rz to restrictive layer if it exceeds it
    TRUE ~ rz_in # Keep all other cases as is
  )) |> 
  group_by(id, water_year) |> 
  # Calculate carryover soil moisture for each field-water year
  mutate(smco_in = pmin(peff_win_in - et_win_in, 0.75 * awc_in_in * rz_in)) |> 
  ungroup()

# ==== MASTERDATA ==============================================================

masterdata = smco_rz |> 
  select(
    id,
    year,
    month,
    acres,
    et_in,
    peff_in,
    smco_in
  )

# ==== SAVE ====================================================================

save(masterdata, file = "Data/Clean/Depletion/masterdata.rda")
