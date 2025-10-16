
library(tidyverse)
library(sf)
library(data.table)
library(readxl)

# ==== LOAD ====================================================================

# Load 2017-2024 fields panel
fields_panel = st_read("Data/Clean/Fields/fields_panel.gpkg") |> 
  st_drop_geometry()

load("Data/Clean/Input Data/ssurgo.rda")
load("Data/Clean/Input Data/peff.rda")
load("Data/Clean/Input Data/openet_eemetric.rda")

# Load crop rooting zone depths
rooting_depth = read_excel("Data/Misc/rooting_depth.xlsx")

# ==== CALCULATE CARRYOVER SOIL MOISTURE =======================================

all_combos = expand_grid(
  id = unique(fields_panel$id),
  date = seq(as.Date("2016-11-01"), as.Date("2024-10-01"), by = "month")
) |> 
  mutate(
    year = year(date), 
    month = month(date),
    water_year = if_else(month >= 11, year + 1, year)
  ) |> 
  select(id, water_year, year, month)

merge = all_combos |> 
  left_join(
    fields_panel |> select(id, year, rz_in),
    by = c("id", "water_year" = "year"),
    relationship = "many-to-one"
  ) |> 
  left_join(
    peff |> select(-peff_in),
    by = c("id", "water_year", "year", "month"),
    relationship = "one-to-one"
  ) |> 
  left_join(
    openet_eemetric |> select(-et_in),
    by = c("id", "water_year", "year", "month"),
    relationship = "one-to-one"
  ) |> 
  left_join(
    ssurgo |> select(id, awc_in_in, max_rz_in, curve_number),
    by = "id",
    relationship = "many-to-one"
  ) |> 
  # Ensure that rooting depth doesn't exceed water table or bedrock
  mutate(rz_in = case_when(
    is.na(rz_in) ~ rz_in, # Keep NA as is
    !is.na(rz_in) & rz_in > max_rz_in ~ max_rz_in, # Set rz to restrictive layer if it exceeds it
    TRUE ~ rz_in # Keep all other cases as is
  ))

smco = merge |> 
  group_by(id, water_year) |> 
  # Calculate carryover soil moisture
  mutate(sm_co_in = pmin(peff_win_in - et_win_in, 0.75 * awc_in_in * rz_in)) |> 
  ungroup() |> 
  distinct(id, water_year, sm_co_in)

# ==== SAVE ====================================================================

save(smco, file = "Data/Clean/Input Data/smco.rda")
