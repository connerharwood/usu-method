
library(tidyverse)
library(sf)
library(data.table)

# ==== LOAD ====================================================================

# Load 2017-2024 Utah fields panel
fields_panel = st_read("Data/Clean/Fields/fields_panel.gpkg") |> 
  st_drop_geometry()

load("Data/Clean/Input Data/openet_eemetric.rda")
load("Data/Clean/Input Data/peff.rda")
load("Data/Clean/Input Data/smco.rda")

# ==== MERGE ===================================================================

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

masterdata = all_combos |> 
  left_join(
    fields_panel,
    by = c("id", "water_year" = "year"),
    relationship = "many-to-one"
  ) |> 
  left_join(
    openet_eemetric |> select(id, water_year, year, month, et_in),
    by = c("id", "water_year", "year", "month"),
    relationship = "one-to-one"
  ) |> 
  left_join(
    peff |> select(id, water_year, year, month, peff_in),
    by = c("id", "water_year", "year", "month"),
    relationship = "one-to-one"
  ) |> 
  left_join(
    smco,
    by = c("id", "water_year"),
    relationship = "many-to-one"
  )
  
# ==== CALCULATE DEPLETION =====================================================

# Calculate monthly depletion for each field and growing month
depletion_monthly = masterdata |>
  # Filter to growing season months
  filter(month %in% 4:10) |>
  # Arrange data to calculate depletion sequentially
  arrange(id, year, month) |>
  # Group by field and year for monthly water balance model
  group_by(id, year) |>
  group_modify(~{
    # Initialize new columns
    df = .x |> mutate(sm_start = NA_real_, sm_end = NA_real_, depletion_in = NA_real_)
    
    # Soil moisture at start of irrigation season
    sm = df$sm_co_in[1]
    
    for (i in 1:nrow(df)) {
      et = df$et_in[i] # Monthly ET value
      peff = df$peff_in[i] # Monthly effective precip value
      df$sm_start[i] = sm # Store starting soil moisture
      
      # Depletion = actual ET - soil moisture - effective precip
      depletion = max(0, et - sm - peff)
      
      # Update soil moisture by subtracting ET that wasn't covered by effective precip
      sm_used = min(sm, max(0, et - peff))
      sm_new = max(0, sm - sm_used)
      
      # Store results
      df$sm_end[i] = sm_new
      df$depletion_in[i] = depletion
      
      # Carry over new soil moisture to next month
      sm = sm_new
    }
    
    # Return dataframe
    df
  }) |> 
  # Remove grouping structure
  ungroup() |> 
  mutate(
    # Convert month numbers to factored labels
    month = factor(
      month, 
      levels = 4:10,
      labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")
    ),
    # Convert depletion to feet and acre-feet
    depletion_ft = depletion_in / 12,
    depletion_af = acres * depletion_ft
  ) |> 
  # Drop unneeded variables
  select(-sm_start, -sm_end, -water_year) |> 
  # Convert to data table
  setDT()

# Sum up monthly depletions to annual (growing season) totals for each field
depletion_annual = depletion_monthly |>
  group_by(id, year) |>
  summarize(
    county = first(county), # County for this field
    acres = first(acres), # Acres for this field
    irr_method = first(irr_method), # Irrigation method for this field-year
    crop = first(crop), # Main crop for this field-year
    rz_in = first(rz_in), # Crop rooting zone depth for this field-year
    awc_in_in = first(awc_in_in), # Available water capacity for this field-year
    swsf = first(swsf), # Soil water storage factor for this field-year
    prcp_win_in = first(prcp_win_in), # Winter precip (inches)
    prcp_grow_in = sum(prcp_in, na.rm = FALSE), # Growing season precip (inches)
    et_win_in = first(et_win_in), # Winter ET (inches)
    sm_co_in = first(sm_co_in), # Winter carryover soil moisture (inches)
    peff_grow_in = first(peff_grow_in), # Growing season effective precip (inches)
    depletion_in = sum(depletion_in, na.rm = FALSE), # Growing season depletion (inches)
    depletion_ft = sum(depletion_ft, na.rm = FALSE), # Growing season depletion (feet)
    depletion_af = sum(depletion_af, na.rm = FALSE), # Growing season depletion (acre-feet)
    .groups = "drop"
  ) |> 
  # Convert to data table
  setDT()