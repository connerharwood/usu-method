
library(tidyverse)
library(sf)

# ==== LOAD ====================================================================

# Load 2017-2024 Utah fields panel
fields_panel = st_read("Data/Clean/Fields/fields_panel.gpkg") |> 
  st_drop_geometry()

load("Data/Clean/Input Data/openet_eemetric.rda")
load("Data/Clean/Input Data/peff.rda")
load("Data/Clean/Input Data/smco_rz.rda")

# ==== MERGE ===================================================================

# Merge all input datasets into one
merge = expand_grid(
  # Create crosswalk of all fields, years, and months
  id = unique(fields_panel$id),
  year = 2016:2024,
  month = 1:12
) |> 
  # Create water year variable
  mutate(water_year = if_else(month >= 11, year + 1, year)) |> 
  # Only include relevant months
  filter(water_year %in% 2017:2024) |> 
  # Join with field acres
  left_join(
    fields_panel |> select(id, year, acres),
    by = c("id", "water_year" = "year"),
    relationship = "many-to-one"
  ) |> 
  # Join with monthly ET data
  left_join(
    openet_eemetric |> select(id, water_year, year, month, et_in),
    by = c("id", "water_year", "year", "month"),
    relationship = "one-to-one"
  ) |> 
  # Join with monthly effective precipitation data
  left_join(
    peff |> select(id, water_year, year, month, peff_in),
    by = c("id", "water_year", "year", "month"),
    relationship = "one-to-one"
  ) |> 
  # Join with carryover soil moisture and rooting zone data
  left_join(
    smco_rz,
    by = c("id", "water_year"),
    relationship = "many-to-one"
  )
  
# ==== CALCULATE DEPLETION =====================================================
















# ==== METHOD 1 ================================================================

tic()
depletion_monthly = masterdata |> 
  filter(month %in% 4:10) |> 
  arrange(id, year, month) |> 
  group_by(id, year) |> 
  mutate(
    sm_start = pmax(0, sm_co_in[1] - cumsum(pmax(0, et_in - peff_in)) + pmax(0, et_in - peff_in)),
    
    sm_used = pmin(sm_start, pmax(0, et_in - peff_in)),
    
    sm_end = sm_start - sm_used,
    
    depletion_in = pmax(0, et_in - sm_start - peff_in)
    
  ) |> 
  ungroup()
toc()
# 57.251 seconds

depletion_annual = depletion_monthly |> 
  mutate(
    # Convert month numbers to factored labels
    # month = factor(
    #   month, 
    #   levels = 4:10,
    #   labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")
    # ),
    # Convert depletion to feet and acre-feet
    depletion_ft = depletion_in / 12,
    depletion_af = acres * depletion_ft
  ) |> 
  group_by(id, year) |> 
  summarize(
    depletion_in = sum(depletion_in, na.rm = FALSE), # Growing season depletion (inches)
    depletion_ft = sum(depletion_ft, na.rm = FALSE), # Growing season depletion (feet)
    depletion_af = sum(depletion_af, na.rm = FALSE), # Growing season depletion (acre-feet)
    .groups = "drop"
  )

median(depletion_annual$depletion_ft, na.rm = TRUE)
# 1.302917

# ==== METHOD 2 ================================================================

tic()
depletion_monthly = masterdata[
  month %in% 4:10
][
  order(id, year, month)
][, {
  # initialize output vectors
  sm_start = numeric(.N)
  sm_end   = numeric(.N)
  depletion_in = numeric(.N)
  
  # soil moisture at start of irrigation season
  sm = sm_co_in[1]
  
  for (i in seq_len(.N)) {
    et   = et_in[i]
    peff = peff_in[i]
    
    sm_start[i] = sm
    
    # depletion calculation
    depletion = max(0, et - sm - peff)
    sm_used   = min(sm, max(0, et - peff))
    sm_new    = max(0, sm - sm_used)
    
    sm_end[i]      = sm_new
    depletion_in[i] = depletion
    
    sm = sm_new  # carry over
  }
  
  .(month, et_in, peff_in, sm_co_in, acres, sm_start, sm_end, depletion_in)
}, by = .(id, year)]
toc()
# 20.451 seconds

depletion_annual = depletion_monthly |> 
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
  group_by(id, year) |> 
  summarize(
    depletion_in = sum(depletion_in, na.rm = FALSE), # Growing season depletion (inches)
    depletion_ft = sum(depletion_ft, na.rm = FALSE), # Growing season depletion (feet)
    depletion_af = sum(depletion_af, na.rm = FALSE), # Growing season depletion (acre-feet)
    .groups = "drop"
  )

median(depletion_annual$depletion_ft, na.rm = TRUE)
# 1.313533

# ==== METHOD 3 ================================================================

tic()
depletion_monthly = masterdata |>
  filter(month %in% 4:10) |>
  arrange(id, year, month) |>
  group_by(id, year) |>
  mutate(
    # Use accumulate to handle sequential logic
    balance = accumulate2(et_in, peff_in, 
                          .init = first(sm_co_in),
                          function(sm, et, peff) {
                            depletion = max(0, et - sm - peff)
                            sm_used = min(sm, max(0, et - peff))
                            sm_new = max(0, sm - sm_used)
                            sm_new
                          })[-1],  # Remove initial value
    
    sm_start = lag(balance, default = first(sm_co_in)),
    sm_end = balance,
    depletion_in = pmax(0, et_in - sm_start - peff_in)
  ) |>
  select(-balance) |>
  ungroup()
toc()
# 

depletion_annual = depletion_monthly |> 
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
  group_by(id, year) |> 
  summarize(
    depletion_in = sum(depletion_in, na.rm = FALSE), # Growing season depletion (inches)
    depletion_ft = sum(depletion_ft, na.rm = FALSE), # Growing season depletion (feet)
    depletion_af = sum(depletion_af, na.rm = FALSE), # Growing season depletion (acre-feet)
    .groups = "drop"
  )

median(depletion_annual$depletion_ft, na.rm = TRUE)
# 1.313533







































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

# ==== SAVE ====================================================================

save(depletion_monthly, file = "Data/Clean/Depletion/depletion_monthly.rda")
save(depletion_annual, file = "Data/Clean/Depletion/depletion_annual.rda")
