
library(tidyverse)
library(sf)

# ==== LOAD ====================================================================

# Load 2017-2024 Utah fields panel
fields_panel = st_read("Data/Clean/Fields/fields_panel.gpkg") |> 
  st_drop_geometry()

load("Data/Clean/Depletion/masterdata.rda")

# ==== CALCULATE DEPLETION =====================================================

depletion_monthly = masterdata |> 
  filter(month %in% 4:10) |> 
  arrange(id, year, month) |> 
  group_by(id, year) |> 
  mutate(
    sm_start = pmax(0, smco_in[1] - cumsum(pmax(0, et_in - peff_in)) + pmax(0, et_in - peff_in)),
    
    sm_used = pmin(sm_start, pmax(0, et_in - peff_in)),
    
    sm_end = sm_start - sm_used,
    
    depletion_in = pmax(0, et_in - sm_start - peff_in),
    depletion_ft = depletion_in / 12,
    depletion_af = acres * depletion_ft
  ) |> 
  ungroup() |> 
  left_join(
    fields_panel,
    by = c("id", "year"),
    relationship = "many-to-one"
  ) |> 
  select(
    id,
    year,
    month,
    county,
    basin,
    sub_area,
    land_use,
    land_use_group,
    crop,
    crop_group,
    irr_method,
    acres = acres.x,
    et_in,
    peff_in,
    smco_in,
    depletion_in,
    depletion_ft,
    depletion_af
  )

depletion_annual = depletion_monthly |> 
  group_by(id, year) |> 
  summarize(
    et_in = sum(et_in, na.rm = FALSE), # Growing season ET (inches)
    peff_in = sum(peff_in, na.rm = FALSE), # Growing season effective precip (inches)
    smco_in = first(smco_in), # Carryover soil moisture (inches)
    
    depletion_in = sum(depletion_in, na.rm = FALSE), # Growing season depletion (inches)
    depletion_ft = sum(depletion_ft, na.rm = FALSE), # Growing season depletion (feet)
    depletion_af = sum(depletion_af, na.rm = FALSE), # Growing season depletion (acre-feet)
    .groups = "drop"
  ) |> 
  left_join(
    fields_panel,
    by = c("id", "year"),
    relationship = "one-to-one"
  ) |> 
  select(
    id,
    year,
    county,
    basin,
    sub_area,
    land_use,
    land_use_group,
    crop,
    crop_group,
    irr_method,
    acres,
    et_in,
    peff_in,
    smco_in,
    depletion_in,
    depletion_ft,
    depletion_af
  )

median(depletion_annual$depletion_ft, na.rm = TRUE) # 1.320243
median(depletion_annual$depletion_af, na.rm = TRUE) # 6.260489

# ==== SAVE ====================================================================

save(depletion_monthly, file = "Data/Clean/Depletion/depletion_monthly.rda")
save(depletion_annual, file = "Data/Clean/Depletion/depletion_annual.rda")
