
library(tidyverse)
library(sf)
library(readxl)
library(glue)

# ==== REPAIR WRLU GEOMETRY ====================================================

for (year in 2017:2024) {
  # Full path of raw shapefile to fix
  shapefile_raw = list.files(
    path = file.path("Data/Raw/WRLU/Raw", year),
    pattern = paste0(".*", year, ".*\\.shp$"),
    full.names = TRUE,
    recursive = FALSE
  )
  
  # Full path to write fixed shapefile to
  shapefile_fixed = file.path("Data/Raw/WRLU/Repaired", year, paste0("wrlu_", year, ".shp"))
  
  # Use ogr2ogr to validate geometry of raw shapefile, saving as new shapefile
  system(paste(
    "ogr2ogr -f 'ESRI Shapefile'",
    shQuote(shapefile_fixed),
    shQuote(shapefile_raw),
    "-makevalid"
  ))
}
# ==== LOAD ====================================================================

# Initialize list to store each WRLU year
wrlu_list = list()

# Load and process each WRLU year
for (yr in 2017:2024) {
  # Shapefile path
  file_path = glue("Data/Raw/WRLU/Repaired/{yr}/wrlu_{yr}.shp")
  
  # Load and process 2024 separately
  if (yr == 2024) {
    # Load shapefile
    wrlu_2024 = st_read(file_path) |> 
      # Only include agriculture fields in Utah
      filter(Landuse == "Agricultural", State == "Utah") |> 
      # Select and rename needed variables
      select(
        id = OBJECTID,
        county = County,
        basin = Basin,
        sub_area = SubArea,
        land_use = Landuse,
        acres_2024 = Acres,
        crop_2024 = Descriptio,
        cdl_2024 = Class_Name,
        crop_group_2024 = CropGroup,
        land_use_group_2024 = LU_Group,
        irr_method_2024 = IRR_Method,
        geometry
      ) |> 
      # Make geometry valid
      st_make_valid() |> 
      # Transform to NAD 83 for spatial operations
      st_transform(crs = 26912) |> 
      # Create field centroid for joining with previous years
      mutate(centroid = st_centroid(geometry))
  } else {
    # Load shapefile
    wrlu = st_read(file_path) |> 
      # Only include agriculture fields in Utah
      filter(Landuse == "Agricultural", State == "Utah") |> 
      # Select and rename needed variables
      select(
        !!glue("acres_{yr}") := Acres,
        !!glue("crop_{yr}") := Descriptio,
        !!glue("cdl_{yr}") := Class_Name,
        !!glue("crop_group_{yr}") := CropGroup,
        !!glue("land_use_group_{yr}") := LU_Group,
        !!glue("irr_method_{yr}") := IRR_Method,
        geometry
      ) |> 
      # Make geometry valid
      st_make_valid() |> 
      # Transform to NAD 83 for spatial operations
      st_transform(crs = 26912)
    
    # Store in list with name by year
    wrlu_list[[as.character(yr)]] = wrlu
  }
}

# ==== BUILD PANEL =============================================================

# Define 2024 fields as base polygons
wrlu_base = wrlu_2024

# Loop through each year, spatially joining with 2024 fields
for (yr in names(wrlu_list)) {
  # Get current year's WRLU sf object
  wrlu_current = wrlu_list[[yr]]
  
  # Join current year's fields with 2024's field centroids
  wrlu_base = st_join(
    wrlu_base,
    wrlu_current,
    join = st_intersects # Find fields that intersect with 2024 centroids
  )
  
  # Create dynamic columns for current year
  acres_col = paste0("acres_", yr)
  crop_col = paste0("crop_", yr)
  cdl_col = paste0("cdl_", yr)
  crop_group_col = paste0("crop_group_", yr)
  land_use_group_col = paste0("land_use_group_", yr)
  irr_method_col = paste0("irr_method_", yr)
  
  wrlu_base = wrlu_base |>
    # Calculate difference in acres between current year and 2024 field
    mutate(acres_diff = abs(acres_2024 - .data[[acres_col]])) |> 
    group_by(id) |> 
    # For each field, keep the intersecting field with smallest difference in acreage
    slice_min(order_by = acres_diff, n = 1, with_ties = FALSE) |> 
    ungroup() |> 
    mutate(
      # Replace current year's entry with NA if acreage difference is too big
      !!crop_col := if_else(acres_diff > 0.01, NA, .data[[crop_col]]),
      !!cdl_col := if_else(acres_diff > 0.01, NA, .data[[cdl_col]]),
      !!crop_group_col := if_else(acres_diff > 0.01, NA, .data[[crop_group_col]]),
      !!land_use_group_col := if_else(acres_diff > 0.01, NA, .data[[land_use_group_col]]),
      !!irr_method_col := if_else(acres_diff > 0.01, NA, .data[[irr_method_col]])
    )
  
  # Pivot to long format
  wrlu_panel = wrlu_base |> 
    pivot_longer(
      cols = matches("^(crop(_group)?|cdl|land_use_group|irr_method)_"),
      names_to = c(".value", "year"),
      names_pattern = "(.*)_(\\d{4})"
    ) |> 
    # Convert year to integer and crop to title case
    mutate(year = as.integer(year), crop = str_to_title(crop))
}

# ==== HARMONIZE WRLU CROPS ACROSS YEARS =======================================

# Count number of crops per year to analyze changes in labels across years
crop_count = wrlu_panel |> 
  st_drop_geometry() |> 
  group_by(crop, year) |> 
  count() |> 
  pivot_wider(names_from = year, values_from = n)

cdl_count = wrlu_panel |> 
  st_drop_geometry() |> 
  group_by(cdl, year) |> 
  count() |> 
  pivot_wider(names_from = year, values_from = n)

# Fields with Grass Hay as crop in 2024
grass_hay_fields = wrlu_panel |> 
  filter(year == 2024, crop == "Grass Hay")

# Harmonize WRLU crops across years
wrlu_harmonized = wrlu_panel |> 
  mutate(crop = case_when(
    # Overwrite 2017-2023 Alfalfa crop with Grass Hay if 2024 crop was Grass Hay
    id %in% grass_hay_fields$id & year < 2024 & crop == "Alfalfa" ~ "Grass Hay",
    # Put Fallow, Idle, and Idle Pasture into one category
    crop %in% c("Fallow", "Idle", "Idle Pasture") ~ "Fallow/Idle",
    # Put Turfgrass into Turfgrass Ag
    crop == "Turfgrass" ~ "Turfgrass Ag",
    # Respell Grassy Hay to Grass Hay
    crop == "Grassy Hay" ~ "Grass Hay",
    # Align CDL name with WRLU name
    crop == "Grassland/Pasture" ~ "Grass/Pasture",
    # Set Dry Land/Other as Fallow/Idle
    crop == "Dry Land/Other" ~ "Fallow/Idle",
    TRUE ~ crop
  ))

# Count number of crops per year after harmonizing
crop_count_harmonized = wrlu_harmonized |> 
  st_drop_geometry() |> 
  group_by(crop, year) |> 
  count() |> 
  pivot_wider(names_from = year, values_from = n)

# Load CDL crops in Utah
cdl_crops = read_excel("Data/Misc/rooting_depth.xlsx", sheet = "cdl") |> 
  filter(utah == 1)

cdl_wrlu_proportions = wrlu_harmonized |> 
  st_drop_geometry() |> 
  mutate(cdl = if_else(cdl == "Grassland/Pasture", "Grass/Pasture", cdl)) |> 
  group_by(year, crop_cdl = cdl, crop_wrlu = crop) |> 
  tally(name = "field_count") |> 
  filter(crop_cdl %in% cdl_crops$crop) |> 
  group_by(year, crop_cdl) |> 
  mutate(prop = field_count / sum(field_count)) |> 
  ungroup() |> 
  select(-field_count) |> 
  pivot_wider(
    names_from = year,
    values_from = prop,
    values_fill = 0
  )

# ==== FINALIZE PRE-CDL PANEL ==================================================

# Finalize 2017-2024 fields panel
fields_panel_temp = wrlu_harmonized |> 
  # Select needed variables
  select(
    id,
    year,
    county,
    basin,
    sub_area,
    land_use,
    acres = acres_2024,
    crop,
    crop_group,
    land_use_group,
    irr_method,
    geometry
  ) |> 
  # Order each field by descending year
  arrange(id, desc(year)) |> 
  # Transform to WGS 84
  st_transform(crs = 4326)

# ==== SAVE ====================================================================

# Save as temporary fields panel (final panel will have CDL data for missing crops)
st_write(fields_panel_temp, "Data/Clean/Fields/Temp/fields_panel_temp.gpkg", delete_layer = TRUE)
