
library(tidyverse)
library(sf)
library(terra)
library(exactextractr)
library(readxl)

# ==== PROCESS CDL DATA ========================================================

# Load crop rooting zone depths
rooting_depth = read_excel("Data/Misc/rooting_depth.xlsx", sheet = "wrlu")

# Load CDL codes
cdl_codes = read_excel("Data/Misc/rooting_depth.xlsx", sheet = "cdl") |> select(cdl_code, crop)

# Load sample rast to transform fields CRS
sample_rast = rast("Data/Raw/CDL/CDL_2024_49.tif")

# Load temporary fields panel
fields_panel_temp = st_read("Data/Clean/Fields/Temp/fields_panel_temp.gpkg") |> 
  # Align CRS with CDL rasters
  st_transform(crs = crs(sample_rast))

# Filter to fields missing crop info
fields_crop_na = fields_panel_temp |> filter(is.na(crop))

# List of yearly CDL TIFF files
tiff_files = list.files("Data/Raw/CDL", full.names = TRUE)

# Initialize list to store each year
cdl_panel = list()

# Loop through and process each TIFF file
for (file in tiff_files) {
  # Extract year from TIFF file
  current_year = as.integer(str_extract(file, "\\d{4}"))
  
  # Filter fields to current year
  fields_current = fields_crop_na |> filter(year == current_year)
  
  # Load CDL TIFF file
  cdl_rast = rast(file)
  
  # Extract mode crop for each field
  cdl_extract = exact_extract(
    # Extract raster values for current year's field polygons
    cdl_rast,
    fields_current,
    
    # Area-weighted majority crop over a field's pixels
    fun = "majority",
    
    # Return output as a dataframe
    force_df = TRUE,
    
    # Max number of raster cells to load
    max_cells_in_memory = 1000000000
  )
  
  # Build yearly table
  cdl_fields = cdl_extract |> 
    # Merge in field ID
    bind_cols(id = fields_current$id) |> 
    # Create year variable
    mutate(year = current_year) |> 
    # Rename extracted column
    rename(cdl_code = majority)
  
  # Store current year in list
  cdl_panel[[as.character(current_year)]] = cdl_fields
}

# ==== HARMONIZE WRLU AND CDL CROPS ============================================

# Join fields panel with CDl data
fields_panel = fields_panel_temp |> 
  # Merge with unnested CDL data
  left_join(
    bind_rows(cdl_panel), 
    by = c("id", "year"),
    relationship = "one-to-one"
  ) |>
  # Merge with rooting zone depths
  left_join(
    cdl_codes, 
    by = "cdl_code",
    relationship = "many-to-one"
  ) |> 
  mutate(
    crop.y = case_when(
      crop.y == "Alfalfa" ~ "Alfalfa",
      crop.y == "Apples" ~ "Apples",
      crop.y == "Apricots" ~ "Apricots",
      crop.y == "Barley" ~ "Barley",
      crop.y == "Barren" ~ "Fallow/Idle",
      crop.y == "Cherries" ~ "Cherries",
      crop.y == "Chick Peas" ~ "Beans",
      crop.y == "Corn" ~ "Corn",
      crop.y == "Dbl Crop Triticale/Corn" ~ "Corn",
      crop.y == "Dbl Crop WinWht/Corn" ~ "Corn",
      crop.y == "Deciduous Forest" ~ "Pasture",
      crop.y == "Developed/Low Intensity" ~ "Pasture",
      crop.y == "Developed/Med Intensity" & year %in% c(2017:2020, 2022) ~ "Pasture",
      crop.y == "Developed/Med Intensity" & year %in% c(2021, 2023, 2024) ~ "Vegetables",
      crop.y == "Developed/Open Space" ~ "Pasture",
      crop.y == "Dry Beans" ~ "Beans",
      crop.y == "Evergreen Forest" ~ "Pasture",
      crop.y == "Fallow/Idle Cropland" ~ "Fallow/Idle",
      crop.y == "Flaxseed" ~ "Flaxseed",
      crop.y == "Grass/Pasture" ~ "Grass Hay",
      crop.y == "Herbaceous Wetlands" ~ "Pasture",
      crop.y == "Herbs" ~ "Horticulture",
      crop.y == "Millet" ~ "Grain/Seeds Unspecified",
      crop.y == "Mint" ~ "Horticulture",
      crop.y == "Misc Vegs & Fruits" ~ "Vegetables",
      crop.y == "Mixed Forest" ~ "Pasture",
      crop.y == "Mustard" ~ "Mustard",
      crop.y == "Oats" ~ "Oats",
      crop.y == "Onions" ~ "Onion",
      crop.y == "Other Crops" ~ "Field Crop Unspecified",
      crop.y == "Other Hay/Non Alfalfa" ~ "Grass Hay",
      crop.y == "Peaches" ~ "Peaches",
      crop.y == "Pears" ~ "Peaches",
      crop.y == "Peas" ~ "Vegetables",
      crop.y == "Potatoes" ~ "Potato",
      crop.y == "Pumpkins" ~ "Pumpkins",
      crop.y == "Rye" ~ "Rye",
      crop.y == "Safflower" ~ "Safflower",
      crop.y == "Shrubland" ~ "Fallow/Idle",
      crop.y == "Sod/Grass Seed" ~ "Grass Hay",
      crop.y == "Sorghum" ~ "Sorghum",
      crop.y == "Spring Wheat" ~ "Spring Wheat",
      crop.y == "Squash" ~ "Squash",
      crop.y == "Sunflower" ~ "Sunflower",
      crop.y == "Sweet Corn" ~ "Corn",
      crop.y == "Triticale" ~ "Triticale",
      crop.y == "Watermelons" ~ "Watermelons",
      crop.y == "Winter Wheat" ~ "Winter Wheat",
      crop.y == "Woody Wetlands" ~ "Pasture",
      TRUE ~ crop.y
    )
  ) |> 
  # Assign missing WRLU crop as CDL crop
  mutate(crop = ifelse(is.na(crop.x), crop.y, crop.x)) |> 
  # Join with crop rooting zone depth
  left_join(
    rooting_depth,
    by = "crop",
    relationship = "many-to-one"
  ) |> 
  # Select needed variables
  select(
    id,
    year,
    county,
    basin,
    sub_area,
    land_use,
    acres,
    cultivated,
    crop,
    crop_group,
    land_use_group,
    rz_in,
    irr_method,
    geometry = geom
  ) |>
  # Transform to WGS 84
  st_transform(crs = 4326)

# ==== SAVE ====================================================================

#st_write(fields_panel, "Data/Clean/Fields/fields_panel.shp", delete_layer = TRUE)
st_write(fields_panel, "Data/Clean/Fields/fields_panel.gpkg", delete_layer = TRUE)
