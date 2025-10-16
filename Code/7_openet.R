
library(tidyverse)
library(sf)
library(data.table)
library(glue)
library(terra)
library(exactextractr)

# ==== LOAD ====================================================================

# Load sample raster for aligning CRS
sample_rast = rast("Data/Raw/OpenET/utah_eemetric_2024_10.tif")

# Load 2024 fields
fields_sf = st_read("Data/Clean/Fields/fields_panel.gpkg") |> 
  # Filter to just 2024 fields
  filter(year == 2024) |> 
  # Select needed variables
  select(id, geometry = geom) |> 
  # Align CRS with ET raster
  st_transform(crs = crs(sample_rast))

# ==== EXTRACT MONTHLY ET ======================================================

# Initialize list to store each year's data
all_et_list = list()

for (year in 2016:2024) {
  # Initialize list for current year's monthly data
  year_list = list()
  
  # Determine which months to process for current year (data is 11/2016-10/2024)
  if (year == 2016) {
    months = 11:12 # Nov-Dec for 2016
  } else if (year == 2024) {
    months = 1:10 # Jan-Oct for 2024
  } else {
    months = 1:12 # All months for 2017-2023
  }
  
  for (month in months) {
    print(glue("Extracting for year {year}, month {month}"))
    
    # Load OpenET raster for current year
    openet_rast = rast(glue("Data/Raw/OpenET/utah_eemetric_{year}_{month}.tif"))
    
    # Extract each field's monthly ET
    et_extract = exact_extract(
      # Extract raster values for current month's field polygons
      openet_rast, 
      fields_sf, 
      
      # Calculate area-weighted mean ET across each field's pixels
      fun = "mean", 
      
      # Return output as a dataframe
      force_df = TRUE,
      
      # Max number of raster cells to load
      max_cells_in_memory = 1000000000
    ) |> 
      # Merge in field ID
      bind_cols(id = fields_sf$id) |> 
      mutate(
        year = year, # Create year column
        month = month, # Create month column
        et_in = mean / 25.4, # Convert ET from mm to in
      ) |> 
      select(id, year, month, et_in) |> 
      # Set as data table for faster processing
      setDT()
    
    # Store current month in list
    year_list[[as.character(month)]] = et_extract
  }
  
  # Combine all months for current year
  all_et_list[[as.character(year)]] = rbindlist(year_list)
}

# Append list of each year's ET data into one dataframe
all_et = rbindlist(all_et_list) |> 
  # Create water year variable
  mutate(water_year = if_else(month >= 11, year + 1, year)) |> 
  select(id, water_year, year, month, et_in)

# Calculate winter ET for each field and water year
et_winter = all_et |> 
  # Filter to November through March
  filter(month %in% c(11, 12, 1, 2, 3)) |> 
  group_by(id, water_year) |> 
  summarize(et_win_in = sum(et_in, na.rm = FALSE), .groups = "drop")

# Rejoin winter and growing season ET with monthly ET panel
openet_eemetric = all_et |> 
  left_join(et_winter, by = c("id", "water_year")) |> 
  select(
    id, 
    water_year,
    year, 
    month, 
    et_in, 
    et_win_in
  ) |> 
  # Set as data table for faster processing
  setDT()

# ==== SAVE ====================================================================

save(openet_eemetric, file = "Data/Clean/Input Data/openet_eemetric.rda")
