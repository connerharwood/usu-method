
library(tidyverse)
library(sf)
library(data.table)
library(glue)
library(terra)
library(exactextractr)

# ==== LOAD ====================================================================

# Load sample raster for aligning CRS
sample_rast = rast("Data/Raw/PRISM Daily/prism_ppt_us_30s_20241031.tif")

# Load 2024 fields
fields_sf = st_read("Data/Clean/Fields/fields_panel.gpkg") |> 
  # Filter to just 2024 fields
  filter(year == 2024) |> 
  # Select needed variables
  select(id, geometry = geom) |> 
  # Align CRS with PRISM raster
  st_transform(crs = crs(sample_rast))

# ==== EXTRACT DAILY PRECIP ====================================================

all_dates = seq(as.Date("2016-11-01"), as.Date("2024-10-31"), by = "day")

# Initialize list to store each year's data
all_prcp_list = list()

for (i in seq_along(all_dates)) {
  date = all_dates[i]
  date_str = format(date, "%Y%m%d")
  
  tif_path = glue("Data/Raw/PRISM Daily/prism_ppt_us_30s_{date_str}.tif")
  
  # Load raster
  prism_rast = rast(tif_path)
  
  # Extract each field's daily precip
  prcp_extract = exact_extract(
    # Extract raster values for current day's field poylgons
    prism_rast,
    fields_sf,
    
    # Calculate area-weighted mean precip across each field's pixels
    fun = "mean",
    
    # Return output as a dataframe
    force_df = TRUE,
    
    # Max number of raster cells to load
    max_cells_in_memory = 1000000000
  ) |>
    # Merge in field ID
    bind_cols(id = fields_sf$id) |>
    mutate(
      date = date,
      year = year(date), # Create year column
      month = month(date), # Create month column
      day = day(date), # Create day column
      prcp_in = mean / 25.4 # Convert precip from mm to in
    ) |>
    select(id, year, month, day, prcp_in) |> 
    # Set as data table for faster processing
    setDT()
  
  # Append current day into list of all days
  all_prcp_list[[i]] = prcp_extract
}

# Append list of each year's precip data into one dataframe
all_prcp = rbindlist(all_prcp_list) |> 
  # Create water year variable
  mutate(water_year = if_else(month >= 11, year + 1, year)) |> 
  select(id, water_year, year, month, day, prcp_in)

# Calculate winter precip for each field and water year
prcp_winter = all_prcp |> 
  # Filter to November through March
  filter(month %in% c(11, 12, 1, 2, 3)) |> 
  group_by(id, water_year) |> 
  summarize(prcp_win_in = sum(prcp_in, na.rm = FALSE), .groups = "drop")

# Rejoin winter precip with daily precip panel
prism_daily = all_prcp |> 
  left_join(prcp_winter, by = c("id", "water_year")) |> 
  select(id, water_year, year, month, day, prcp_in, prcp_win_in) |> 
  setDT()

# ==== SAVE ====================================================================

save(prism_daily, file = "Data/Clean/Input Data/prism_daily.rda")
