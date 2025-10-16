
library(tidyverse)

# Directory to download and extract PRISM data to
dir = "Data/USU Method/PRISM Daily"

# Define start and end date to download daily data for
start_date = as.Date("2016-11-01")
end_date = as.Date("2024-10-31")

all_dates = seq(start_date, end_date, by = "day")

# Loop through each year and month
for (i in seq_along(all_dates)) {
  date = all_dates[i]
  
  # Format date as YYYYMMDD
  date_str = format(date, "%Y%m%d")
  year_str = format(date, "%Y")
  
  # Create download URL
  url = paste0(
    "https://data.prism.oregonstate.edu/time_series/us/an/800m/ppt/daily/",
    year_str, 
    "/prism_ppt_us_30s_", 
    date_str, 
    ".zip"
  )
  
  dest_zip = paste0(dir, "temp.zip")
  
  # Try downloading until successful
  success = FALSE
  attempt = 1
  
  while(!success && attempt <= 5) {
    tryCatch({
      download.file(url, dest_zip, quiet = TRUE, mode = "wb")
      
      # Check if file downloaded
      if (file.exists(dest_zip) && file.info(dest_zip)$size > 0) {
        success = TRUE
      } else {
        Sys.sleep(2)
      }
    }, error = function(e) {
      Sys.sleep(2)
    })
    attempt = attempt + 1
  }
  
  if (success) {
    tryCatch({
      # Unzip the file
      unzip(dest_zip, exdir = dir)
      
      # Remove temporary zip file
      file.remove(dest_zip)
      
      # Remove any extracted non-TIF files
      all_files = list.files(dir, full.names = TRUE)
      non_tif_files = all_files[!grepl("\\.tif$", all_files, ignore.case = TRUE)]
      if (length(non_tif_files) > 0) {
        file.remove(non_tif_files)
      }
      
    }, error = function(e) {
      cat("Error processing file for", date_str, ":", e$message, "\n")
    })
  } else {
    cat("Failed to download after 5 attempts:", date_str, "\n")
  }
}


library(terra)
files = list.files("Data/Raw/PRISM Daily/", full.names = TRUE)
file_dates <- as.Date(sub(".*_(\\d{8})\\.tif$", "\\1", files), format = "%Y%m%d")
files_to_delete <- files[file_dates < as.Date("2016-11-01")]
file.remove(files_to_delete)
