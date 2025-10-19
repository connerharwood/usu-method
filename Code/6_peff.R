
library(tidyverse)

# ==== LOAD ====================================================================

load("Data/Clean/Input Data/ssurgo.rda")
load("Data/Clean/Input Data/prism_daily.rda")

# ==== DAILY EFFECTIVE PRECIP ==================================================

peff_daily = left_join(
  prism_daily,
  ssurgo |> select(id, curve_number),
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
  ) |> 
  select(id, water_year, year, month, day, peff_in)

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
  left_join(peff_winter, by = c("id", "water_year")) |> 
  select(id, water_year, year, month, peff_in, peff_win_in)
  
# ==== SAVE ====================================================================

save(peff, file = "Data/Clean/Input Data/peff.rda")
