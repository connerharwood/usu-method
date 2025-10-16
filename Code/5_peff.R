
library(tidyverse)
library(data.table)

# ==== LOAD ====================================================================

load("Data/USU Method/ssurgo.rda")
load("Data/USU Method/prism_daily.rda")

# ==== DAILY EFFECTIVE PRCP ====================================================
ssurgo = ssurgo |> select(id, curve_number)
gc()

prism_ssurgo = left_join(
  prism,
  ssurgo,
  by = "id",
  relationship = "many-to-one"
)
rm(prism, ssurgo)
gc()

peff1 = prism_ssurgo |> 
  mutate(max_retention_in = (1000 / curve_number) - 10)
rm(prism_ssurgo)
gc()

peff2 = peff1 |> 
  mutate(initial_abstractions_in = 0.2 * max_retention_in)
rm(peff1)
gc()
save(peff2, file = "Data/USU Method/peff2_temp.rda")
gc()

load("Data/USU Method/peff2_temp.rda")
gc()
peff3 = peff2 |> 
  mutate(runoff_in = if_else(
    prcp_in <= initial_abstractions_in, 0,
    (prcp_in - initial_abstractions_in)^2 / (prcp_in - initial_abstractions_in + max_retention_in)
  ))
rm(peff2)
gc()
peff4 = peff3 |> 
  mutate(peff_in_usu = prcp_in - runoff_in)
rm(peff3)
gc()

save(peff4, file = "Data/USU Method/peff_usu.rda")
peff5 = peff4 |> select(id, year, month, day, peff_in_usu)
rm(peff4)
gc()
save(peff5, file = "Data/USU Method/peff_usu_daily.rda")
peff_monthly_usu = peff5 |> 
  group_by(id, year, month) |> 
  summarize(peff_in_usu = sum(peff_in_usu, na.rm = FALSE))

save(peff_monthly_usu, file = "Data/USU Method/peff_usu_monthly.rda")
load("")

# ==== COMPARE =================================================================

load("Data/Clean/Depletion/Utah/masterdata.rda")

peff_monthly_jacobs = masterdata |> 
  select(id, year, month, peff_in_jacobs = peff_in)
rm(masterdata)
gc()

compare_peff = inner_join(
  peff_monthly_jacobs,
  peff_monthly_usu,
  by = c("id", "year", "month"),
  relationship = "one-to-one"
)

compare_peff = compare_peff |> 
  mutate(diff = peff_in_usu - peff_in_jacobs)

ggplot(compare_peff, aes(x = diff)) +
  geom_histogram()

ggplot(compare_peff, aes(x = peff_in_jacobs, y = peff_in_usu)) +
  geom_point()

compare_monthly_mean = compare_peff |> 
  group_by(year, month) |> 
  summarize(
    peff_in_jacobs = mean(peff_in_jacobs, na.rm = TRUE),
    peff_in_usu = mean(peff_in_usu, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(compare_monthly_mean, aes(x = peff_in_jacobs, y = peff_in_usu)) +
  geom_point() +
  geom_abline(slope = 1)

# ==== MONTHLY PEFF ============================================================

load("Data/USU Method/ssurgo.rda")
load("Data/Clean/Input Data/Utah/prism.rda")

gc()
prism_ssurgo = left_join(
  prism,
  ssurgo,
  by = "id",
  relationship = "many-to-one"
)
rm(prism, ssurgo)
gc()

peff1 = prism_ssurgo |> 
  mutate(max_retention_in = (1000 / curve_number) - 10)
rm(prism_ssurgo)
gc()

peff2 = peff1 |> 
  mutate(initial_abstractions_in = 0.2 * max_retention_in)
rm(peff1)
gc()

peff3 = peff2 |> 
  mutate(runoff_in = if_else(
    prcp_in <= initial_abstractions_in, 0,
    (prcp_in - initial_abstractions_in)^2 / (prcp_in - initial_abstractions_in + max_retention_in)
  ))
rm(peff2)
gc()
peff4 = peff3 |> 
  mutate(peff_in_usu = prcp_in - runoff_in)
rm(peff3)
gc()

peff_monthly_usu_actual = peff4 |> select(id, year, month, peff_in_usu)
save(peff_monthly_usu_actual, file = "Data/USU Method/peff_usu_monthly_actual.rda")

load("Data/USU Method/peff_usu_monthly_actual.rda")
load("Data/USU Method/peff_usu_monthly_sum.rda")

join = inner_join(
  peff_monthly_usu,
  peff_monthly_usu_actual,
  by = c("id", "year", "month"),
  relationship = "one-to-one"
)

compare_peff = join |>
  rename(peff_summed = peff_in_usu.x, peff_actual = peff_in_usu.y) |> 
  mutate(diff = peff_summed - peff_actual)

ggplot(compare_peff, aes(x = diff)) +
  geom_histogram()

sum = compare_peff |> 
  group_by(id, year) |> 
  summarize(
    peff_summed = sum(peff_summed, na.rm = TRUE),
    peff_actual = sum(peff_actual, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(sum, aes(x = peff_summed, y = peff_actual)) +
  geom_point() +
  geom_abline()

fields_panel = st_read("Data/Clean/Fields/Utah/fields_panel.shp") |> 
  filter(year == 2024)
library(sf)

gsl_basin = st_read("../")







load("Data/Clean/Depletion/Utah/masterdata.rda")
load("Data/USU Method/peff_usu_monthly_actual.rda")
load("Data/USU Method/peff_usu_monthly_sum.rda")

merge1 = inner_join(
  masterdata |> select(id, year, month, peff_jacobs = peff_in),
  peff_monthly_usu |> select(id, year, month, peff_usu_sum = peff_in_usu),
  by = c("id", "year", "month"),
  relationship = "one-to-one"
)

merge2 = inner_join(
  merge1,
  peff_monthly_usu_actual |> select(id, year, month, peff_usu_nonsum = peff_in_usu),
  by = c("id", "year", "month"),
  relationship = "one-to-one"
)

compare = merge2 |> 
  mutate(
    diff_sum = peff_jacobs - peff_usu_sum,
    diff_nonsum = peff_jacobs - peff_usu_nonsum,
    diff_usu = peff_usu_sum - peff_usu_nonsum
  )

ggplot(compare, aes(x = diff_sum)) +
  geom_histogram()
ggplot(compare, aes(x = diff_nonsum)) +
  geom_histogram()
ggplot(compare, aes(x = diff_usu)) +
  geom_histogram()
mean(compare$diff_sum, na.rm = TRUE)
mean(compare$diff_nonsum, na.rm = TRUE)
mean(compare$diff_usu, na.rm = TRUE)

ggplot(compare, aes(x = peff_jacobs, y = peff_usu_sum)) +
  geom_point() +
  geom_abline(slope = 1)

ggplot(compare, aes(x = peff_jacobs, y = peff_usu_nonsum)) +
  geom_point() +
  geom_abline(slope = 1)
