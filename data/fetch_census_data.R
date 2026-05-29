# Fetch Census Data Script
# Run this script locally to fetch and save Minnesota census tract population data
# Requires: Census API key set via tidycensus::census_api_key()
# Output: data/raw/mn_population_census_tracts.rds

library(tidycensus)
library(sf)
library(rmapshaper)
library(here)

message("Fetching Minnesota census tract population data from 2019-2023 5-year ACS...")

population <- get_acs(
  geography = "tract",
  table = "B01003",
  state = "MN",
  geometry = TRUE,
  cache_table = TRUE
) %>%
  sf::st_transform("+proj=longlat +datum=WGS84")

message("Simplifying geometry...")

population <- ms_simplify(population,
  keep = 0.05,
  keep_shapes = TRUE
)

output_path <- paste0(here::here(), "/data/raw/mn_population_census_tracts.rds")

message("Saving to: ", output_path)

saveRDS(population, output_path)

message("Successfully saved ", nrow(population), " census tracts to ", output_path)
