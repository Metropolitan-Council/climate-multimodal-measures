# Load Minnesota Census Tract Population Data
# Attempts to fetch from tidycensus API first, falls back to cached RDS if API fails

population <- tryCatch(
  {
    message("Attempting to fetch population data from Census API...")
    
    pop_data <- get_acs(
      geography = "tract",
      table = "B01003",
      state = "MN",
      geometry = TRUE,
      cache_table = TRUE
    ) %>%
      sf::st_transform("+proj=longlat +datum=WGS84")
    
    pop_data <- ms_simplify(pop_data,
      keep = 0.05,
      keep_shapes = TRUE
    )
    
    message("Successfully fetched population data from Census API")
    pop_data
  },
  error = function(e) {
    warning("Census API call failed: ", e$message)
    message("Loading population data from cached file...")
    
    cached_file <- paste0(here::here(), "/data/raw/mn_population_census_tracts.rds")
    
    if (!file.exists(cached_file)) {
      stop(
        "Cached population data not found at: ", cached_file, "\n",
        "Please run data/fetch_census_data.R locally to create the cached data file."
      )
    }
    
    pop_data <- readRDS(cached_file)
    message("Successfully loaded population data from cache (", nrow(pop_data), " census tracts)")
    pop_data
  }
)
