trails_bike_facilities <- function(average_daily_traffic,
                                   facility_length_range,
                                   no_key_destinations_25,
                                   no_key_destinations_50,
                                   facility_type,
                                   project_start,
                                   project_lifetime,
                                   days_open = NULL,
                                   length_trip_replaced_walking = NULL,
                                   length_trip_replaced_biking = NULL) {
  if (is.null(days_open)) {
    days_open = 214
  }
  
  if (is.null(length_trip_replaced_walking)) {
    length_trip_replaced_walking = 0.6
  }
  
  if (is.null(length_trip_replaced_biking)) {
    length_trip_replaced_biking = 3.6
  }
  
  growth_factor_adjustment <- switch(
      facility_type,
      "on_street" = 1,
      "new_multiuse" = 1.54,
      "conversion" = 0.54,
      1
    )
  
  traffic_range <- case_when(
    average_daily_traffic <= 12000 ~ "1 to 12,000",
    average_daily_traffic <= 24000 ~ "12,001 to 24,000",
    average_daily_traffic <= 30000 ~ "24,001 to 30,000",
    TRUE ~ NA_character_
  )
  
  facility_length_range <- case_when(
    one_way_facility_length == 1 ~ "1",
    one_way_facility_length > 1 &
      one_way_facility_length <= 2 ~ "1.01",
    one_way_facility_length > 2 ~ "2",
    TRUE ~ NA_character_
  )

  mode_shift_factor <- ModeShiftFactor %>%
      filter(
        average_daily_traffic_vehicle_trips_per_day == traffic_range,
        one_way_facility_length_miles_low == facility_length_range
      ) %>%
      pull(mode_shift_factor_m)
  
  # Generate years project covers based on project start date and length of project
  project_start <- lubridate::year(project_start)
  project_start <- as.numeric(project_start)
  project_years <- seq(project_start, project_start + project_lifetime - 1)
  
  
  # Initialize vectors to store results
  vmt_displaced <- numeric(length(project_years))
  ghg_impact <- numeric(length(project_years))
  carbon_cost <- numeric(length(project_years))
  
  # Calculate vmt, ghg, and social cost of carbon for each year of the project timeline
  for (i in seq_along(project_years)) {
    year <- project_years[i]
    
    # Calculate VMT displaced for the current year
    vmt_displaced_year <- annual_use_days * average_daily_traffic * (mode_shift_factor + key_destination_credit) * (
      length_trip_replaced_walking + growth_factor_adjustment * length_trip_replaced_biking
    )
    
    # Filter GHG emission factor (EF) for the current year
    greet_ef_year <- GREETCarbonIntensity %>% filter(Year == year)
    
    discount_rate <- SocialCostCarbon %>% filter(`emission.year` == year &
                                                   gas == "CO2")
    
    ghg_impact_year <- 
      ((vmt_displaced_year * greet_ef_year$gasoline * fleet_proportion$gasoline) +
         (vmt_displaced_year * greet_ef_year$diesel * fleet_proportion$diesel) +
         (vmt_displaced_year * greet_ef_year$electricity * fleet_proportion$electricity)) / 1000000
    
    social_cost_carbon <- ghg_impact_year * discount_rate$`2.0% Ramsey`
    
    # Store results for the current year
    auto_vmt_displaced[i] <- vmt_displaced_year
    ghg_impact[i] <- ghg_impact_year
    carbon_cost[i] <- social_cost_carbon
  }
  
  # Calculate total vmt_displaced and ghg_impact
  total_vmt_displaced <- sum(auto_vmt_displaced)
  total_ghg_impact <- sum(ghg_impact)
  total_carbon_cost <- sum(carbon_cost)
  
  # Create a data frame with results including totals
  results <- data.frame(
    year = c(project_years, "Total"),
    vmt_displaced = c(auto_vmt_displaced, total_vmt_displaced),
    ghg_impact = c(ghg_impact, total_ghg_impact),
    carbon_cost = c(carbon_cost, total_carbon_cost)
  )
  
  return(results)
}