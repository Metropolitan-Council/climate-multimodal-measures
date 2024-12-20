pedestrian_facilities <- function(average_daily_traffic,
                                  one_way_facility_length,
                                  no_key_destinations_25,
                                  no_key_destinations_50,
                                  location,
                                  project_start,
                                  project_lifetime,
                                  annual_use_days = NULL,
                                  average_trip_replaced = NULL) {
  
  community_type <- CommunityTypeShape %>%
    filter(CTU_NAME == location) %>%
    pull(MappedCommunity)
  
  if(is.null(annual_use_days)){
    annual_use_days = 214
  }
  
  if (is.null(average_trip_replaced)) {
    average_trip_replaced = TripDistances %>% filter(mode_type == "Walk") %>% pull(distance_avg)
  }
  
  # Use case_when for traffic range
  traffic_range <- case_when(
    average_daily_traffic <= 12000 ~ "1 to 12,000",
    average_daily_traffic <= 24000 ~ "12,001 to 24,000",
    average_daily_traffic <= 30000 ~ "24,001 to 30,000",
    TRUE ~ NA_character_
  )
  
  # Use case_when for facility length range
  facility_length_range <- case_when(
    one_way_facility_length == 1 ~ "1",
    one_way_facility_length > 1 &
      one_way_facility_length <= 2 ~ "1.01",
    one_way_facility_length > 2 ~ "2",
    TRUE ~ NA_character_
  )
  
  # Using dplyr to filter and pull the mode_shift_factor_m value
  mode_shift_factor <- ModeShiftFactor %>%
    filter(
      average_daily_traffic_vehicle_trips_per_day == traffic_range,
      one_way_facility_length_miles_low == facility_length_range
    ) %>%
    pull(mode_shift_factor_m)
  
  # Determine the category for the number of key destinations
  destination_category_25 <- case_when(
    no_key_destinations_25 <= 2 ~ "0 to 2",
    no_key_destinations_25 == 3 ~ "3",
    no_key_destinations_25 >= 4 & no_key_destinations_25 <= 6 ~ "4 to 6",
    no_key_destinations_25 >= 7 ~ "7 or more"
  )
  
  destination_category_50 <- case_when(
    no_key_destinations_50 <= 2 ~ "0 to 2",
    no_key_destinations_50 == 3 ~ "3",
    no_key_destinations_50 >= 4 & no_key_destinations_50 <= 6 ~ "4 to 6",
    no_key_destinations_50 >= 7 ~ "7 or more"
  )
  
  # Filter the CreditForKeyDestinations dataframe to get the relevant rows
  credit_25 <- CreditForKeyDestinations %>%
    filter(number_of_key_destinations == destination_category_25) %>%
    pull(credit_within_1_4_mile_of_facility_c)
  
  credit_50 <- CreditForKeyDestinations %>%
    filter(number_of_key_destinations == destination_category_50) %>%
    pull(credit_within_1_2_mile_of_facility_c)
  
  # Compare and assign the larger credit value
  key_destination_credit <- max(credit_25, credit_50, na.rm = TRUE)
  
  # Generate years project covers
  project_start <- lubridate::year(project_start)
  project_start <- as.numeric(project_start)
  project_years <- seq(project_start, project_start + project_lifetime - 1)
  
  # Initialize vectors to store results
  auto_vmt_displaced <- numeric(length(project_years))
  ghg_impact <- numeric(length(project_years))
  carbon_cost <- numeric(length(project_years))
  
  for (i in seq_along(project_years)) {
    year <- project_years[i]
    
    # Calculate VMT displaced for the current year
    vmt_displaced_year <- annual_use_days * average_daily_traffic * (mode_shift_factor + key_destination_credit) * average_trip_replaced
    
    
    # Filter GHG emission factor (EF) for the current year
    greet_ef_year <- GREETCarbonIntensity %>% filter(Year == year)
    
    discount_rate <- SocialCostCarbon %>% filter(`emission.year` == year &
                                                   gas == "CO2")
    
    current_year <- project_years[i]
    
    # Filter to community type provided
    FleetData <- FleetData %>% filter(MappedCommunity == community_type)
    
    FleetData <- FleetData %>% mutate(year = as.numeric(year))
    
    # # Determine the closest year
    closest_year <- FleetData %>%
      summarise(closest_year = year[which.min(abs(year - current_year))]) %>%
      pull(closest_year)
    
    # # Filter the data set to get the fleet proportions from the closest year
    fleet_proportion <- FleetData %>%
      filter(year == closest_year)
    
    diesel_ef_year <- DieselEFsCommunityType %>% filter(year == current_year) %>% filter(MappedCommunity == community_type) %>% pull(EF)
    
    gasoline_ef_year <- GasolineEFsCommunityType %>% filter(year == current_year) %>% filter(MappedCommunity == community_type) %>% pull(EF)
    
    ghg_impact_year <-
      ((
        vmt_displaced_year * gasoline_ef_year * fleet_proportion$gasoline
      ) +
        (
          vmt_displaced_year * diesel_ef_year * fleet_proportion$diesel
        ) +
        (
          vmt_displaced_year * greet_ef_year$electricity * fleet_proportion$electricity
        )
      ) / 1000000
    
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
    "VMT (Miles)" = round(c(auto_vmt_displaced, total_vmt_displaced), 0),
    "GHG Impact (kt CO₂)" = round(c(ghg_impact, total_ghg_impact), 1),
    "Carbon Cost ($)" = round(c(carbon_cost, total_carbon_cost), 0),
    check.names = FALSE
  )
  
  return(results)
}

# test<- pedestrian_facilities(average_daily_traffic = 6000,
# one_way_facility_length = 1.5,
# no_key_destinations_25 = 1,
# no_key_destinations_50 = 4,
# location = "Andover",
# project_start = "2024-01-01",
# project_lifetime = 1,
# annual_use_days = NULL,
# average_trip_replaced = NULL)
