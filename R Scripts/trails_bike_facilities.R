trails_bike_facilities <- function(average_daily_traffic,
                                   facility_length_range,
                                   no_key_destinations_25,
                                   no_key_destinations_50,
                                   facility_type,
                                   location,
                                   project_start,
                                   project_lifetime,
                                   days_open = NULL,
                                   length_trip_replaced_biking = NULL) {
  community_type <- CommunityTypeShape %>% 
    filter(CTU_NAME == location) %>% 
    pull(MappedCommunity)
  
  if (is.null(days_open)) {
    days_open = 214
  }
  
  if (is.null(length_trip_replaced_biking)) {
    length_trip_replaced_biking = 3.6
  }
  
  growth_factor_adjustment <- switch(
    facility_type,
    "On Street" = 1,
    "New Multiuse" = 1.54,
    "Conversion" = 0.54,
    1
  )
  
  traffic_range <- case_when(
    average_daily_traffic <= 12000 ~ "1 to 12,000",
    average_daily_traffic <= 24000 ~ "12,001 to 24,000",
    average_daily_traffic <= 30000 ~ "24,001 to 30,000",
    TRUE ~ NA_character_
  )
  
  facility_length_range <- case_when(
    facility_length_range == 1 ~ "1",
    facility_length_range > 1 &
      facility_length_range <= 2 ~ "1.01",
    facility_length_range > 2 ~ "2",
    TRUE ~ NA_character_
  )
  
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
  
  print(key_destination_credit)
  
  # Generate years project covers based on project start date and length of project
  project_start <- as.numeric(project_start)  # Ensure numeric year
  project_years <- seq(project_start, project_start + project_lifetime - 1)  # Create year range
  
  
  # Initialize vectors to store results
  auto_vmt_displaced <- numeric(length(project_years))
  ghg_impact <- numeric(length(project_years))
  carbon_cost <- numeric(length(project_years))
  
  # Calculate vmt, ghg, and social cost of carbon for each year of the project timeline
  for (i in seq_along(project_years)) {
    current_year <- project_years[i]

    # Calculate VMT displaced for the current year
    vmt_displaced_year <- days_open * average_daily_traffic *
      (mode_shift_factor + key_destination_credit) *
      (growth_factor_adjustment * length_trip_replaced_biking)
    
    # Filter GHG emission factor (EF) for the current year
    greet_ef_year <- GREETCarbonIntensity %>% filter(Year == current_year)
    
    # Filter Discount Rate for the current year
    discount_rate <- SocialCostCarbon %>% 
      filter(`emission.year` == current_year & gas == "CO2")
    
    # Filter to Community Stype provided
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
    Year = c(project_years, "Total"),
    "VMT Reduction (Miles)" = format(round(c(auto_vmt_displaced, total_vmt_displaced), 0), big.mark = ","),
    "GHG Reduction (MT CO₂)" = format(round(c(ghg_impact, total_ghg_impact), 0), big.mark = ","),
    "Carbon Cost Reduction ($) <i class='fas fa-question-circle' 
   title='The Social Cost of Carbon estimates the economic savings from avoiding one ton of CO₂ emissions, reflecting reduced damages to agriculture, human health, infrastructure, and ecosystems. Using a 2% Ramsey discount rate, future damages are valued at 98% of their present value.'></i>" = 
      format(round(c(carbon_cost, total_carbon_cost), 0), big.mark = ","),
    check.names = FALSE
  )
  
  return(results)
}
