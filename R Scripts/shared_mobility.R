shared_mobility <-
  function(fleet,
           no_vehicles,
           no_trips,
           project_lifetime,
           project_start,
           location,
           adjustment_factor = NULL,
           average_occupancy = NULL,
           trip_miles = NULL,
           prct_deadhead_miles = NULL) {
    
    community_type <- CommunityTypeShape %>% 
      filter(CTU_NAME == location) %>% 
      pull(MappedCommunity)
    
    # Assign Bike fleet
    if (fleet == "Bike") {
      if (is.null(trip_miles)) {
        trip_miles <- TripDistances %>% filter(mode_type == "Bicycle") %>% pull(distance_avg)
      }
      if (is.null(adjustment_factor)) {
        adjustment_factor <- 0.5
      }
      if (is.null(average_occupancy)) {
        average_occupancy <- 1
      }
    }
    
    # Assign Scooter fleet
    if (fleet == "Scooter") {
      if (is.null(trip_miles)) {
        trip_miles <- TripDistances %>% filter(mode_type == "Micromobility") %>% pull(distance_avg)
      }
      if (is.null(adjustment_factor)) {
        adjustment_factor <- 0.5
      }
      if (is.null(average_occupancy)) {
        average_occupancy <- 1
      }
    }
    
    # Assign Non-EV Rideshares and EV Rideshares fleet
    if (fleet == "Non-EV Rideshares" || fleet == "EV Rideshares") {
      if (is.null(trip_miles)) {
        trip_miles <- TripDistances %>% filter(mode_type == "Smartphone ridehailing service") %>% pull(distance_avg)
      }
      if (is.null(adjustment_factor)) {
        adjustment_factor <- 0.83
      }
      if (is.null(prct_deadhead_miles)) {
        prct_deadhead_miles <- 0.4
      }
      if (is.null(average_occupancy)) {
        average_occupancy <- 1.55
      }
    }
    
    # Generate years project covers
    project_start <- lubridate::year(project_start)
    project_start <- as.numeric(project_start)
    project_years <-
      seq(project_start, project_start + project_lifetime - 1)
    
    # Initialize vectors to store results
    new_service_vmt <- numeric(length(project_years))
    auto_vmt_displaced <- numeric(length(project_years))
    ghg_impact <- numeric(length(project_years))
    carbon_cost <- numeric(length(project_years))
    
    for (i in seq_along(project_years)) {
      current_year <- project_years[i]
      
      #################################################################################################
      # Calculate new service vmt
      
      if (fleet == "Non-EV Rideshares" || fleet == "EV Rideshares") {
      new_service_vmt = no_vehicles * no_trips * trip_miles 
      }
      
      if (fleet == "Bike" || fleet == "Scooter"){
        new_service_vmt = no_vehicles * no_trips * trip_miles  
      }
      ##################################################################################################
      # Calculate auto VMT displaced
      
      if (fleet == "Non-EV Rideshares" || fleet == "EV Rideshares") {
        vmt_displaced_year <-
          no_vehicles * no_trips * adjustment_factor * average_occupancy * trip_miles * (1 - prct_deadhead_miles)
      }
      
      if (fleet == "Bike" || fleet == "Scooter"){
        vmt_displaced_year <-
          no_vehicles * no_trips * adjustment_factor * average_occupancy * trip_miles
      }
      ###################################################################################################
      # Calculate GHG impact
      greet_ef_year <- GREETCarbonIntensity %>% filter(Year == current_year)
      
      # Filter to Community Type provided
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
      
      if (fleet == "Non-EV Rideshares") {
        ghg_impact_year <- (((vmt_displaced_year * gasoline_ef_year * fleet_proportion$gasoline) +
                              (vmt_displaced_year * diesel_ef_year * fleet_proportion$diesel) +
                              (vmt_displaced_year * greet_ef_year$electricity * fleet_proportion$electricity)) - 
          (new_service_vmt * greet_ef_year$gasoline)) / 1000000
      }
      
      
      if (fleet == "EV Rideshares") {
        ghg_impact_year <- (((vmt_displaced_year * gasoline_ef_year * fleet_proportion$gasoline) +
                               (vmt_displaced_year * diesel_ef_year * fleet_proportion$diesel) +
                              (vmt_displaced_year * greet_ef_year$electricity * fleet_proportion$electricity)) - 
          (new_service_vmt * greet_ef_year$electricity)) / 1000000
      }
      
      if (fleet == "Bike") {
        ghg_impact_year <- (((vmt_displaced_year * gasoline_ef_year * fleet_proportion$gasoline) +
                               (vmt_displaced_year * diesel_ef_year * fleet_proportion$diesel) +
                              (vmt_displaced_year * greet_ef_year$electricity * fleet_proportion$electricity))) / 1000000
      }
      
      if (fleet == "Scooter") {
        ghg_impact_year <- (((vmt_displaced_year * gasoline_ef_year * fleet_proportion$gasoline) +
                               (vmt_displaced_year * diesel_ef_year * fleet_proportion$diesel) +
                              (vmt_displaced_year * greet_ef_year$electricity * fleet_proportion$electricity))) / 1000000
      }
      
      #################################################################################################
      
      # Calculate social cost of carbon for the current year
      discount_rate <-
        SocialCostCarbon %>% filter(`emission.year` == current_year &
                                      gas == "CO2")
      social_cost_carbon <-
        ghg_impact_year * discount_rate$`2.0% Ramsey`
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
   title='Place holder text to explain Social Cost of Carbon'></i>" = 
        format(round(c(carbon_cost, total_carbon_cost), 0), big.mark = ","),
      check.names = FALSE
    )
    
    return(results)
  }