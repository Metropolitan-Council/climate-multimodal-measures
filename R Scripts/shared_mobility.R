# Remaining Questions:
# Do we want to specify location? To get proportion of fleet 
# confirm equation for vmt displaced from rideshares - 
# in the task 4 memo we the equation is A × O × (1- DM) - I think we are forgetting something 

shared_mobility <-
  function(fleet,
           no_vehicles,
           no_trips,
           project_lifetime,
           project_start,
           location) {
    
    if (fleet == "Bike" || fleet == "Scooter"){
      trip_miles = 1.4
      adjustment_factor = .5
      average_occupancy = 1
    }
    
    if (fleet == "Non-EV Rideshares" || fleet == "EV Rideshares") {
      trip_miles = 7.7
      adjustment_factor = .83
      prct_deadhead_miles = 0.4
      average_occupancy = 1.55
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
      
      print(paste("New service vmt:" , new_service_vmt))
      
      ##################################################################################################
      # Calculate auto VMT displaced
      
      if (fleet == "Non-EV Rideshares" || fleet == "EV Rideshares") {
        vmt_displaced_year <-
          adjustment_factor * average_occupancy * trip_miles * (1 - prct_deadhead_miles)
      }
      
      if (fleet == "Bike" || fleet == "Scooter"){
        vmt_displaced_year <-
          adjustment_factor * average_occupancy * trip_miles
      }
      
      print(paste("vmt displaced:" , vmt_displaced_year))
      ###################################################################################################
      # Calculate GHG impact
      greet_ef_year <- GREETCarbonIntensity %>% filter(Year == current_year)
      
      # Filter to CTU provided
      FleetData <- FleetData %>% filter(ctu == location)
      
      FleetData <- FleetData %>% mutate(year = as.numeric(year))
      
      # # Determine the closest year
      closest_year <- FleetData %>%
        summarise(closest_year = year[which.min(abs(year - current_year))]) %>%
        pull(closest_year)
      
      # # Filter the data set to get the fleet proportions from the closest year
      fleet_proportion <- FleetData %>%
        filter(year == closest_year)
      
      if (fleet == "Non-EV Rideshares") {
        ghg_impact_year <- ((vmt_displaced_year * greet_ef_year$gasoline * fleet_proportion$gasoline) +
                              (vmt_displaced_year * greet_ef_year$diesel * fleet_proportion$diesel) +
                              (vmt_displaced_year * greet_ef_year$electricity * fleet_proportion$electricity)) / 
          (new_service_vmt * greet_ef_year$gasoline)
      }
      
      
      if (fleet == "EV Rideshares") {
        ghg_impact_year <- ((vmt_displaced_year * greet_ef_year$gasoline * fleet_proportion$gasoline) +
                              (vmt_displaced_year * greet_ef_year$diesel * fleet_proportion$diesel) +
                              (vmt_displaced_year * greet_ef_year$electricity * fleet_proportion$electricity)) / 
          (new_service_vmt * greet_ef_year$electricity)
      }
      
      if (fleet == "Bike") {
        ghg_impact_year <- ((vmt_displaced_year * greet_ef_year$gasoline * fleet_proportion$gasoline) +
                              (vmt_displaced_year * greet_ef_year$diesel * fleet_proportion$diesel) +
                              (vmt_displaced_year * greet_ef_year$electricity * fleet_proportion$electricity))
      }
      
      if (fleet == "Scooter") {
        ghg_impact_year <- ((vmt_displaced_year * greet_ef_year$gasoline * fleet_proportion$gasoline) +
                              (vmt_displaced_year * greet_ef_year$diesel * fleet_proportion$diesel) +
                              (vmt_displaced_year * greet_ef_year$electricity * fleet_proportion$electricity))
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
    results <- data.frame(year = c(project_years, "Total"),
                          vmt_displaced = c(auto_vmt_displaced, total_vmt_displaced),
                          ghg_impact = c(ghg_impact, total_ghg_impact),
                          carbon_cost = c(carbon_cost, total_carbon_cost))
    
    return(results)
  }

# shared_mobility (fleet = 'EV Rideshares',
#            no_vehicles = 20,
#            no_trips = 50000,
#            project_lifetime = 10,
#            project_start = "2024-01-01",
#            location = "Andover")
