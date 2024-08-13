shared_mobility <-
  function(fleet,
           no_vehicles,
           no_trips,
           project_lifetime,
           project_start) {
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
    project_years <-
      seq(project_start, project_start + project_lifetime - 1)
    
    # Initialize vectors to store results
    new_service_vmt <- numeric(length(project_years))
    auto_vmt_displaced <- numeric(length(project_years))
    ghg_impact <- numeric(length(project_years))
    carbon_cost <- numeric(length(project_years))
    
    for (i in seq_along(project_years)) {
      year <- project_years[i]
      
      # Calculate new service vehicle miles traveled (VMT)
      new_service_vmt_year <-
        (no_vehicles * no_trips * ride_share_miles * trip_miles)
      
      
      # Calculate auto VMT displaced
      if (fleet == "Non-EV Rideshares" || fleet == "EV Rideshares") {
        vmt_displaced_year <-
          adjustment_factor * average_occupancy * trip_miles * (1 - prct_deadhead_miles)
      }
      
      if (fleet == "Bike" || fleet == "Scooter"){
        vmt_displaced_year <-
          adjustment_factor * average_occupancy * trip_miles
      }
      
      ###################################################################################################
      
      
      if (fleet == "Non-EV Rideshares") {
        ((vmt_displaced_year * greet_ef_year$gasoline))
      }
      
      if (fleet == "EV Rideshares") {
        ghg_impact_year <-
          ((vmt_displaced_year * greet_ef_year$electricity))
      }
      
      if (fleet == "Bike") {
        ghg_impact_year <-
          ((vmt_displaced_year))
      }
      
      if (fleet == "Scooter") {
        ghg_impact_year <-
          ((vmt_displaced_year * greet_ef_year$electricity))
      }
      
      #################################################################################################
      
      # Calculate social cost of carbon for the current year
      discount_rate <-
        SocialCostCarbon %>% filter(`emission.year` == year &
                                      gas == "CO2")
      social_cost_carbon <-
        ghg_impact_year * discount_rate$`2.0% Ramsey`
      
      # Store results for the current year
      new_service_vmt[i] <- new_service_vmt_year
      auto_vmt_displaced[i] <- auto_vmt_displaced_year
      ghg_impact[i] <- ghg_impact_year
      carbon_cost[i] <- social_cost_carbon
    }
    
    # Calculate total new_service_vmt and total_ghg_impact
    total_new_service_vmt <- sum(new_service_vmt)
    total_ghg_impact <- sum(ghg_impact)
    total_carbon_cost <- sum(carbon_cost)
    
    # Create a data frame with results including totals
    results <- data.frame(
      year = c(project_years, "Total"),
      new_service_vmt = c(new_service_vmt, total_new_service_vmt),
      auto_vmt_displaced = auto_vmt_displaced,
      ghg_impact = c(ghg_impact, total_ghg_impact),
      carbon_cost = c(carbon_cost, total_carbon_cost)
    )
    
    return(results)
  }
