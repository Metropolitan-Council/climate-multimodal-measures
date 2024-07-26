shared_mobility <-
  function(no_bikes,
           no_cars,
           average_annual_trips_bike,
           average_annual_trips_ride,
           project_lifetime,
           project_start) {
    
    bike_trip_miles <- 1.4
    ride_share_miles <- 7.7
    adjustment_factor <- #Short or long distance 
    
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
      new_service_vmt_year_ride <-
        (no_cars * average_annual_trips_ride * ride_share_miles)
      
      new_service_vmt_year_bike <-
        (no_bikes * average_annual_trips_bike * bike_trip_miles) +
        
        total_new_service_vmt <-
        new_service_vmt_year_ride + new_service_vmt_year_ride
      
      # Calculate auto VMT displaced
      vmt_displaced_year <-
        adjustment_factor * average_occupancy * prct_deadhead_miles
      
      # Calculate GHG impact for the current year (assuming carbon_intensity is available)
      ghg_impact_year <-
        ((
          vmt_displaced_year * greet_ef_year$gasoline * fleet_proportion$gasoline
        ) +
          (
            vmt_displaced_year * greet_ef_year$diesel * fleet_proportion$diesel
          ) +
          (
            vmt_displaced_year * greet_ef_year$compressed_natural_gas * fleet_proportion$compressed_natural_gas
          ) +
          (
            vmt_displaced_year * greet_ef_year$electricity * fleet_proportion$electricity
          )
        )
      # -
      #adjust logic based on their proposed fleet
      # ((
      #   new_service_vmt_year * greet_ef_year$gasoline * fleet_proportion$gasoline
      # ) +
      #   (
      #     new_service_vmt_year * greet_ef_year$diesel * fleet_proportion$diesel
      #   ) +
      #   (
      #     new_service_vmt_year * greet_ef_year$compressed_natural_gas * fleet_proportion$compressed_natural_gas +
      #       (
      #         new_service_vmt_year * greet_ef_year$electricity * fleet_proportion$electricity
      #       )
      #   )
      # )
      
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
