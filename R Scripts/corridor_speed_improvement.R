corridor_speed_improvements <-
  function(corridor_distance,
           avg_annual_daily_traffic,
           avg_corridor_speed_no_build,
           avg_corridor_speed_build,
           location,
           project_start,
           project_lifetime,
           fleet_ratio = NULL) {
    
    speed_improvement_prct <- ((avg_corridor_speed_build - avg_corridor_speed_no_build) / avg_corridor_speed_no_build
    ) * 100
    
    if (speed_improvement_prct <= 5) {
      induced_demand_elasticity <- 0
    } else if (speed_improvement_prct > 5 &
               speed_improvement_prct <= 20) {
      induced_demand_elasticity <- 2 * speed_improvement_prct - 0.1
    } else if (speed_improvement_prct > 20) {
      induced_demand_elasticity <- 0.3
    }
    
    # Generate years project covers
    project_start <- lubridate::year(project_start)
    project_start <- as.numeric(project_start)
    project_years <- seq(project_start, project_start + project_lifetime - 1)
    
    # Initialize vectors to store results
    fuel_consumption_reduced <- numeric(length(project_years))
    ghg_impact <- numeric(length(project_years))
    carbon_cost <- numeric(length(project_years))
    
    for (i in seq_along(project_years)) {
      year <- project_years[i]
      
      # Calculate VMT displaced for the current year
      # fuel_consumption_reduced <- corridor_distance * avg_annual_daily_traffic(
      #   avg_corridor_speed_build - (1 + induced_demand_elasticity) * avg_corridor_speed_build
      # )
      
      # Filter GHG emission factor (EF) for the current year
      greet_ef_year <- GREETCarbonIntensity %>% filter(Year == year)
      
      discount_rate <- SocialCostCarbon %>% filter(`emission.year` == year &
                                                     gas == "CO2")
      ghg_impact_year <- fuel_consumption_reduced * 9.915
      
      social_cost_carbon <- ghg_impact_year * discount_rate$`2.0% Ramsey`
      
      # Store results for the current year
      auto_vmt_displaced[i] <- vmt_displaced_year
      ghg_impact[i] <- ghg_impact_year
      carbon_cost[i] <- social_cost_carbon
    }
    
    # Calculate total vmt_displaced and ghg_impact
    total_fuel_consumption_reduced <- sum(fuel_consumption_reduced)
    total_ghg_impact <- sum(ghg_impact)
    total_carbon_cost <- sum(carbon_cost)
    
    # Create a data frame with results including totals
    results <- data.frame(
      year = c(project_years, "Total"),
      fuel_consumption_reduced = c(fuel_consumption_reduced, total_fuel_consumption_reduced),
      ghg_impact = c(ghg_impact, total_ghg_impact),
      carbon_cost = c(carbon_cost, total_carbon_cost)
    )
    
    return(results)
  }
