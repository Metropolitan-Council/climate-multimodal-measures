corridor_speed_improvements <- function(corridor_distance,
                                        avg_annual_daily_traffic,
                                        avg_corridor_speed_no_build,
                                        avg_corridor_speed_build,
                                        location,
                                        project_start,
                                        project_lifetime) {
  
  k1_speed_build <- 0.000019137 * avg_corridor_speed_build^2 - 0.0020660 * avg_corridor_speed_build + 0.088916
  K1_speed_no_build <- 0.000019137 * avg_corridor_speed_no_build^2 - 0.0020660 * avg_corridor_speed_no_build + 0.088916
  
  speed_improvement_prct <- ((avg_corridor_speed_build - avg_corridor_speed_no_build) / avg_corridor_speed_no_build) * 100
  
  if (speed_improvement_prct <= 5) {
    induced_demand_elasticity <- 0
  } else if (speed_improvement_prct > 5 & speed_improvement_prct <= 20) {
    induced_demand_elasticity <- 2 * speed_improvement_prct - 0.1
  } else if (speed_improvement_prct > 20) {
    induced_demand_elasticity <- 0.3
  }
  
  # Generate years project covers
  project_start <- lubridate::year(project_start)
  project_years <- seq(project_start, project_start + project_lifetime - 1)
  
  # Initialize vectors to store results
  fuel_consumption_reduced <- numeric(length(project_years))
  induced_demand <- numeric(length(project_years))
  ghg_impact <- numeric(length(project_years))
  carbon_cost <- numeric(length(project_years))
  
  for (i in seq_along(project_years)) {
    year <- project_years[i]
    
    # Calculate VMT displaced for the current year and store it in the i-th position
    fuel_consumption_reduced[i] <- corridor_distance * avg_annual_daily_traffic * (K1_speed_no_build - k1_speed_build)
    
    # Calculate induced demand and store it in the i-th position
    induced_demand[i] <- corridor_distance * avg_corridor_speed_no_build * k1_speed_build * induced_demand_elasticity
    
    # Filter GHG emission factor (EF) for the current year
    greet_ef_year <- GREETCarbonIntensity %>% filter(Year == year)
    discount_rate <- SocialCostCarbon %>% filter(`emission.year` == year & gas == "CO2")
    
    # Calculate GHG impact for the current year
    ghg_impact_year <- (fuel_consumption_reduced[i] - induced_demand[i]) * 9.915
    
    # Calculate social cost of carbon
    social_cost_carbon <- ghg_impact_year * discount_rate$`2.0% Ramsey`
    
    # Store results for the current year
    ghg_impact[i] <- ghg_impact_year
    carbon_cost[i] <- social_cost_carbon
  }
  
  # Calculate total values
  total_fuel_consumption_reduced <- sum(fuel_consumption_reduced)
  total_induced_demand <- sum(induced_demand)
  total_ghg_impact <- sum(ghg_impact)
  total_carbon_cost <- sum(carbon_cost)
  
  # Create a data frame with results including totals
  results <- data.frame(
    year = c(project_years, "Total"),
    fuel_consumption_reduced = c(fuel_consumption_reduced, total_fuel_consumption_reduced),
    induced_demand = c(induced_demand, total_induced_demand),
    ghg_impact = c(ghg_impact, total_ghg_impact),
    carbon_cost = c(carbon_cost, total_carbon_cost)
  )
  
  return(results)
}