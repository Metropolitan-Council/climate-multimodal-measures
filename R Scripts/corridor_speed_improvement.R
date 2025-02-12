corridor_speed_improvements <- function(corridor_distance,
                                        avg_annual_daily_traffic,
                                        avg_corridor_speed_no_build,
                                        avg_corridor_speed_build,
                                        location,
                                        project_start,
                                        project_lifetime) {
  service_days = 260 
  k1_speed_build <- 0.000019137 * avg_corridor_speed_build^2 - 0.0020660 * avg_corridor_speed_build + 0.088916
  K1_speed_no_build <- 0.000019137 * avg_corridor_speed_no_build^2 - 0.0020660 * avg_corridor_speed_no_build + 0.088916
  
  print(k1_speed_build)
  print(K1_speed_no_build)
  
  speed_improvement_prct <- ((avg_corridor_speed_build - avg_corridor_speed_no_build) / avg_corridor_speed_no_build)
  
  if (speed_improvement_prct <= .05) {
    induced_demand_elasticity <- 0
  } else if (speed_improvement_prct > .05 & speed_improvement_prct <= .2) {
    induced_demand_elasticity <- 2 * speed_improvement_prct - 0.1
  } else if (speed_improvement_prct > .2) {
    induced_demand_elasticity <- 0.3
  }
  
  # Generate years project covers
  project_start <- as.numeric(project_start)  # Ensure numeric year
  project_years <- seq(project_start, project_start + project_lifetime - 1)  # Create year range
  
  # Initialize vectors to store results
  fuel_consumption_reduced <- numeric(length(project_years))
  induced_demand <- numeric(length(project_years))
  ghg_impact <- numeric(length(project_years))
  carbon_cost <- numeric(length(project_years))
  
  for (i in seq_along(project_years)) {
    year <- project_years[i]
    
    # Calculate VMT displaced for the current year
    fuel_consumption_reduced[i] <- (corridor_distance * 
      avg_annual_daily_traffic *
      (K1_speed_no_build - k1_speed_build)) * service_days 
    
    # Calculate induced demand
    induced_demand[i] <- corridor_distance *
      avg_annual_daily_traffic *
      induced_demand_elasticity * service_days
    
    # Filter GHG emission factor (EF) for the current year
    greet_ef_year <- GREETCarbonIntensity %>%
      filter(Year == year)
    discount_rate <- SocialCostCarbon %>%
      filter(`emission.year` == year & gas == "CO2")
    
    # Calculate GHG impact for the current year
    ghg_impact_year <- ((fuel_consumption_reduced[i] - induced_demand[i] * k1_speed_build) * 9.915)/ 1000 # 9.915 derived from avg LD WTW GHG emission factor 

    
    # Calculate social cost of carbon
    social_cost_carbon <- ghg_impact_year * discount_rate$`2.0% Ramsey`
    
    # Store results
    ghg_impact[i]  <- ghg_impact_year
    carbon_cost[i] <- social_cost_carbon
  }
  
  # Calculate total values
  total_fuel_consumption_reduced <- sum(fuel_consumption_reduced)
  total_induced_demand <- sum(induced_demand)
  total_ghg_impact <- sum(ghg_impact)
  total_carbon_cost <- sum(carbon_cost)
  
  # Create a data frame with results including totals
  results <- data.frame(
    Year = c(project_years, "Total"),
    "Fuel Consumption Reduced (gallons)" = round(c(fuel_consumption_reduced, total_fuel_consumption_reduced), 0),
    "Induced Vehicle Miles (Miles)" = round(c(induced_demand, total_induced_demand), 0),
    "GHG Reduction (MT CO₂)" = round(c(ghg_impact, total_ghg_impact), 0),
    "Carbon Cost Reduction ($) 
    <i class='fas fa-question-circle' data-toggle='tooltip' data-placement='top' 
      title='The Social Cost of Carbon estimates the economic savings from avoiding one ton of CO₂ emissions, reflecting reduced damages to agriculture, human health, infrastructure, and ecosystems. Using a 2% Ramsey discount rate, future damages are valued at 98% of their present value.'></i>" = 
      format(round(c(carbon_cost, total_carbon_cost), 0), big.mark = ","),
    check.names = FALSE
  )
  
  
  return(results)
}
