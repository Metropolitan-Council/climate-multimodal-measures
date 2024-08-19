ev_infrastructure <- function(no_chargers, charge_power, annual_hours_available, EV_type, project_start, project_lifetime) {
  
  utilization_rate <- X 
  average_energy_efficiency <- X
  
  #CALCULATE ICE VEHICLES IN FLEET (LD or HD)
  percentage_ice <- X
  
  # Generate years project covers based on project start date and length of project
  project_years <- seq(project_start, project_start + project_lifetime - 1)
  
  # Initialize vectors to store results
  vmt_displaced <- numeric(length(project_years))
  ghg_impact <- numeric(length(project_years))
  carbon_cost <- numeric(length(project_years))
  
  # Calculate vmt, ghg, and social cost of carbon for each year of the project timeline
  for (i in seq_along(project_years)) {
    current_year <- project_years[i]
    
    #VMT DISPLACED BY TYPE (LD or HD)
    
    #GHG IMPACT
    
    #SOCIAL COST OF CARBON
  }
}