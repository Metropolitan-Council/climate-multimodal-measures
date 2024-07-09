employee_commute <- function(daily_commute_no, project_start, project_lifetime, average_commute) {
  
  working_days <- 260  # Assuming 260 working days per year
  
  # Generate years project covers
  project_years <- seq(project_start, project_start + project_lifetime - 1)
  
  # Initialize vectors to store results
  vmt_displaced <- numeric(length(project_years))
  ghg_impact <- numeric(length(project_years))
  
  for (i in seq_along(project_years)) {
    year <- project_years[i]
    
    # Calculate VMT displaced for the current year
    vmt_displaced_year <- daily_commute_no * average_commute * working_days
    
    # Filter GHG emission factor (EF) for the current year
    greet_ef_year <- greet_carbon_intensity %>% filter(emission_year == year)
    
    discount_rate <- social_cost_carbon %>% filter(emission_year == year)
    
    # Calculate GHG impact for the current year
    ghg_impact_year <- vmt_displaced_year * greet_ef_year$ef * discount_rate
    
    # Store results for the current year
    vmt_displaced[i] <- vmt_displaced_year
    ghg_impact[i] <- ghg_impact_year
  }
  
  # Returning the results as a data frame
  return(data.frame(year = project_years, vmt_displaced = vmt_displaced, ghg_impact = ghg_impact))
}

