employee_commute <- function(daily_commute_no, project_start, project_lifetime, 
                             average_commute) {
  
  working_days <- 260  # Assuming 260 working days per year
  
  # Generate list of project years
  project_years <- seq(project_start, project_start + project_lifetime - 1)
  
  # Initialize vectors to store results
  VMT_displaced <- numeric(length(project_years))
  GHG_impact <- numeric(length(project_years))
  
  for (i in seq_along(project_years)) {
    year <- project_years[i]
    
    # Calculate VMT displaced for the current year
    VMT_displaced_year <- daily_commute_no * average_commute * working_days
    
    # Filter GHG emission factor (EF) for the current year
    Greet_EF_year <- Greet_EF %>% filter(year == year)
    
    discount_rate <- SCC %>% filter(year == year )
    
    # Calculate GHG impact for the current year
    GHG_impact_year <- VMT_displaced_year * Greet_EF_year$EF * discount_rate
    
    # Store results for the current year
    VMT_displaced[i] <- VMT_displaced_year
    GHG_impact[i] <- GHG_impact_year
  }
  
# What format are we returning this in Shiny? 
  return(data.frame(Year = project_years, VMT_displaced = VMT_displaced, GHG_impact = GHG_impact))
}
