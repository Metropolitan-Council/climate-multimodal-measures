ev_outreach <- function(no_participants, conversion_rate, audience, project_start, project_lifetime) {
  
  average_annual_accrual <- 5567
  
  # Generate years project covers
  project_start <- lubridate::year(project_start)
  project_start <- as.numeric(project_start)
  project_years <- seq(project_start, project_start + project_lifetime - 1)
  
  # Initialize vectors to store results
  auto_vmt_displaced <- numeric(length(project_years))
  ghg_impact <- numeric(length(project_years))
  carbon_cost <- numeric(length(project_years))
  
  for (i in seq_along(project_years)) {
    year <- project_years[i]
    
    # Calculate VMT displaced for the current year
    vmt_displaced_year <- no_participants * conversion_rate * average_annual_accrual
    
    # Filter GHG emission factor (EF) for the current year
    greet_ef_year <- GREETCarbonIntensity %>% filter(Year == year)
    
    discount_rate <- SocialCostCarbon %>% filter(`emission.year` == year & gas == "CO2")
    
    if (audience == "Light Duty") {
      ghg_impact_year <-(vmt_displaced_year * (greet_ef_year$gasoline - greet_ef_year$electricity)) / 1000000
    }
    
    if (audience == "Heavy Duty") {
      ghg_impact_year <-(vmt_displaced_year * (greet_ef_year$diesel - greet_ef_year$electricity)) / 1000000
    }
    
    social_cost_carbon <- ghg_impact_year * discount_rate$`2.0% Ramsey`
    
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
# 
# test<- ev_outreach(
#   no_participants = 4000,
#   conversion_rate = .04,
#   audience = "Light Duty",
#   project_start = "2024-01-01",
#   project_lifetime = 5
# )

