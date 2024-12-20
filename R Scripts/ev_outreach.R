ev_outreach <- function(no_participants,
                        conversion_rate,
                        location,
                        audience,
                        project_start,
                        project_lifetime) {
  
  community_type <- CommunityTypeShape %>% 
    filter(CTU_NAME == location) %>% 
    pull(MappedCommunity)
  
  average_annual_accrual <- PerVehicleVMT %>% filter(MappedCommunity == community_type) %>% pull(PerVehicleVMT)
  
  # Generate years project covers
  project_start <- lubridate::year(project_start)
  project_start <- as.numeric(project_start)
  project_years <- seq(project_start, project_start + project_lifetime - 1)
  
  # Initialize vectors to store results
  auto_vmt_displaced <- numeric(length(project_years))
  ghg_impact <- numeric(length(project_years))
  carbon_cost <- numeric(length(project_years))
  
  for (i in seq_along(project_years)) {
    current_year <- project_years[i]
    
    # Calculate VMT displaced for the current year
    vmt_displaced_year <- no_participants * conversion_rate * average_annual_accrual
    
    # Filter GHG emission factor (EF) for the current year
    greet_ef_year <- GREETCarbonIntensity %>% filter(Year == current_year)
  
    discount_rate <- SocialCostCarbon %>% filter(`emission.year` == current_year &
                                            gas == "CO2")
    
    diesel_ef_year <- DieselEFsCommunityType %>% filter(year == current_year) %>% filter(MappedCommunity == community_type) %>% pull(EF)
    
    gasoline_ef_year <- GasolineEFsCommunityType %>% filter(year == current_year) %>% filter(MappedCommunity == community_type) %>% pull(EF)
    
    if (audience == "Light Duty") {
      ghg_impact_year <- (vmt_displaced_year * (gasoline_ef_year - greet_ef_year$electricity)) / 1000000
    }
    
    if (audience == "Heavy Duty") {
      ghg_impact_year <- (vmt_displaced_year * (diesel_ef_year - greet_ef_year$electricity)) / 1000000
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
  results <- data.frame(
    year = c(project_years, "Total"),
    "VMT (Miles)" = round(c(auto_vmt_displaced, total_vmt_displaced), 0),
    "GHG Impact (kt CO₂)" = round(c(ghg_impact, total_ghg_impact), 1),
    "Carbon Cost ($)" = round(c(carbon_cost, total_carbon_cost), 0),
    check.names = FALSE
  )
  
  return(results)
}



# test<- ev_outreach(
#   no_participants = 4000,
#   conversion_rate = .04,
#   audience = "Light Duty",
#   project_start = "2024-01-01",
#   location = "Andover",
#   project_lifetime = 5
# )
