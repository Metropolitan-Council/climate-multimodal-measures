ev_infrastructure <- function(ev_type,
                              no_chargers,
                              charger_type,
                              charge_power,
                              annual_hours_available,
                              location,
                              project_start,
                              project_lifetime,
                              utilization_rate = NULL) {
  
  community_type <- CommunityTypeShape %>% 
    filter(CTU_NAME == location) %>% 
    pull(MappedCommunity)

  
  if (is.null(utilization_rate)) {
    if (charger_type == "DCFC"){
      utilization_rate <- ChargerUtilizationRates$DC_fast
    }
    
    if (charger_type == "Level 2"){
      utilization_rate <- ChargerUtilizationRates$level_2
    }
  }
  
  if (ev_type == "Light-Duty") {
    average_energy_efficiency <- FuelEfficiency %>% filter(`Vehicle Type` == "Light-Duty") %>%
      pull(`Fuel Efficiency (Wh/mi)`)
  }
  
  if (ev_type == "Heavy-Duty") {
    average_energy_efficiency <- FuelEfficiency %>% filter(`Vehicle Type` == "Heavy-Duty") %>%
      pull(`Fuel Efficiency (Wh/mi)`)
  }
  
  # Generate years project covers based on project start date and length of project
  project_start <- lubridate::year(project_start)
  project_start <- as.numeric(project_start)
  project_years <- seq(project_start, project_start + project_lifetime - 1)
  
  # Initialize vectors to store results
  vmt_displaced <- numeric(length(project_years))
  ghg_impact <- numeric(length(project_years))
  carbon_cost <- numeric(length(project_years))
  
  # Calculate vmt, ghg, and social cost of carbon for each year of the project timeline
  for (i in seq_along(project_years)) {
    current_year <- project_years[i]
    
    # Filter to CTU provided
    FleetData <- FleetData %>% filter(MappedCommunity == community_type)
    
    FleetData <- FleetData %>% mutate(year = as.numeric(year))
    
    # # Determine the closest year
    closest_year <- FleetData %>%
      summarise(closest_year = year[which.min(abs(year - current_year))]) %>%
      pull(closest_year)
    
    # # Filter the data set to get the fleet proportions from the closest year
    fleet_proportion <- FleetData %>%
      filter(year == closest_year)
    
    percentage_ICE <- (100 - fleet_proportion$electricity)
    
    # Calculate VMT displaced for the current year
    vmt_displaced_year <- vmt_displaced_year <- (no_chargers * charge_power * utilization_rate * annual_hours_available) / average_energy_efficiency * (percentage_ICE / 100)

    
    # Filter GHG emission factor (EF) for the current year
    greet_ef_year <- GREETCarbonIntensity %>% filter(Year == current_year)
    
    diesel_ef_year <- DieselEFsCommunityType %>% filter(year == current_year) %>% filter(MappedCommunity == community_type) %>% pull(EF)
    
    gasoline_ef_year <- GasolineEFsCommunityType %>% filter(year == current_year) %>% filter(MappedCommunity == community_type) %>% pull(EF)
    
    
    if (ev_type == "Light-Duty") {
      carbon_intensity <- gasoline_ef_year
    }
    
    if (ev_type == "Heavy-Duty") {
      carbon_intensity <- diesel_ef_year
    }
    
    carbon_intensity_grid <- greet_ef_year %>%
      pull(electricity)
    
    ghg_impact_year <- vmt_displaced_year * (carbon_intensity - carbon_intensity_grid) / 1000000
    
    # Filter Discount Rate for the current year
    discount_rate <- SocialCostCarbon %>% 
      filter(`emission.year` == current_year & gas == "CO2")
    
    social_cost_carbon <- ghg_impact_year * discount_rate$`2.0% Ramsey`
    
    # Store results for the current year
    vmt_displaced[i] <- vmt_displaced_year
    ghg_impact[i] <- ghg_impact_year
    carbon_cost[i] <- social_cost_carbon
  }
  
  # Calculate total vmt_displaced and ghg_impact
  total_vmt_displaced <- sum(vmt_displaced)
  total_ghg_impact <- sum(ghg_impact)
  total_carbon_cost <- sum(carbon_cost)
  
  # Create a data frame with results including totals
  results <- data.frame(
    year = c(project_years, "Total"),
    vmt_displaced = c(vmt_displaced, total_vmt_displaced),
    ghg_impact = c(ghg_impact, total_ghg_impact),
    carbon_cost = c(carbon_cost, total_carbon_cost)
  )
  
  return(results)
}

test <- ev_infrastructure(
  ev_type = "Light-Duty",
  no_chargers = 25,
  charge_power = 19.2,
  charger_type = "Level 2",
  annual_hours_available = 8760,
  location = "Andover",
  project_start = "2024-01-01",
  project_lifetime = 1
)
