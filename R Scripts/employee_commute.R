employee_commute <- function(daily_commute_no,
                             project_start,
                             project_lifetime,
                             community_type, 
                             location) {
  
  working_days <- 260  # Assuming 260 working days per year
  
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
    
    closest_year_vmt <- VMTByCommunityType %>%
      summarise(closest_year = cd_year[which.min(abs(cd_year - current_year))]) %>%
      pull(closest_year)
    
    average_two_way_commute <- VMTByCommunityType %>% 
      filter(cd_year == closest_year_vmt, CD == community_type, survey_year == 2021)
    
    average_commute <- average_two_way_commute$vmt/2
    
    # Calculate VMT displaced for the current year
    vmt_displaced_year <- daily_commute_no * average_commute * working_days
    
    # Filter GHG emission factor (EF) for the current year
    greet_ef_year <- GREETCarbonIntensity %>% filter(Year == current_year)
    
    # Filter Discount Rate for the current year
    discount_rate <- SocialCostCarbon %>% 
      filter(`emission.year` == current_year & gas == "CO2")
    
   # Filter to CTU provided
    FleetData <- FleetData %>% filter(ctu == location)

    FleetData <- FleetData %>% mutate(year = as.numeric(year))

    # # Determine the closest year
    closest_year <- FleetData %>%
      summarise(closest_year = year[which.min(abs(year - current_year))]) %>%
      pull(closest_year)

    # # Filter the data set to get the fleet proportions from the closest year
    fleet_proportion <- FleetData %>%
      filter(year == closest_year)
    
    # Calculate GHG impact for the current year
    ghg_impact_year <- 
      (vmt_displaced_year * greet_ef_year$gasoline * fleet_proportion$gasoline) +
      (vmt_displaced_year * greet_ef_year$diesel * fleet_proportion$diesel) +
      (vmt_displaced_year * greet_ef_year$electricity * fleet_proportion$electricity)
    
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

# test <- employee_commute(daily_commute_no = 200,
#                          project_start = 2029,
#                          project_lifetime = 10,
#                          community_type = "Urban",
#                          location = "Andover")
