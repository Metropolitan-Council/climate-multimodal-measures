#' Title
#'
#' @param daily_commute_no 
#' @param project_start 
#' @param project_lifetime 
#' @param location 
#' @param working_days 
#' @param average_commute 
#'
#' @returns
#' @export
#'
#' @examples
employee_commute <- function(daily_commute_no,
                             project_start,
                             project_lifetime,
                             location,
                             working_days = NULL,
                             average_commute = NULL) {
  
  community_type <- CommunityTypeShape %>% 
    filter(CTU_NAME == location) %>% 
    pull(MappedCommunity)
  
  if (is.null(working_days)) {
    working_days <- 260  # Assuming 260 working days per year
  }
  
  # Generate years project covers based on project start date and length of project
  project_start <- as.numeric(project_start)  # Ensure numeric year
  project_years <- seq(project_start, project_start + project_lifetime - 1)  # Create year range
  
  
  # Initialize vectors to store results
  vmt_displaced <- numeric(length(project_years))
  ghg_impact <- numeric(length(project_years))
  carbon_cost <- numeric(length(project_years))
  
  # Calculate vmt, ghg, and social cost of carbon for each year of the project timeline
  for (i in seq_along(project_years)) {
    current_year <- project_years[i]
    
    # Choosing the closest year to grab VMT data from
    closest_year_vmt <- VMTByCommunityType %>%
      summarise(closest_year = cd_year[which.min(abs(cd_year - current_year))]) %>%
      pull(closest_year)
    
    # Finding the average two way commute based on community type 
    if (is.null(average_commute)) {
      average_two_way_commute <- VMTByCommunityType %>% 
        filter(CD == community_type)
      
      # Making average commute a one way commute 
      average_commute <- average_two_way_commute$vmt/2
    }
    
    # Calculate VMT displaced for the current year
    vmt_displaced_year <- daily_commute_no * average_commute * working_days
    
    # Filter GHG emission factor (EF) for the current year
    greet_ef_year <- GREETCarbonIntensity %>% filter(Year == current_year)
    
    diesel_ef_year <- DieselEFsCommunityType %>% filter(year == current_year) %>% filter(MappedCommunity == community_type) %>% pull(EF)
    
    gasoline_ef_year <- GasolineEFsCommunityType %>% filter(year == current_year) %>% filter(MappedCommunity == community_type) %>% pull(EF)
    
    # Filter Discount Rate for the current year
    discount_rate <- SocialCostCarbon %>% 
      filter(`emission.year` == current_year & gas == "CO2")
    
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
    
    # Calculate GHG impact for the current year
    ghg_impact_year <- 
      ((vmt_displaced_year * gasoline_ef_year * fleet_proportion$gasoline) +
         (vmt_displaced_year * diesel_ef_year * fleet_proportion$diesel) +
         (vmt_displaced_year * greet_ef_year$electricity * fleet_proportion$electricity)) / 1000000
    
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
  
  results <- data.frame(
    Year = c(project_years, "Total"),
    "VMT Reduction (Miles)" = format(round(c(vmt_displaced, total_vmt_displaced), 0), big.mark = ","),
    "GHG Reduction (MT CO₂)" = format(round(c(ghg_impact, total_ghg_impact), 0), big.mark = ","),
    "Social Cost of Carbon Reduction ($) <i class='fas fa-question-circle' 
   title='The Social Cost of Carbon estimates the economic savings from avoiding one ton of CO₂ emissions, reflecting reduced damages to agriculture, human health, infrastructure, and ecosystems. Using a 2% Ramsey discount rate, future damages are valued at 98% of their present value.'></i>" = 
      format(round(c(carbon_cost, total_carbon_cost), 0), big.mark = ","),
    check.names = FALSE
  )
  return(results)
}