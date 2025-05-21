#' Title
#'
#' @param no_participants 
#' @param conversion_rate 
#' @param location 
#' @param project_start 
#' @param project_lifetime 
#'
#' @returns
#' @export
#'
#' @examples
ev_outreach <- function(no_participants,
                        conversion_rate,
                        location,
                        # audience,
                        project_start,
                        project_lifetime) {
  
  community_type <- CommunityTypeShape %>% 
    filter(CTU_NAME == location) %>% 
    pull(MappedCommunity)
  
  average_annual_accrual <- PerVehicleVMT %>% filter(MappedCommunity == community_type) %>% pull(PerVehicleVMT)
  

  project_start <- as.numeric(project_start)  # Ensure numeric year
  project_years <- seq(project_start, project_start + project_lifetime - 1)  # Create year range
  
  
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
    
    # if (audience == "Light Duty") {
    #   ghg_impact_year <- (vmt_displaced_year * (gasoline_ef_year - greet_ef_year$electricity)) / 1000000
    # }
    # 
    # if (audience == "Heavy Duty") {
    #   ghg_impact_year <- (vmt_displaced_year * (diesel_ef_year - greet_ef_year$electricity)) / 1000000
    # }
    
    ghg_impact_year <- (vmt_displaced_year * (gasoline_ef_year - greet_ef_year$electricity)) / 1000000
    
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
  
  results <- data.frame(
    Year = c(project_years, "Total"),
    "VMT Reduction (Miles)" = format(round(c(auto_vmt_displaced, total_vmt_displaced), 0), big.mark = ","),
    "GHG Reduction (MT CO₂)" = format(round(c(ghg_impact, total_ghg_impact), 0), big.mark = ","),
    "Social Cost of Carbon Reduction ($) <i class='fas fa-question-circle' 
   title='The Social Cost of Carbon estimates the economic savings from avoiding one ton of CO₂ emissions, reflecting reduced damages to agriculture, human health, infrastructure, and ecosystems. Using a 2% Ramsey discount rate, future damages are valued at 98% of their present value.'></i>" = 
      format(round(c(carbon_cost, total_carbon_cost), 0), big.mark = ","),
    check.names = FALSE
  )

  
  return(results)
}
