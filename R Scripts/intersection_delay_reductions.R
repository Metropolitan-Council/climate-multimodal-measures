#' Calculate Benefits of Intersection Delay Reduction Projects 
#'
#' @param number_peak_hours (numeric), Number of peak hours per day where delay is reduced
#' @param vehicle_per_hour (numeric), Number of vehicles per hour during peak hours
#' @param peak_hour_delay_noBuild (numeric), Peak Hour Delay Per Vehicle under No-Build Condition
#' @param peak_hour_delay_build (numeric), Peak Hour Delay Per Vehicle under Build Condition
#' @param project_start (numeric), Year the project starts
#' @param project_lifetime (numeric), Year the project ends
#' 
#' @returns
#' A data frame with Fuel Consumption (gallons), GHG Reduction (MT CO2), and Social Cost of Carbon ($) Reduction for each year of the project, including totals.
#' @export
#'
#' @examples
#' intersection_delay_reductions(
#' number_peak_hours = 2,
#' vehicle_per_hour = 1147,
#' peak_hour_delay_noBuild = 0.03,
#' peak_hour_delay_build = 0.015,
#' project_start = 2025,
#' project_lifetime = 7
#')

intersection_delay_reductions <-
  function(number_peak_hours,
           vehicle_per_hour,
           peak_hour_delay_noBuild,
           peak_hour_delay_build,
           project_start,
           project_lifetime){
    
    service_days = 260
    k2 = 0.37 #default idling fuel factor in gallon/hour - comes from Argonne National Laboratory (ANL) Vehicle Idle Reduction Savings Worksheet
    total_peak_hours_reduced = vehicle_per_hour * number_peak_hours * (peak_hour_delay_noBuild - peak_hour_delay_build)
    
    # Generate years project covers
    project_start <- as.numeric(project_start)  # Ensure numeric year
    project_years <- seq(project_start, project_start + project_lifetime - 1)  # Create year range
    
    # Initialize vectors to store results
    fuel_consumption_reduced <- numeric(length(project_years))
    ghg_impact <- numeric(length(project_years))
    carbon_cost <- numeric(length(project_years))
    
    for (i in seq_along(project_years)) {
      current_year <- project_years[i]
      
      # Calculate VMT displaced for the current year and store in the i-th position
      fuel_consumption_reduced[i] <- (k2 * total_peak_hours_reduced) * service_days 
      
      ghg_impact_year <- (fuel_consumption_reduced[i] * 9.915)/ 1000 # 9.915 derived from avg LD WTW GHG emission factor 
      
      discount_rate <-
        SocialCostCarbon %>% filter(`emission.year` == current_year &
                                      gas == "CO2")
      
      social_cost_carbon <- ghg_impact_year * discount_rate$`2.0% Ramsey`
      
      # Store results for the current year
      ghg_impact[i] <- ghg_impact_year
      carbon_cost[i] <- social_cost_carbon
    }
    
    
    # Calculate total vmt_displaced and ghg_impact
    total_fuel_consumption_reduced <- sum(fuel_consumption_reduced)
    total_ghg_impact <- sum(ghg_impact)
    total_carbon_cost <- sum(carbon_cost)
    
    # Create a data frame with results including totals
    results <- data.frame(
      Year = c(project_years, "Total"),
      "Fuel Consumption Reduced (gallons)" = round(c(fuel_consumption_reduced, total_fuel_consumption_reduced),0),
      "GHG Reduction (MT CO₂)" = round(c(ghg_impact, total_ghg_impact),0),
      "Social Cost of Carbon Reduction ($) <i class='fas fa-question-circle' 
   title='The Social Cost of Carbon estimates the economic savings from avoiding one ton of CO₂ emissions, reflecting reduced damages to agriculture, human health, infrastructure, and ecosystems. Using a 2% Ramsey discount rate, future damages are valued at 98% of their present value.'></i>" = 
        format(round(c(carbon_cost, total_carbon_cost), 0), big.mark = ","),
      check.names = FALSE
    )
    
    return(results)
  }
