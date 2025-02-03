intersection_delay_reductions <-
  function(number_peak_hours,
           vehicle_per_hour,
           peak_hour_delay_noBuild,
           peak_hour_delay_build,
           location,
           project_start,
           project_lifetime){
    
    service_days = 260
    k2 = 0.37 #default idling fuel factor in gallon/hour - comes from Argonne National Laboratory (ANL) Vehicle Idle Reduction Savings Worksheet
    total_peak_hours_reduced = vehicle_per_hour * number_peak_hours * (peak_hour_delay_noBuild - peak_hour_delay_build)
    
    # Generate years project covers
    project_start <- lubridate::year(project_start)
    project_start <- as.numeric(project_start)
    project_years <- seq(project_start, project_start + project_lifetime - 1)
    
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
      "GHG Reduction (MT CO₂))" = round(c(ghg_impact, total_ghg_impact)0),
      "Carbon Cost Reduction ($) <i class='fas fa-question-circle' 
   title='Place holder text to explain Social Cost of Carbon'></i>" = 
        format(round(c(carbon_cost, total_carbon_cost), 0), big.mark = ","),
      check.names = FALSE
    )
    
    return(results)
  }