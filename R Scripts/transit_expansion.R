transit_expansion <-
  function(ridership_increase,
           route_type,
           added_transit,
           fleet_type,
           project_start,
           project_lifetime) {
    # Select average trip length by route type
    average_trip_length <- AdjustmentFactorsAndTripLengths %>%
      filter(route_type == route_type) %>%
      pull(average_trip_length_mi_trip)
    
    # Select adjustment factor by route type
    adjustment_factor <-
      AdjustmentFactorsAndTripLengths %>% filter(route_type == route_type) %>% pull(adjustment_factor)
    
    # Generate years project covers
    project_years <-
      seq(project_start, project_start + project_lifetime - 1)
    
    # Initialize vectors to store results
    new_service_vmt <- numeric(length(project_years))
    auto_vmt_displaced <- numeric(length(project_years))
    ghg_impact <- numeric(length(project_years))
    carbon_cost <- numeric(length(project_years))
    
    for (i in seq_along(project_years)) {
      year <- project_years[i]
      
      # Calculate VMT displaced for the current year
      vmt_displaced_year <-
        ridership_increase * average_trip_length * adjustment_factor
      
      # Filter GHG emission factor (EF) for the current year
      greet_ef_year <- GREETCarbonIntensity %>% filter(Year == year)
      
      # Filter discount rate for the current year
      discount_rate <-
        SocialCostCarbon %>% filter(`emission.year` == year &
                                      gas == "CO2")
      
      #Filter fleet percentages for the current year
      fleet_proportion <- FleetProportion %>% filter(Year == year)
      
      if (fleet_type == "")
        fleet_ghg_added <- added_transit
      
      # Calculate GHG impact for the current year
      ghg_impact_year <-
        ((
          vmt_displaced_year * greet_ef_year$gasoline * fleet_proportion$gasoline
        ) +
          (
            vmt_displaced_year * greet_ef_year$diesel * fleet_proportion$diesel
          ) +
          (
            vmt_displaced_year * greet_ef_year$compressed_natural_gas * fleet_proportion$compressed_natural_gas
          ) +
          (
            vmt_displaced_year * greet_ef_year$electricity * fleet_proportion$electricity
          )
        ) -
        (added_transit * greet_ef_year$diesel) #Change to what the fleet proposed is made of - maybe make user input
      
      social_cost_carbon <-
        ghg_impact_year * discount_rate$`2.0% Ramsey`
      
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


test <- employee_commute(200, 2025, 10, 2.8)
