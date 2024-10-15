# Are we displacing electricity, diesel, and gasoline vehicle proportions by ctu?
# Should we ask the user for input on the type of fuel their fleet will run on?
transit_expansion <-
  function(ridership_increase,
           route_type,
           added_transit,
           fleet_type,
           location,
           project_start,
           project_lifetime) {
    
    # Select average trip length by route type
    average_trip_length <- AdjustmentFactorsAndTripLengths %>%
      filter(route_type == !!route_type) %>%
      pull(average_trip_length_mi_trip)
    
    adjustment_factor <- AdjustmentFactorsAndTripLengths %>% 
      filter(route_type == !!route_type) %>% 
      pull(adjustment_factor)
    
    # Generate years project covers
    project_start <- lubridate::year(project_start)
    project_start <- as.numeric(project_start)
    project_years <-
      seq(project_start, project_start + project_lifetime - 1)
    
    # Initialize vectors to store results
    vmt_displaced <- numeric(length(project_years))
    ghg_impact <- numeric(length(project_years))
    carbon_cost <- numeric(length(project_years))
    
    for (i in seq_along(project_years)) {
      current_year <- project_years[i]
      
      # Calculate VMT displaced for the current year
      vmt_displaced_year <-
        ridership_increase * average_trip_length * adjustment_factor
      
      # Filter GHG emission factor (EF) for the current year
      greet_ef_year <- GREETCarbonIntensity %>% filter(Year == current_year)
      
      # Filter discount rate for the current year
      discount_rate <-
        SocialCostCarbon %>% filter(`emission.year` == current_year &
                                      gas == "CO2")
      
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
        ((vmt_displaced_year * greet_ef_year$gasoline * fleet_proportion$gasoline) +
        (vmt_displaced_year * greet_ef_year$diesel * fleet_proportion$diesel) +
        (vmt_displaced_year * greet_ef_year$electricity * fleet_proportion$electricity)) - 
        (added_transit * (greet_ef_year[[fleet_type]]))
      
      # print(paste("ghg impact", ghg_impact_year))
      
      # Filter Discount Rate for the current year
      discount_rate <- SocialCostCarbon %>% 
        filter(`emission.year` == current_year & gas == "CO2")
      
      social_cost_carbon <- ghg_impact_year * discount_rate$`2.0% Ramsey`
      # print(paste("social cost carbon", social_cost_carbon))
      
      # print(social_cost_carbon)
      
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


test <- transit_expansion(ridership_increase = 100000,
                          route_type = "Core Local",
                          added_transit = 100000,
                          fleet_type = "diesel",
                          location = "Andover",
                          project_start = "2027-01-01",
                          project_lifetime = 5)
