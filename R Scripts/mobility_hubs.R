mobility_hubs <-
  function(mobility_mode,
           added_vmt,
           project_lifetime,
           project_start,
           location,
           population_3mile = NULL,
           reduction_potential = NULL,
           annual_vmt = NULL) {
    
    community_type <- CommunityTypeShape %>% 
      filter(CTU_NAME == location) %>% 
      pull(MappedCommunity)
    
    if (is.null(reduction_potential)) {
      reduction_potential <- TotalVMTReductionPotential %>%
        filter(mobility_mode %in% mobility_mode) %>% # Filter to include all selected modes
        summarise(total_vmt_reduction_potential = sum(total_vmt_reduction_potential, na.rm = TRUE)) %>%
        pull(total_vmt_reduction_potential)
    }
    
    if (is.null(annual_vmt)) {
      annual_vmt = VMTPerCapitaByCommunityType %>% filter(MappedCommunity == community_type) %>% pull(VMTperCapita)
    }
    
    # Generate years project covers
    project_start <- as.numeric(project_start)  # Ensure numeric year
    project_years <- seq(project_start, project_start + project_lifetime - 1)  # Create year range
    
    # Initialize vectors to store results
    auto_vmt_displaced <- numeric(length(project_years))
    ghg_impact <- numeric(length(project_years))
    carbon_cost <- numeric(length(project_years))
    
    for (i in seq_along(project_years)) {
      current_year <- project_years[i]
      
      # Calculate auto VMT displaced
      vmt_displaced_year <- reduction_potential * population_3mile * annual_vmt
      
      # Calculate GHG impact
      greet_ef_year <- GREETCarbonIntensity %>% filter(Year == current_year)
      
      # Filter to Mapped Community provided
      FleetData <- FleetData %>% filter(MappedCommunity == community_type)
      
      FleetData <- FleetData %>% mutate(year = as.numeric(year))
      
      # Determine the closest year
      closest_year <- FleetData %>%
        summarise(closest_year = year[which.min(abs(year - current_year))]) %>%
        pull(closest_year)
      
      # Filter the data set to get the fleet proportions from the closest year
      fleet_proportion <- FleetData %>%
        filter(year == closest_year)
      
      diesel_ef_year <- DieselEFsCommunityType %>% filter(year == current_year) %>% filter(MappedCommunity == community_type) %>% pull(EF)
      
      gasoline_ef_year <- GasolineEFsCommunityType %>% filter(year == current_year) %>% filter(MappedCommunity == community_type) %>% pull(EF)
      
      ghg_impact_year <- (((
        vmt_displaced_year * gasoline_ef_year * fleet_proportion$gasoline
      ) +
        (
          vmt_displaced_year * diesel_ef_year * fleet_proportion$diesel
        ) +
        (
          vmt_displaced_year * greet_ef_year$electricity * fleet_proportion$electricity
        )
      ) -
        (added_vmt * greet_ef_year$diesel)) / 1000000
      
      #################################################################################################
      
      # Calculate social cost of carbon for the current year
      discount_rate <-
        SocialCostCarbon %>% filter(`emission.year` == current_year &
                                      gas == "CO2")
      social_cost_carbon <-
        ghg_impact_year * discount_rate$`2.0% Ramsey`
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
      Year = c(project_years, "Total"),
      "VMT Reduction (Miles)" = format(round(c(auto_vmt_displaced, total_vmt_displaced), 0), big.mark = ","),
      "GHG Reduction (MT CO₂)" = format(round(c(ghg_impact, total_ghg_impact),0), big.mark = ","),
      "Carbon Cost Reduction ($) <i class='fas fa-question-circle' 
   title='The Social Cost of Carbon estimates the economic savings from avoiding one ton of CO₂ emissions, reflecting reduced damages to agriculture, human health, infrastructure, and ecosystems. Using a 2% Ramsey discount rate, future damages are valued at 98% of their present value.'></i>" = 
        format(round(c(carbon_cost, total_carbon_cost), 0), big.mark = ","),
      check.names = FALSE
    )
    
    return(results)
  }

