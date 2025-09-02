#' Calculate Benefits of Transit Expansion Projects
#'
#' @param ridership_increase (numeric), Increase in annual transit ridership
#' @param route_type (character), Type of transit route (e.g., "Suburban Local", "Commuter Rail", "Bus Rapid Transit")
#' @param added_transit (numeric), Additional transit service added, used to calculate GHG impact
#' @param fuel_type (character), Type of fuel used by the added transit service (e.g., "Diesel", "Electric")
#' @param location (character), CTU name of the community where the project is located, used to assign community type
#' @param project_start (numeric), Year the project starts
#' @param project_lifetime (numeric), Length of the project in years
#' @param average_trip_length (numeric), Length of average auto trip replaced
#' @param adjustment_factor (numeric), Adjustment factor to account for transit dependency
#'
#' @returns
#' A data frame with VMT Reduction (miles), GHG Reduction (MT CO₂), and Social Cost of Carbon ($) Reduction for each year of the project, including totals.
#' @export
#'
#' @examples
#' transit_expansion(
#'   ridership_increase = 10000,
#'   route_type = "Suburban Local",
#'   added_transit = 50000,
#'   fuel_type = "Diesel",
#'   location = "Andover",
#'   project_start = 2025,
#'   project_lifetime = 14
#' )
transit_expansion <-
  function(ridership_increase,
           route_type,
           added_transit,
           fuel_type,
           location,
           project_start,
           project_lifetime,
           average_trip_length = NULL,
           adjustment_factor = NULL) {
    community_type <- CommunityTypeShape %>%
      filter(CTU_NAME == location) %>%
      pull(MappedCommunity)

    # Select average trip length by route type if it is NULL
    if (is.null(average_trip_length)) {
      average_trip_length <- AdjustmentFactorsAndTripLengths %>%
        filter(route_type == !!route_type) %>%
        pull(average_trip_length_mi_trip)
    }

    # Select adjustment factor by route type if it is NULL
    if (is.null(adjustment_factor)) {
      adjustment_factor <- AdjustmentFactorsAndTripLengths %>%
        filter(route_type == !!route_type) %>%
        pull(adjustment_factor)
    }

    # Generate years project covers
    project_start <- as.numeric(project_start) # Ensure numeric year
    project_years <- seq(project_start, project_start + project_lifetime - 1) # Create year range

    # Initialize vectors to store results
    vmt_displaced <- numeric(length(project_years))
    ghg_impact <- numeric(length(project_years))
    carbon_cost <- numeric(length(project_years))


    if (fuel_type == "Diesel") {
      fuel_type <- "diesel"
    }

    if (fuel_type == "Electric") {
      fuel_type <- "electricity"
    }

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

      # Filter to mapped community provided
      FleetData <- FleetData %>% filter(MappedCommunity == community_type)

      FleetData <- FleetData %>% mutate(year = as.numeric(year))

      # # Determine the closest year
      closest_year <- FleetData %>%
        summarise(closest_year = year[which.min(abs(year - current_year))]) %>%
        pull(closest_year)

      # # Filter the data set to get the fleet proportions from the closest year
      fleet_proportion <- FleetData %>%
        filter(year == closest_year)

      diesel_ef_year <- DieselEFsCommunityType %>%
        filter(year == current_year) %>%
        filter(MappedCommunity == community_type) %>%
        pull(EF)

      gasoline_ef_year <- GasolineEFsCommunityType %>%
        filter(year == current_year) %>%
        filter(MappedCommunity == community_type) %>%
        pull(EF)

      if (fuel_type == "diesel") {
        added_transit_fuel_ef <- diesel_ef_year
      } else {
        (
          added_transit_fuel_ef <- greet_ef_year[[fuel_type]]
        )
      }
      # Calculate GHG impact for the current year
      ghg_impact_year <- (
        (
          vmt_displaced_year * (
            gasoline_ef_year * fleet_proportion$gasoline +
              diesel_ef_year * fleet_proportion$diesel +
              greet_ef_year$electricity * fleet_proportion$electricity
          )
        ) - (
          added_transit * added_transit_fuel_ef
        )
      ) / 1000000

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
