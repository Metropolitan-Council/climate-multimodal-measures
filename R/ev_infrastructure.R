#'  Calculate Benefits of Public Infrastructure Projects
#'
#' @param no_chargers (numeric), Number of chargers of a certain power level
#' @param charger_type (logical), DCFC or Level 2
#' @param charge_power (numeric), Power of the charger in kW
#' @param annual_hours_available (numeric), Annual hours the charger is available for use
#' @param location (character), CTU name of the community where the project is located, used to assign community type
#' @param project_start (numeric), Year the project starts
#' @param project_lifetime (numeric), Lifetime of the project in years
#' @param utilization_rate (numeric), Average charger utilization rate; the percentage of time when a charger is actively in use
#' @param average_energy_efficiency (numeric), Average energy efficiency of the vehicle type in Wh per mile; if not provided, defaults to Light-Duty vehicle efficiency
#' @param percentage_ICE (numeric), Percentage of Internal Combustion Engine vehicles in the fleet; if not provided, defaults to the percentage from the closest year in the fleet data
#'
#' @returns
#'  A data frame with VMT Reduction (miles), GHG Reduction (MT CO2), and Social Cost of Carbon ($) Reduction for each year of the project, including totals.
#' @export
#'
#' @examples
#' ev_infrastructure(
#'   no_chargers = 10,
#'   charger_type = "DCFC",
#'   charge_power = 150,
#'   annual_hours_available = 8760,
#'   location = "Andover",
#'   project_start = 2025,
#'   project_lifetime = 10,
#'   utilization_rate = 3.75,
#'   average_energy_efficiency = 378,
#'   percentage_ICE = .99
#' )
ev_infrastructure <- function( # ev_type, #commented in case we want to include MD later
                              no_chargers,
                              charger_type,
                              charge_power,
                              annual_hours_available,
                              location,
                              project_start,
                              project_lifetime,
                              utilization_rate = NULL,
                              average_energy_efficiency = NULL,
                              percentage_ICE = NULL) {
  # Get the community type based on the selected location
  community_type <- CommunityTypeShape %>%
    filter(CTU_NAME == location) %>%
    pull(MappedCommunity)

  # Set default utilization rate if not provided
  if (is.null(utilization_rate)) {
    utilization_rate <- if (charger_type == "DCFC") {
      ChargerUtilizationRates$DC_fast
    } else {
      ChargerUtilizationRates$level_2
    }
  }

  utilization_rate <- utilization_rate / 100

  # Set default average energy efficiency if not provided
  if (is.null(average_energy_efficiency)) {
    average_energy_efficiency <- FuelEfficiency %>%
      filter(`Vehicle Type` == "Light-Duty") %>%
      pull(`Average of Fuel Economy Combined (kWh/100 mi)`) / 100
  }

  # Generate years project covers based on project start date and length of project
  project_start <- as.numeric(project_start) # Ensure numeric year
  project_years <- seq(project_start, project_start + project_lifetime - 1) # Create year range

  # Calculate default percentage_ICE for the first year
  FleetData <- FleetData %>%
    filter(MappedCommunity == community_type) %>%
    mutate(year = as.numeric(year))

  closest_year_first <- FleetData %>%
    summarise(closest_year = year[which.min(abs(year - project_start))]) %>%
    pull(closest_year)

  default_fleet_proportion <- FleetData %>%
    filter(year == closest_year_first) %>%
    pull(electricity)

  default_percentage_ICE <- round(100 - default_fleet_proportion, 2)
  percentage_ICE <- round(100 - percentage_ICE, 2)

  # Check if user-provided percentage_ICE matches the default
  use_dynamic_ICE <- is.null(percentage_ICE) || round(percentage_ICE, 2) == default_percentage_ICE

  # Initialize vectors to store results
  vmt_displaced <- numeric(length(project_years))
  ghg_impact <- numeric(length(project_years))
  carbon_cost <- numeric(length(project_years))

  # Loop through each project year
  for (i in seq_along(project_years)) {
    current_year <- project_years[i]

    # Decide on the percentage_ICE to use
    if (use_dynamic_ICE) {
      # Determine the closest year
      closest_year <- FleetData %>%
        summarise(closest_year = year[which.min(abs(year - current_year))]) %>%
        pull(closest_year)

      # Get fleet proportions for the closest year and calculate ICE percentage
      fleet_proportion <- FleetData %>%
        filter(year == closest_year) %>%
        pull(electricity)

      current_percentage_ICE <- round(100 - fleet_proportion, 2)
    } else {
      current_percentage_ICE <- percentage_ICE
    }

    # Convert percentage to a fraction for calculations
    current_percentage_ICE_fraction <- current_percentage_ICE / 100

    # Calculate VMT displaced for the current year
    vmt_displaced_year <- (no_chargers * charge_power * utilization_rate * annual_hours_available) /
      average_energy_efficiency * current_percentage_ICE_fraction

    # Get GHG emission factors
    greet_ef_year <- GREETCarbonIntensity %>% filter(Year == current_year)
    diesel_ef_year <- DieselEFsCommunityType %>%
      filter(year == current_year, MappedCommunity == community_type) %>%
      pull(EF)
    gasoline_ef_year <- GasolineEFsCommunityType %>%
      filter(year == current_year, MappedCommunity == community_type) %>%
      pull(EF)

    # Determine carbon intensity based on vehicle type
    # carbon_intensity <- if (ev_type == "Light-Duty") gasoline_ef_year else diesel_ef_year
    carbon_intensity <- gasoline_ef_year
    carbon_intensity_grid <- greet_ef_year %>% pull(electricity)

    # Calculate GHG impact
    ghg_impact_year <- vmt_displaced_year * (carbon_intensity - carbon_intensity_grid) / 1e6

    # Calculate social cost of carbon
    discount_rate <- SocialCostCarbon %>%
      filter(`emission.year` == current_year, gas == "CO2") %>%
      pull(`2.0% Ramsey`)

    social_cost_carbon <- ghg_impact_year * discount_rate

    # Store results
    vmt_displaced[i] <- vmt_displaced_year
    ghg_impact[i] <- ghg_impact_year
    carbon_cost[i] <- social_cost_carbon
  }

  # Calculate totals
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

test <- ev_infrastructure(
  no_chargers = 10,
  charger_type = "DCFC",
  charge_power = 150,
  annual_hours_available = 8760,
  location = "Andover",
  project_start = 2025,
  project_lifetime = 10,
  utilization_rate = 3.75,
  average_energy_efficiency = 378,
  percentage_ICE = .99
)
