# Loading Packages
library(scales)
library(shiny)
library(bslib)
library(readxl)
library(tidyverse)
library(here)
library(DT)
library(lubridate)
library(sf)
library(tidycensus)
library(tigris)
library(leaflet)
library(rmapshaper)
library(shinyBS)
library(councilR)
library(shinyWidgets)

backgroundDataPath <- paste0(here::here(), "/data/raw/MetCouncilTables.xlsx")

backgroundDataNames <- excel_sheets(backgroundDataPath)

for (sheet in backgroundDataNames) {
  assign(sheet, read_excel(backgroundDataPath, sheet = sheet), envir = .GlobalEnv)
}

FleetData <- read_xlsx(paste0(here::here(), "/data/raw/FleetData.xlsx")) %>%
  filter(!mode %in% c("RU", "RI"))

CommunityDesignation <- st_read(paste0(here::here(), "/data/raw/PROPOSED2050COMMUNITYDESIGNATIONS.gpkg")) %>%
  rename(COMDESNAME = CD2050)
CommunityType <- CommunityDesignation
mpo_area <- import_from_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/trans_metro_planning_org_area/gpkg_trans_metro_planning_org_area.zip") %>% st_zm()

source(paste0(getwd(), "/data/community_type_mapping.R"))
source(paste0(getwd(), "/data/EFs_by_community_type.R"))
source(paste0(getwd(), "/data/stock_percentages.R"))
source(paste0(getwd(), "/data/vmt_per_vehicle.R"))
source(paste0(getwd(), "/data/vmt_per_capita.R"))

added_functions <- c(
  "employee_commute", "ev_outreach", "ev_infrastructure",
  "shared_mobility", "transit_expansion", "mobility_hubs",
  "pedestrian_facilities", "trails_bike_facilities",
  "corridor_speed_improvement", "intersection_delay_reductions"
)
for (added_function in added_functions) {
  source(paste0(getwd(), "/R/", added_function, ".R"))
}

population <- get_acs(
  geography = "tract",
  table = "B01003",
  state = "MN",
  geometry = TRUE,
  cache_table = TRUE
) %>%
  sf::st_transform("+proj=longlat +datum=WGS84")

population <- ms_simplify(population,
  keep = 0.05,
  keep_shapes = TRUE
)

locations <- CommunityDesignation %>%
  st_transform(., crs = 4326)


met_council_datatable <- function(provided_data) {
  formatted_data <- provided_data
  rownames(formatted_data) <- NULL # ✅ Remove row names explicitly

  pretty_names <- colnames(formatted_data) %>%
    str_replace_all("_", " ") %>%
    str_to_title() %>%
    str_replace_all("Vmt", "VMT") %>%
    str_replace_all("Ghg", "GHG")
  colnames(formatted_data) <- pretty_names
  cols_to_center <- setdiff(pretty_names, "Year")

  numeric_cols <- colnames(formatted_data)[sapply(formatted_data, class) == "numeric"]
  formatted_data[numeric_cols] <- round(formatted_data[numeric_cols], digits = 4)

  datatable(
    formatted_data %>%
      mutate(across(numeric_cols, ~ formatC(.x,
        big.mark = ",",
        format = "f",
        drop0trailing = TRUE
      ))),
    fillContainer = TRUE,
    rownames = FALSE,
    options = list(
      rownames = FALSE,
      searching = FALSE, # Disable search
      paging = FALSE, # Disable pagination
      # dom = 't',          # Show only the table (no extra controls)
      ordering = FALSE, # Disable ordering
      columnDefs = list(
        list(
          targets = which(pretty_names != "Year") - 1,
          className = "dt-center"
        )
      )
    )
  ) %>%
    formatStyle("Year",
      target = "row",
      fontWeight = styleEqual("Total", c("bold"))
    ) %>%
    formatStyle(cols_to_center, textAlign = "center")
}

additional_sources <- readxl::read_excel("data/raw/input_default_values_sources.xlsx")


source_citations <-
  purrr::map_dfr(
    c(
      "AdjustmentFactorsAndTripLengths", "TripDistances",
      "DefaultLifetime", "AnnualVMT", "VehiclePopulation", "TransitDependencyAdjustments",
      "GREETCarbonIntensity", "FuelEfficiency", "VMTByCommunityType",
      "TotalVMTReductionPotential", "ChargerUtilizationRatesAndPower",
      "ModeShiftFactor", "CreditForKeyDestinations", "SocialCostCarbon"
    ),
    function(x) {
      pretty_name <- paste(str_split(x, "(?<=[a-z])(?=[A-Z])|(?<=[A-Z])(?=[A-Z][a-z])")[[1]], collapse = " ")

      data_source <- get(x) %>%
        select(data_source, source_note) %>%
        filter(!is.na(data_source)) %>%
        unique()

      tibble(
        sheet = x,
        Table = pretty_name,
        Source = data_source$data_source,
        Description = data_source$source_note
      )
    }
  )
