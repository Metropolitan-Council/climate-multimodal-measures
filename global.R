#Loading Packages
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
library(mapview)
library(leaflet)
library(rmapshaper)
options(tigris_use_cache = TRUE)

#Reading in our background data
backgroundDataPath <- paste0(here::here(),"/data/raw data/MetCouncilTables.xlsx")

backgroundDataNames <- excel_sheets(backgroundDataPath)

for (sheet in backgroundDataNames) {
  assign(sheet, read_excel(backgroundDataPath, sheet = sheet), envir = .GlobalEnv)
}

FleetData <- read_xlsx(paste0(here::here(),"/data/raw data/FleetData.xlsx"))
CommunityDesignation <- st_read(paste0(here::here(),"/data/raw data/shp_society_thrive_msp2040_com_des/ThriveMSP2040CommunityDesignation.shp"))
CommunityType <- CommunityDesignation
source(paste0(getwd(), "/data/community_type_mapping.R"))
source(paste0(getwd(), "/data/EFs_by_community_type.R"))
source(paste0(getwd(), "/data/stock_percentages_ctu.R"))
source(paste0(getwd(), "/data/vmt_per_vehicle.R"))
source(paste0(getwd(), "/data/vmt_per_capita.R"))

added_functions <- c("employee_commute", "ev_outreach", "ev_infrastructure",
                     "shared_mobility", "transit_expansion", "mobility_hubs",
                     "pedestrian_facilities", "trails_bike_facilities")
for(added_function in added_functions) {
  source(paste0(getwd(),"/R Scripts/", added_function, ".R"))
}

population <- get_acs(
  geography = "tract",
  table = "B01003",
  state = "MN",
  geometry = TRUE,
  cache_table = TRUE
) %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

population <- ms_simplify(population, keep = 0.05,
                          keep_shapes = TRUE)

locations <- CommunityDesignation %>%
  st_transform(., crs = 4326)

# location_bounds <- st_bbox(locations) 

met_council_datatable <- function(provided_data) {
  
  formatted_data <- provided_data
  
  pretty_names <- colnames(formatted_data) %>%
    str_replace_all("_", " ") %>%  
    str_to_title() %>%
    str_replace_all("Vmt", "VMT") %>%
    str_replace_all("Ghg", "GHG") 
  colnames(formatted_data) <- pretty_names
  cols_to_center <- setdiff(pretty_names, "Year")
  
  numeric_cols <- colnames(formatted_data)[sapply(formatted_data, class)=="numeric"]
  formatted_data[numeric_cols] <- round(formatted_data[numeric_cols], digits = 3) 
  
  datatable(formatted_data %>%
              mutate(across(numeric_cols, ~ formatC(.x, big.mark = ",", 
                                                    format = "f",
                                                    drop0trailing = TRUE))), 
            fillContainer = TRUE,
            rownames = FALSE,
            options = list(
              searching = FALSE,  # Disable search
              paging = FALSE,     # Disable pagination
              dom = 't',          # Show only the table (no extra controls)
              ordering = FALSE,    # Disable ordering
              columnDefs = list(
                list(targets = which(pretty_names != "Year") - 1,  
                     className = 'dt-center')))) %>%
    formatStyle('Year', target = 'row', 
                fontWeight = styleEqual("Total", c('bold'))) %>%
    formatStyle(cols_to_center, textAlign = 'center')
}