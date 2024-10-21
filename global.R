#Loading Packages
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
backgroundDataPath <- paste0(here::here(),"/data/MetCouncilTables.xlsx")

backgroundDataNames <- excel_sheets(backgroundDataPath)

for (sheet in backgroundDataNames) {
  assign(sheet, read_excel(backgroundDataPath, sheet = sheet), envir = .GlobalEnv)
}

FleetData <- read_xlsx(paste0(here::here(),"/data/FleetData.xlsx"))
CommunityTypeShape <- st_read(paste0(here::here(),"/data/shp_society_thrive_msp2040_com_des/ThriveMSP2040CommunityDesignation.shp"))
source(paste0(getwd(), "/data/community_type_mapping.R"))
source(paste0(getwd(), "/data/stock_percentages_ctu.R"))

added_functions <- c("employee_commute", "ev_outreach", "ev_infrastructure",
                     "shared_mobility", "transit_expansion")
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

locations <- st_read(paste0(getwd(),"/data/shp_society_thrive_msp2040_com_des/ThriveMSP2040CommunityDesignation.shp")) %>%
  st_transform(., crs = 4326)

# location_bounds <- st_bbox(locations) 
