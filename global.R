#Loading Packages
library(shiny)
library(bslib)
library(readxl)
library(tidyverse)
library(here)
library(DT)
library(lubridate)
library(sf)


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
source(paste0(getwd(),"/R Scripts/employee_commute.R"))