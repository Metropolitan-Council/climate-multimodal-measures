#Loading Packages
library(shiny)
# library(shinydashboard)
library(bslib)
library(readxl)
library(tidyverse)
# library(here)
library(DT)
library(lubridate)


#Reading in our background data
backgroundDataPath <- paste0(here::here(),"/data/MetCouncilTables.xlsx")

backgroundDataNames <- excel_sheets(backgroundDataPath)

for (sheet in backgroundDataNames) {
  assign(sheet, read_excel(backgroundDataPath, sheet = sheet), envir = .GlobalEnv)
}

FleetData <- read_xlsx(paste0(here::here(),"/data/FleetData.xlsx"))

source(paste0(getwd(),"/R Scripts/employee_commute.R"))
