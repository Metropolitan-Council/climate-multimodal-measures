#Loading Packages
library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse)
library(here)


#Reading in our background data
backgroundDataPath <- paste0(here::here(),"/data/MetCouncilTables.xlsx")

backgroundDataNames <- excel_sheets(backgroundDataPath)

for (sheet in backgroundDataNames) {
  assign(sheet, read_excel(backgroundDataPath, sheet = sheet), envir = .GlobalEnv)
}
