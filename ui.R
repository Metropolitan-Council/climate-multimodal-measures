#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# page_fluid(
#   card(
#     # full_screen = TRUE,
#     max_height = 350,
#     card_header("My table"),
#     dataTableOutput("dt")
#   )
# )

# test <- dataTableOutput("dt")

page_navbar(
  title = "Metropolitan Council",
  bg = "#0062cc",
  underline = TRUE,
  # nav_panel(title = "Introduction", p("Overview of app")),
  nav_panel(title = "Calculations", 
            navset_pill_list(
              # nav_panel(title = "Map", 
              nav_panel(title = textOutput("map_tab_label"), 
                        page_fillable(
                          card(leaflet::leafletOutput( outputId = "myMap"
                                                       , height = 850
                          ),
                          verbatimTextOutput("tract_info")),
                          "This product uses the Census Bureau Data API but is not endorsed or certified by the Census Bureau."
                        )),
              # nav_panel(title = "Employee Commute Reduction", !!!employee_commute_reduction),
              nav_panel(title = "Employee Commute Reduction",
                        page_fillable(
                          card(
                            layout_column_wrap(
                              width = 1/2,
                              # NEED TO SET FROM CommunityTypeShape
                              # selectInput("community_type",
                              #             "Community Type",
                              #             choices = unique(VMTByCommunityType$CD),
                              #             selected = VMTByCommunityType$CD[1]),
                              selectInput("location",
                                          "Location",
                                          choices = unique(FleetData$ctu),
                                          selected = FleetData$ctu[1]),
                              numericInput("daily_commute_no", 
                                           "Number of Daily One-Way Commute Trips Reduced", 
                                           value = 1000),
                              dateInput("project_start", 
                                        "Project Start", 
                                        value = "2024-01-01"),
                              numericInput("project_lifetime", 
                                           "Project Lifetime (in years)", 
                                           value = 4)
                              # ,
                              # numericInput("average_commute", 
                              #              "Average Commute Distance (in miles)", 
                              #              value = 10.9)
                            )
                          ),
                          card(dataTableOutput("employee_commute_table"))
                        )),
              nav_panel(title = "EV Outreach Reduction",
                        page_fillable(
                          card(
                            layout_column_wrap(
                              width = 1/2,
                              numericInput("no_participants", 
                                           "Number of Participants",
                                           #  NEED DEFAULT
                                           value = 0),
                              dateInput("ev_outreach_project_start", 
                                        "Project Start", 
                                        value = "2024-01-01"),
                              numericInput("ev_outreach_project_lifetime", 
                                           "Project Lifetime (in years)",
                                           #  NEED DEFAULT
                                           value = 1),
                              numericInput("conversion_rate", 
                                           "Conversion Rate",
                                           #  NEED DEFAULT
                                           value = 0),
                              radioButtons("audience", 
                                           "Audience",
                                           choices = c("Light Duty", "Heavy Duty"),
                                           selected = "Light Duty")
                            )
                          ),
                          card(dataTableOutput("ev_outreach_table"))
                        )),
              nav_panel(title = "EV Infrastructure",
                        page_fillable(
                          card(
                            layout_column_wrap(
                              width = 1/2,
                              radioButtons("ev_type", 
                                           "EV Type",
                                           choices = c("Light-Duty", "Heavy-Duty"),
                                           selected = "Light-Duty"),
                              radioButtons("charger_type", 
                                           "Charger Type",
                                           choices = c("DCFC", "Level 2"),
                                           selected = "DCFC"),
                              selectInput("ev_infrastructure_location",
                                          "EV Infrastructure Location",
                                          choices = unique(FleetData$ctu),
                                          selected = FleetData$ctu[1]),
                              numericInput("no_chargers", 
                                           "Number of Chargers",
                                           #  NEED DEFAULT
                                           value = 0),
                              numericInput("charge_power", 
                                           "Charge Power",
                                           #  NEED DEFAULT
                                           value = 0),
                              numericInput("annual_hours_available", 
                                           "Annual Hours Available",
                                           #  NEED DEFAULT
                                           value = 0),
                              dateInput("ev_infrastructure_project_start", 
                                        "Project Start", 
                                        value = "2024-01-01"),
                              numericInput("ev_infrastructure_project_lifetime", 
                                           "Project Lifetime (in years)",
                                           #  NEED DEFAULT
                                           value = 1)
                            )
                          ),
                          card(dataTableOutput("ev_infrastructure_table"))
                        )),
              nav_panel(title = "Shared Mobility",
                        page_fillable(
                          card(
                            layout_column_wrap(
                              width = 1/2,
                              selectInput("fleet",
                                          "Fleet Type",
                                          choices = c("Bike", "Scooter", "Non-EV Rideshares", "EV Rideshares"),
                                          selected = "Bike"),
                              selectInput("shared_mobility_location",
                                          "Shared Mobility Location",
                                          choices = unique(FleetData$ctu),
                                          selected = FleetData$ctu[1]),
                              numericInput("no_trips", 
                                           #  confirm title of input
                                           "Number of Daily One-Way Commute Trips Reduced", 
                                           #  NEED DEFAULT
                                           value = 1000),
                              dateInput("shared_mobility_project_start", 
                                        "Project Start", 
                                        value = "2024-01-01"),
                              numericInput("shared_mobility_project_lifetime", 
                                           "Project Lifetime (in years)", 
                                           #  NEED DEFAULT
                                           value = 1),
                              numericInput("no_vehicles", 
                                           "Number of Vehicles", 
                                           #  NEED DEFAULT
                                           value = 1)
                            )
                          ),
                          card(dataTableOutput("shared_mobility_table"))
                        )),
              nav_panel(title = "Transit Expansion",
                        page_fillable(
                          card(
                            layout_column_wrap(
                              width = 1/2,
                              selectInput("route_type",
                                          "Route Type",
                                          choices = AdjustmentFactorsAndTripLengths$route_type,
                                          selected = AdjustmentFactorsAndTripLengths$route_type[1]),
                              selectInput("transit_expansion_location",
                                          "Location",
                                          choices = unique(FleetData$ctu),
                                          selected = FleetData$ctu[1]),
                              numericInput("ridership_increase", 
                                           "Ridership Increase", 
                                           #  NEED DEFAULT
                                           value = 1),
                              dateInput("transit_expansion_project_start", 
                                        "Project Start", 
                                        value = "2024-01-01"),
                              numericInput("transit_expansion_project_lifetime", 
                                           "Project Lifetime (in years)", 
                                           #  NEED DEFAULT
                                           value = 20),
                              numericInput("added_transit", 
                                           "Added Transit", 
                                           #  NEED DEFAULT, NONEIN MEMO
                                           value = 1)
                            )
                          ),
                          card(dataTableOutput("transit_expansion_table"))
                        )),
              # nav_panel(title = "Three", p("Third tab content"))
              widths = c(2, 10)
            )),
  nav_panel(title = "Sources", 
            page_fillable(
              card(
                h2("Employee Commute VMT Reduction"),
                p(""),
                br(),
                
                h2("Shared Mobility"),
                p(""),
                br(),
                
                h2("Electric Vehicle Education & Outreach "),
                p(""),
                br(),
                
                h2("Public Outreach Infrastructure"),
                p(""),
                br(),
                
                h2("Transit Expansion"),
                p("")
              )
            )
  )   #,
  # nav_spacer(),
  # nav_menu(
  #   title = "Links",
  #   align = "right"
  # ,
  # nav_item(link_shiny),
  # nav_item(link_posit)
  # )
)