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
tags$script(HTML('$(document).ready(function(){ $("[data-toggle=\'tooltip\']").tooltip(); });'))
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
                          )),
                          "This product uses the Census Bureau Data API but is not endorsed or certified by the Census Bureau."
                        )),
              nav_panel(title = "Electric Vehicles", 
                        navset_card_tab(
                          nav_panel(title = "EV Education and Outreach", 
                                    page_fillable(
                                      card(
                                        layout_column_wrap(
                                          width = 1/2,
                                          numericInput("no_participants", 
                                                       "Expected Number of Participants",
                                                       value = 4000),
                                          selectInput("ev_outreach_location",
                                                      "Location",
                                                      choices = unique(CommunityType$CTU_NAME),
                                                      selected = CommunityType$CTU_NAME[1]),
                                          dateInput("ev_outreach_project_start", 
                                                    "Year", 
                                                    value = "2024-01-01"),
                                          numericInput("ev_outreach_project_lifetime", 
                                                       "Project Lifetime (in years)",
                                                       value = 14),
                                          numericInput("conversion_rate", 
                                                       "Conversion Rate",
                                                       value = 0.04),
                                          radioButtons("audience", 
                                                       "Target Audience",
                                                       choices = c("Light Duty", "Heavy Duty"),
                                                       selected = "Light Duty")
                                        )
                                      ),
                                      card(dataTableOutput("ev_outreach_table"))
                                    )),
                          nav_panel(title = "Public Infrastructure Installation",
                                    page_fillable(
                                      card(
                                        layout_column_wrap(
                                          width = 1/2,
                                          radioButtons("ev_type", 
                                                       "Type of Vehicle Serviced",
                                                       choices = c("Light-Duty", "Heavy-Duty"),
                                                       selected = "Light-Duty"),
                                          radioButtons("charger_type", 
                                                       "Type of Charger Installed",
                                                       choices = c("DCFC", "Level 2"),
                                                       selected = "DCFC"),
                                          selectInput("ev_infrastructure_location",
                                                      "Location",
                                                      choices = unique(CommunityType$CTU_NAME),
                                                      selected = CommunityType$CTU_NAME[1]),
                                          textOutput("selected_community_type"),
                                          numericInput("no_chargers", 
                                                       "Number of Chargers",
                                                       value = 10),
                                          numericInput("charge_power", 
                                                       "Charger Power Level (kW)",
                                                       #  NEED TO BE SET BASED ON charger_type {L2=19.2, DCFC=150}
                                                       value = 150),
                                          numericInput("annual_hours_available", 
                                                       "Annual Hours Available",
                                                       value = 8760),
                                          dateInput("ev_infrastructure_project_start", 
                                                    "Year", 
                                                    value = "2024-01-01"),
                                          numericInput("ev_infrastructure_project_lifetime", 
                                                       "Project Lifetime (in years)",
                                                       value = 10),
                                          numericInput("utilization_rate",
                                          "Utilization",
                                          value = .6),
                                          numericInput("average_energy_efficiency",
                                                       "Vehicle Energy Efficiency (kWh/mile)",
                                                       value = 1),
                                          numericInput(
                                            "percentage_ICE",
                                            "Fraction of EV in Regional Fleet",
                                            value = NULL,
                                            min = 0,
                                            max = 1,
                                            step = 0.01
                                          )
                                        )
                                      ),
                                      card(dataTableOutput("ev_infrastructure_table"))
                                    )))),
              nav_panel(title = "Facilities/Hubs", 
                        navset_card_tab(
                          nav_panel(
                            title = "Mobility Hubs",
                            page_fillable(
                              card(
                                layout_column_wrap(
                                  width = 1/2,
                                  
                                  checkboxGroupInput("mobility_mode",
                                                     "Mobility Mode/s",
                                                     choices = TotalVMTReductionPotential$mobility_mode,
                                                     selected = TotalVMTReductionPotential$mobility_mode[1]),
                                  selectInput("hub_location",
                                              "Location",
                                              choices = unique(CommunityType$CTU_NAME),
                                              selected = CommunityType$CTU_NAME[1]),
                                  numericInput("population_3mile",
                                               "Population within the service area",
                                               #  Default=rural area
                                               value = 14137),
                                  dateInput("hub_project_start",
                                            "Year",
                                            value = "2024-01-01"),
                                  numericInput("hub_project_lifetime",
                                               "Project Lifetime (in years)",
                                               value = 20),
                                  numericInput("added_vmt",
                                               "Increase in Annual Transit VMT (Mile)",
                                               #  NEED DEFAULT
                                               value = 0),
                                  numericInput("reduction_potential",
                                               "Total VMT Reduction Potential",
                                               # VALUE SHOULD CHANGE BASED ON MODE SELECTIONS
                                               # DEFAULT (Pedestrian Facility)
                                               value = .058),
                                  numericInput("annual_vmt",
                                               "Annual VMT per capita",
                                               # VALUE SHOULD CHANGE BASED ON LOCATION
                                               # DEFAULT (Lindwood Twp.)
                                               value = 10655)
                                  
                                )
                              ),
                              card(dataTableOutput("mobility_hub_table"))
                            )),
                          nav_panel(
                            title = "Pedestrian Facilities",
                            page_fillable(
                              card(
                                layout_column_wrap(
                                  width = 1/2,
                                  selectInput("pedestrian_location",
                                              "Location",
                                              choices = unique(CommunityType$CTU_NAME),
                                              selected = CommunityType$CTU_NAME[1]),
                                  dateInput("pedestrian_project_start",
                                            "Year",
                                            value = "2024-01-01"),
                                  numericInput("pedestrian_project_lifetime",
                                               "Project Lifetime (in years)",
                                               value = 20),
                                  numericInput("average_daily_traffic", "Average Annual Daily traffic (two way) on road parallel or adjacent to facility", value = 6000),
                                  numericInput("one_way_facility_length", "Facility Length", value = 1),
                                  numericInput("no_key_destinations_25", "Number of Key Destinations within 0.25 mile", value = 1),
                                  numericInput("no_key_destinations_50", "Number of Key Destinations within 0.5 mile", value = 1),
                                  numericInput("annual_use_days", "Facility Annual Days of Use", value = 214),
                                  numericInput("average_trip_replaced", "Average Length of Auto Trip Replaced (Mile)", value = .86)
                                )
                              ),
                              card(dataTableOutput("pedestrian_facilities_table"))
                            )),
                          nav_panel(
                            title = "Multi-Use Trails and Bicycle Facilities",
                            page_fillable(
                              card(
                                layout_column_wrap(
                                  width = 1/2,
                                  selectInput("trails_bike_location",
                                              "Location",
                                              choices = unique(CommunityType$CTU_NAME),
                                              selected = CommunityType$CTU_NAME[1]),
                                  selectInput("facility_type",
                                              "Facility Type",
                                              choices = c("On Street", "New Multiuse",
                                                          "Conversion"),
                                              selected = "On Street"),
                                  dateInput("trails_bike_project_start",
                                            "Year",
                                            value = "2024-01-01"),
                                  numericInput("trails_bike_project_lifetime",
                                               "Project Lifetime (in years)",
                                               value = 20),
                                  numericInput("trails_bike_average_daily_traffic", "Average Annual Daily traffic (two way) on road parallel or adjacent to facility", value = 6000),
                                  numericInput("facility_length_range", "Facility Length", value = 1),
                                  numericInput("trails_bike_no_key_destinations_25", "Number of Key Destinations within 0.25 mile", value = 1),
                                  numericInput("trails_bike_no_key_destinations_50", "Number of Key Destinations within 0.5 mile", value = 1),
                                  numericInput("days_open", "Facility Annual Days of Use", value = 214),
                                  numericInput("length_trip_replaced_biking", "Average Length of Auto Trip Replaced (Mile)", value = 3.6)
                                )
                              ),
                              card(dataTableOutput("trails_bike_facilities_table"))
                            )
                          ))),
              nav_panel(title = "Behavioral Changes", 
                        navset_card_tab(
                          nav_panel(
                            title = "Employee Commute",
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
                                              choices = unique(CommunityType$CTU_NAME),
                                              selected = CommunityType$CTU_NAME[1]),
                                  numericInput("daily_commute_no", 
                                               "Number of Daily One-Way Commute Trips Reduced", 
                                               value = 1000),
                                  dateInput("project_start", 
                                            "Year", 
                                            value = "2024-01-01"),
                                  numericInput("project_lifetime", 
                                               "Project Lifetime (in years)", 
                                               value = 4)
                                  ,
                                  numericInput("average_commute",
                                               "Average One-way Commute Trip Distance (Mile)",
                                               value = 10.9),
                                  numericInput("working_days",
                                               "Annual Number of Working Days",
                                               value = 260)
                                )
                              ),
                              card(dataTableOutput("employee_commute_table"))
                            )),
                            nav_panel(
                            title = "Shared Mobility",
                            page_fillable(
                              card(
                                layout_column_wrap(
                                  width = 1/2,
                                  selectInput("fleet",
                                              "Mobility Type",
                                              choices = c("Bike", "Scooter", "Non-EV Rideshares", "EV Rideshares"),
                                              selected = "Bike"),
                                  selectInput("shared_mobility_location",
                                              "Shared Mobility Location",
                                              choices = unique(CommunityType$CTU_NAME),
                                              selected = CommunityType$CTU_NAME[1]),
                                  numericInput("no_trips", 
                                               "Number of Annual Trips per Vehicle/Equipment", 
                                               #  NEED DEFAULT
                                               value = 1000),
                                  dateInput("shared_mobility_project_start", 
                                            "Year", 
                                            value = "2024-01-01"),
                                  numericInput("shared_mobility_project_lifetime", 
                                               "Project Lifetime (in years)", 
                                               value = 8),
                                  numericInput("no_vehicles", 
                                               "Number of Daily Vehicle or Equipment Dispatched", 
                                               value = 1),
                                  numericInput("trip_miles", 
                                               "Average Length of Vehicle Trip Displaced (Mile)", 
                                               value = 20),
                                  numericInput("shared_mobility_adjustment_factor", 
                                               "Vehicle Dependency Adjustment Factor", 
                                               value = 1),
                                  numericInput("average_occupancy", 
                                               "Average Occupancy per Vehicle", 
                                               value = 20),
                                  numericInput("prct_deadhead_miles", 
                                               "Percent of Deadhead Miles", 
                                               value = .83),
                                  
                                )
                              ),
                              card(dataTableOutput("shared_mobility_table"))
                            )))),
              nav_panel(title = "Transit Expansion",
                        page_fillable(
                          card(
                            layout_column_wrap(
                                          width = 1/2,
                              selectInput("route_type",
                                          "Transit Service Type",
                                          choices = AdjustmentFactorsAndTripLengths$route_type,
                                          selected = AdjustmentFactorsAndTripLengths$route_type[1]),
                              selectInput("transit_expansion_location",
                                          "Location",
                                          choices = unique(CommunityType$CTU_NAME),
                                          selected = CommunityType$CTU_NAME[1]),
                              numericInput("ridership_increase", 
                                           "Ridership Increase", 
                                           #  Default from sample project: MetroTransit Micro G Line Exp
                                           value = 32976),
                              dateInput("transit_expansion_project_start", 
                                        "Year", 
                                        value = "2024-01-01"),
                              numericInput("transit_expansion_project_lifetime", 
                                           "Project Lifetime (in years)", 
                                           value = 14), 
                              numericInput("added_transit", 
                                           "Increase in Annual Transit VMT (Mile)", 
                                           #  Default from sample project: MetroTransit Micro G Line Exp
                                           value = 1566),
                                           #  NEED DEFAULT, NONEIN MEMO
                                           value = 1),
                              numericInput("average_trip_length", 
                                           "Average Auto Trip Replaced (Mile)", 
                                           value = AdjustmentFactorsAndTripLengths$adjustment_factor[1]), 
                              numericInput("transit_expansion_adjustment_factor", 
                                           "Transit Dependency Adjustment Factor", 
                                           value = AdjustmentFactorsAndTripLengths$adjustment_factor[1]),
                            )
                          ),
                          card(dataTableOutput("transit_expansion_table"))
                        ),
              nav_panel(title = "General Infrastructure Improvements",
                        navset_card_tab(
                          nav_panel(
                            title = "Intersection Delay",
                            page_fillable(
                              card(
                                layout_column_wrap(
                                  width = 1/2,
                                  selectInput("intersection_delay_location",
                                              "Location",
                                              choices = unique(CommunityType$CTU_NAME),
                                              selected = CommunityType$CTU_NAME[1]),
                                  numericInput("number_peak_hours",
                                               "Number of Peak Hours",
                                               #  NEED DEFAULT
                                               value = 1),
                                  dateInput("intersection_delay_project_start", 
                                            "Year", 
                                            value = "2024-01-01"),
                                  numericInput("intersection_delay_project_lifetime", 
                                               "Project Lifetime (in years)", 
                                               value = 7),
                                 numericInput("vehicle_per_hour",
                                               "Intersection Vehicle Per Hour",
                                               #  NEED DEFAULT, NONEIN MEMO
                                               value = 1),
                                  numericInput("peak_hour_delay_noBuild",
                                               "Peak Hour Delay Per Vehicle under No-Build Condition (Hour)",
                                               #  NEED DEFAULT
                                               value = 2),
                                  numericInput("peak_hour_delay_build",
                                               "Peak Hour Delay Per Vehicle under Build Condition (Hour)",
                                               #  NEED DEFAULT
                                               value = 1)
                                )
                              ),
                              card(dataTableOutput("intersection_delay_reductions_table"))
                            )
                          ),
                          nav_panel(
                            title = "Corridor Speed",
                            page_fillable(
                              card(
                                
                                layout_column_wrap(
                                  width = 1/2,
                                  selectInput("corridor_speed_location",
                                              "Location",
                                              choices = unique(CommunityType$CTU_NAME),
                                              selected = CommunityType$CTU_NAME[1]),
                                  numericInput("corridor_distance",
                                               "Corridor Distance (Mile)",
                                               #  NEED DEFAULT
                                               value = 1),
                                  dateInput("corridor_speed_project_start",
                                            "Project Start",
                                            value = "2024-01-01"),
                                   numericInput("corridor_speed_project_lifetime",
                                               "Project Lifetime (in years)",
                                               value = 7),
                                  numericInput("avg_annual_daily_traffic",
                                               "Annual average daily traffic under the no-build condition",
                                               #  NEED DEFAULT, NONEIN MEMO
                                               value = 1),
                                   numericInput("avg_corridor_speed_no_build",
                                               "Average Corridor Speed under the No-Build Condition (mph)",
                                               #  NEED DEFAULT
                                               value = 1),
                                  numericInput("avg_corridor_speed_build",
                                               "Average Corridor Speed under the Build Condition (mph)",
                                               #  NEED DEFAULT
                                               value = 1.2)
                                )
                              ),
                              card(dataTableOutput("corridor_speed_improvements_table"))
                            )
                          )
                          )),
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