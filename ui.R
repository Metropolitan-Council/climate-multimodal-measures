# #
# # This is the user-interface definition of a Shiny web application. You can
# # run the application by clicking 'Run App' above.
# #
# # Find out more about building applications with Shiny here:
# #
# #    http://shiny.rstudio.com/
# #
#
# Load External Dependencies (Placed Once)
tags$head(
  tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/bootstrap/5.1.3/js/bootstrap.bundle.min.js"),
  tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
  
  tags$script(
    HTML(
      '$(document).ready(function(){
          // Initialize static tooltips and popovers
          $("[data-bs-toggle=\'tooltip\']").tooltip();
          $("[data-bs-toggle=\'popover\']").popover({ trigger: "hover", html: true });
        });

        // Use event delegation to initialize popovers for dynamically added elements
        $(document).on("mouseenter", "[data-bs-toggle=\'popover\']", function(){
          var $el = $(this);
          // If the popover hasn\'t been initialized yet, initialize it
          if (!$el.data("bs.popover")) {
            $el.popover({ trigger: "hover", html: true });
            $el.popover("show");
          }
        });
      '
    )
  )
)


# Main Container
div(
  class = "main-container",
  # Custom Class for Styling
  style = "width: 100vw; height: 100vh; padding: 0; margin: 0; overflow: hidden; display: flex; flex-direction: column;",
  
  # Header Section (Above Navbar)
  div(
    class = "header-section",
    style = "display: flex; align-items: center; padding: 10px; background-color: #f8f9fa; border-bottom: 2px solid #ddd;",
    tags$img(
      src = "main-logo.png",
      height = "50px",
      style = "margin-right: 15px;"
    ),
    h2("Transportation Project Emission Reduction Calculator", class = "header-title")
  ),
  
  # Nested Page Navbar
  div(
    class = "navbar-wrapper",
    style = "flex-grow: 1; display: flex; flex-direction: column; background-color: white; padding: 15px;",
    
    
    page_navbar(
      # Add dependencies for Bootstrap 5 and FontAwesome
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
        tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
        tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/bootstrap/5.1.3/js/bootstrap.bundle.min.js"),
        tags$script(
          HTML(
            '
          $(document).ready(function() {
            const tooltipTriggerList = document.querySelectorAll(\'[data-bs-toggle="tooltip"]\');
            tooltipTriggerList.forEach(tooltipTriggerEl => {
              new bootstrap.Tooltip(tooltipTriggerEl);
            });
          });
          '
          )
        )
      ),
      
      
      # Define Navigation Panels
      nav_panel(
        title = tags$span("Calculations", style = "color: #002b5c; font-weight: bold; font-size: 18px"),
        navset_pill_list(
          nav_panel(
            title = textOutput("map_tab_label"),
            page_fillable(
              card(leaflet::leafletOutput(outputId = "myMap"
                                          , height = 850)),
              "This product uses the Census Bureau Data API but is not endorsed or certified by the Census Bureau."
            )
          ),
          nav_panel(title = "Electric Vehicles", navset_card_tab(
            nav_panel(title = "EV Education and Outreach", page_fillable(
              card(
                layout_column_wrap(
                  width = 1 / 2,
                  shinyWidgets::autonumericInput(
                    "no_participants",
                    "Expected Number of Participants",
                    value = 4000,
                    decimalPlaces = 0,
                    align = 'left',
                    emptyInputBehavior = "zero"
                  ),
                  selectInput(
                    "ev_outreach_location",
                    "Location",
                    choices = unique(CommunityType$CTU_NAME),
                    selected = CommunityType$CTU_NAME[2]
                  ),
                  selectInput(
                    "ev_outreach_project_start",
                    "Year",
                    choices = seq(2024, 2050, 1),
                    selected = 2024
                  ),
                  numericInput(
                    "ev_outreach_project_lifetime",
                    "Project Lifetime (in years)",
                    value = 8
                  ),
                  div(
                    style = "display: flex; align-items: center;",
                    numericInput(
                      "conversion_rate",
                      "Conversion Rate",
                      value = 0.04,
                      min = 0,
                      max = 1,
                      step = 0.01
                    ),
                    tags$i(
                      class = "fas fa-question-circle",
                      style = "margin-left: 5px; cursor: pointer;",
                      `data-bs-toggle` = "tooltip",
                      `data-bs-placement` = "right",
                      title = "Fraction of participants who go on to purchase an EV"
                    )
                  ),
                  # radioButtons(
                  #   "audience",
                  #   "Target Audience",
                  #   choices = c("Light Duty", "Heavy Duty"),
                  #   selected = "Light Duty"
                  # )
                ),
                layout_column_wrap(
                  width = 2,
                  height = "auto",
                  tags$div(class = "info-box", textOutput("selected_community_type_EVOutreach")),
                  tags$div(class = "info-box", textOutput("average_annual_accrual"))
                )
              ),
              card(uiOutput("ev_outreach_ui"))
            )),
            nav_panel(title = "Public Infrastructure Installation", page_fillable(
              card(
                layout_column_wrap(
                  width = 1 / 2,
                  # radioButtons(
                  #   "ev_type",
                  #   "Type of Vehicle Serviced",
                  #   choices = c("Light-Duty", "Heavy-Duty"),
                  #   selected = "Light-Duty"
                  # ),
                  radioButtons(
                    "charger_type",
                    "Type of Charger Installed",
                    choices = c("DCFC", "Level 2"),
                    selected = "DCFC"
                  ),
                  selectInput(
                    "ev_infrastructure_location",
                    "Location",
                    choices = unique(CommunityType$CTU_NAME),
                    selected = CommunityType$CTU_NAME[2]
                  ),
                  numericInput("no_chargers", "Number of Chargers", value = 10),
                  numericInput("charge_power", "Charger Power Level (kW)", value = 150),
                  shinyWidgets::autonumericInput(
                    "annual_hours_available",
                    "Annual Hours Available",
                    value = 8760,
                    decimalPlaces = 0,
                    align = 'left',
                    emptyInputBehavior = "zero"
                  ),
                  selectInput(
                    "ev_infrastructure_project_start",
                    "Year",
                    choices = seq(2024, 2050, 1),
                    selected = 2024
                  ),
                  numericInput(
                    "ev_infrastructure_project_lifetime",
                    "Project Lifetime (in years)",
                    value = 10
                  ),
                  # numericInput("utilization_rate", "Utilization", value = .6),
                  # numericInput(
                  #   "average_energy_efficiency",
                  #   "Vehicle Energy Efficiency (kWh/mile)",
                  #   value = 1
                  # ),
                  div(
                    style = "display: flex; align-items: center;",
                    numericInput(
                      "utilization_rate",
                      "Utilization Rate",
                      value = .5,
                      min = 0,
                      max = 1,
                      step = 0.01
                    ),
                    tags$i(
                      class = "fas fa-question-circle",
                      style = "margin-left: 5px; cursor: pointer;",
                      `data-bs-toggle` = "tooltip",
                      `data-bs-placement` = "right",
                      title = "Percentage of time a charging station is used"
                    )
                  ),
                  numericInput(
                    "percentage_ICE",
                    "Fraction of EV in Regional Fleet",
                    value = NULL,
                    min = 0,
                    max = 1,
                    step = 0.01
                  )
                ),
                tags$div(
                  class = "info-box",
                  textOutput("selected_community_type_EVInfrastructure")
                )
              ), card(uiOutput("ev_infrastructure_ui"))
            ))
          )),
          
          nav_panel(title = "Transit Expansion", navset_card_tab(
            nav_panel(title = "Transit Expansion", page_fillable(
              card(
                layout_column_wrap(
                  width = 1 / 2,
                  selectInput(
                    "route_type",
                    "Transit Service Type",
                    choices = AdjustmentFactorsAndTripLengths$route_type,
                    selected = AdjustmentFactorsAndTripLengths$route_type[1]
                  ),
                  selectInput(
                    "fuel_type",
                    "Fuel Type",
                    choices = c("Electric", "Diesel"),
                    selected = "Electric"
                  ),
                  selectInput(
                    "transit_expansion_location",
                    "Location",
                    choices = unique(CommunityType$CTU_NAME),
                    selected = CommunityType$CTU_NAME[2]
                  ),
                  shinyWidgets::autonumericInput(
                    "ridership_increase",
                    "Ridership Increase",
                    #  Default from sample project: MetroTransit Micro G Line Exp
                    value = 32976,
                    decimalPlaces = 0,
                    align = 'left',
                    emptyInputBehavior = "zero"
                  ),
                  selectInput(
                    "transit_expansion_project_start",
                    "Year",
                    choices = seq(2024, 2050, 1),
                    selected = 2024
                  ),
                  numericInput(
                    "transit_expansion_project_lifetime",
                    "Project Lifetime (in years)",
                    value = 14
                  ),
                  shinyWidgets::autonumericInput(
                    "added_transit",
                    "Increase in Annual Transit VMT (Mile)",
                    #  Default from sample project: MetroTransit Micro G Line Exp
                    value = 1566,
                    decimalPlaces = 0,
                    align = 'left'
                  ),
                  numericInput(
                    "average_trip_length",
                    "Average Auto Trip Replaced (Mile)",
                    value = AdjustmentFactorsAndTripLengths$adjustment_factor[1]
                  ),
                  numericInput(
                    "transit_expansion_adjustment_factor",
                    "Transit Dependency Adjustment Factor",
                    value = AdjustmentFactorsAndTripLengths$adjustment_factor[1]
                  ),
                  tags$div(class = "info-box", textOutput("selected_community_type"), ),
                )
              ), card(uiOutput("transit_expansion_ui"))
            )),
            nav_panel(title = "Mobility Hubs", page_fillable(
              card(
                layout_column_wrap(
                  width = 1 / 3,
                  selectizeInput(
                    "mobility_mode",
                    "Mobility Mode/s",
                    choices = TotalVMTReductionPotential$mobility_mode,
                    selected = TotalVMTReductionPotential$mobility_mode[1],
                    multiple = TRUE,
                    options = list(plugins = list("remove_button"))
                  ),
                  selectInput(
                    "hub_location",
                    "Location",
                    choices = unique(CommunityType$CTU_NAME),
                    selected = CommunityType$CTU_NAME[2]
                  ),
                  shinyWidgets::autonumericInput(
                    "population_3mile",
                    "Population within the service area",
                    #  Default=rural area
                    value = 14137,
                    decimalPlaces = 0,
                    align = 'left',
                    emptyInputBehavior = "zero"
                  ),
                  selectInput(
                    "hub_project_start",
                    "Year",
                    choices = seq(2024, 2050, 1),
                    selected = 2024
                  ),
                  numericInput("hub_project_lifetime", "Project Lifetime (in years)", value = 20),
                  numericInput("added_vmt", "Increase in Annual Transit VMT (Mile)", value = 0),
                  numericInput(
                    "reduction_potential",
                    "Total VMT Reduction Potential",
                    value = 0.058,
                    min = 0,
                    max = 1,
                    step = 0.01
                  ),
                  shinyWidgets::autonumericInput(
                    "annual_vmt",
                    "Annual VMT per capita",
                    value = 10655,
                    decimalPlaces = 0,
                    align = 'left',
                    emptyInputBehavior = "zero"
                  ),
                  tags$div(class = "info-box", textOutput("selected_community_type_mobilityHub"))
                )
              ), card(uiOutput("mobility_hub_ui"))
            ))
          )),
          
          
          
          nav_panel(title = "Travel Demand Management", navset_card_tab(
            nav_panel(title = "Employee Commute", page_fillable(
              card(
                layout_column_wrap(
                  width = 1 / 2,
                  selectInput(
                    "location",
                    "Location",
                    choices = unique(CommunityType$CTU_NAME),
                    selected = CommunityType$CTU_NAME[2]
                  ),
                  shinyWidgets::autonumericInput(
                    "daily_commute_no",
                    "Number of Daily One-Way Commute Trips Reduced",
                    value = 1000 ,
                    decimalPlaces = 0,
                    align = 'left',
                    emptyInputBehavior = "zero"
                  ),
                  selectInput(
                    "project_start",
                    "Year",
                    choices = seq(2024, 2050, 1),
                    selected = 2024
                  ),
                  numericInput("project_lifetime", "Project Lifetime (in years)", value = 4)
                  ,
                  numericInput(
                    "average_commute",
                    "Average One-way Commute Trip Distance (Mile)",
                    value = 10.9
                  ),
                  numericInput("working_days", "Annual Number of Working Days", value = 260),
                  tags$div(
                    class = "info-box",
                    textOutput("selected_community_type_employeeCommute")
                  )
                )
              ), card(uiOutput("employee_commute_ui"))
            )),
            nav_panel(title = "Shared Mobility", page_fillable(
              card(
                layout_column_wrap(
                  width = 1 / 2,
                  selectInput(
                    "fleet",
                    "Mobility Type",
                    choices = c("Bike", "Scooter", "Non-EV Rideshares", "EV Rideshares"),
                    selected = "Bike"
                  ),
                  selectInput(
                    "shared_mobility_location",
                    "Shared Mobility Location",
                    choices = unique(CommunityType$CTU_NAME),
                    selected = CommunityType$CTU_NAME[2]
                  ),
                  shinyWidgets::autonumericInput(
                    "no_trips",
                    "Number of Annual Trips per Vehicle/Equipment",
                    value = 1000,
                    decimalPlaces = 0,
                    align = 'left',
                    emptyInputBehavior = "zero"
                  ),
                  selectInput(
                    "shared_mobility_project_start",
                    "Year",
                    choices = seq(2024, 2050, 1),
                    selected = 2024
                  ),
                  numericInput(
                    "shared_mobility_project_lifetime",
                    "Project Lifetime (in years)",
                    value = 8
                  ),
                  numericInput(
                    "no_vehicles",
                    "Number of Daily Vehicle or Equipment Dispatched",
                    value = 1
                  ),
                  numericInput(
                    "trip_miles",
                    "Average Length of Vehicle Trip Displaced (Mile)",
                    value = 20
                  ),
                  numericInput(
                    "shared_mobility_adjustment_factor",
                    "Vehicle Dependency Adjustment Factor",
                    value = 1
                  ),
                  numericInput("average_occupancy", "Average Occupancy per Vehicle", value = 20),
                  numericInput("prct_deadhead_miles", "Percent of Deadhead Miles", value = .83),
                  tags$div(
                    class = "info-box",
                    textOutput("selected_community_type_sharedMobility")
                  )
                  
                )
              ), card(uiOutput("shared_mobility_ui"))
            ))
          )),
          nav_panel(title = "Bicycle and Pedestrian Facilities", navset_card_tab(
            nav_panel(title = "Pedestrian Facilities", page_fillable(
              card(
                layout_column_wrap(
                  width = 1 / 2,
                  selectInput(
                    "pedestrian_location",
                    "Location",
                    choices = unique(CommunityType$CTU_NAME),
                    selected = CommunityType$CTU_NAME[2]
                  ),
                  selectInput(
                    "pedestrian_project_start",
                    "Year",
                    choices = seq(2024, 2050, 1),
                    selected = 2024
                  ),
                  numericInput(
                    "pedestrian_project_lifetime",
                    "Project Lifetime (in years)",
                    value = 20
                  ),
                  shinyWidgets::autonumericInput(
                    "average_daily_traffic",
                    "Average Annual Daily traffic (two way) on road parallel or adjacent to facility",
                    value = 6000,
                    decimalPlaces = 0,
                    align = 'left',
                    emptyInputBehavior = "zero"
                  ),
                  numericInput("one_way_facility_length", "Facility Length", value = 1),
                  numericInput(
                    "no_key_destinations_25",
                    "Number of Key Destinations within 0.25 mile",
                    value = 1
                  ),
                  numericInput(
                    "no_key_destinations_50",
                    "Number of Key Destinations within 0.5 mile",
                    value = 1
                  ),
                  numericInput("annual_use_days", "Facility Annual Days of Use", value = 214),
                  numericInput(
                    "average_trip_replaced",
                    "Average Length of Auto Trip Replaced (Mile)",
                    value = .86
                  ),
                  tags$div(
                    class = "info-box",
                    textOutput("selected_community_type_pedestrianFacility")
                  ),
                  tags$div(class = "info-box", textOutput("mode_shift_factor_pedestrianFacility"), ),
                  tags$div(
                    class = "info-box",
                    textOutput("credit_key_destinations_pedestrianFacility")
                  )
                )
              ), card(uiOutput("pedestrian_facility_ui"))
            )),
            nav_panel(title = "Multi-Use Trails and Bicycle Facilities", page_fillable(
              card(
                layout_column_wrap(
                  width = 1 / 2,
                  selectInput(
                    "trails_bike_location",
                    "Location",
                    choices = unique(CommunityType$CTU_NAME),
                    selected = CommunityType$CTU_NAME[2]
                  ),
                  selectInput(
                    "trails_bike_facility_type",
                    "Facility Type",
                    choices = c("On Street", "New Multiuse", "Conversion"),
                    selected = "On Street"
                  ),
                  selectInput(
                    "trails_bike_project_start",
                    "Year",
                    choices = seq(2024, 2050, 1),
                    selected = 2024
                  ),
                  numericInput(
                    "trails_bike_project_lifetime",
                    "Project Lifetime (in years)",
                    value = 20
                  ),
                  shinyWidgets::autonumericInput(
                    "trails_bike_average_daily_traffic",
                    "Average Annual Daily traffic (two way) on road parallel or adjacent to facility",
                    value = 6000,
                    decimalPlaces = 0,
                    align = 'left',
                    emptyInputBehavior = "zero"
                  ),
                  numericInput(
                    "trails_bike_facility_length_range",
                    "Facility Length",
                    value = 1
                  ),
                  numericInput(
                    "trails_bike_no_key_destinations_25",
                    "Number of Key Destinations within 0.25 mile",
                    value = 1
                  ),
                  numericInput(
                    "trails_bike_no_key_destinations_50",
                    "Number of Key Destinations within 0.5 mile",
                    value = 1
                  ),
                  numericInput("trails_bike_days_open", "Facility Annual Days of Use", value = 214),
                  numericInput(
                    "length_trip_replaced_biking",
                    "Average Length of Auto Trip Replaced (Mile)",
                    value = 3.6
                  ),
                  tags$div(class = "info-box", textOutput("selected_community_type_trailsBikes")),
                  tags$div(class = "info-box", textOutput("mode_shift_factor_trailsBike")),
                  tags$div(class = "info-box", textOutput("credit_key_destinations_trailsBike"))
                )
              ), card(uiOutput("trails_bike_ui"))
            ))
          )),
          nav_panel(title = "Roadways", navset_card_tab(
            nav_panel(title = "Intersection Delay Reductions", page_fillable(
              card(
                layout_column_wrap(
                  width = 1 / 2,
                  selectInput(
                    "intersection_delay_location",
                    "Location",
                    choices = unique(CommunityType$CTU_NAME),
                    selected = CommunityType$CTU_NAME[2]
                  ),
                  numericInput("number_peak_hours", "Number of Peak Hours", value = 1),
                  selectInput(
                    "intersection_delay_project_start",
                    "Year",
                    choices = seq(2024, 2050, 1),
                    selected = 2024
                  ),
                  numericInput(
                    "intersection_delay_project_lifetime",
                    "Project Lifetime (in years)",
                    value = 20
                  ),
                  numericInput("vehicle_per_hour", "Intersection Vehicle Per Hour", value = 1),
                  numericInput(
                    "peak_hour_delay_noBuild",
                    "Peak Hour Delay Per Vehicle under No-Build Condition (Vehicle Hours)",
                    value = 2
                  ),
                  numericInput(
                    "peak_hour_delay_build",
                    "Peak Hour Delay Per Vehicle under Build Condition (Vehicle Hours)",
                    value = 1
                  ),
                  tags$div(
                    class = "info-box",
                    textOutput("selected_community_type_intersectionDelay")
                  )
                )
              ), card(uiOutput("intersection_delay_ui"))
            )),
            nav_panel(title = "Corridor Speed Improvements", page_fillable(
              card(
                layout_column_wrap(
                  width = 1 / 2,
                  selectInput(
                    "corridor_speed_location",
                    "Location",
                    choices = unique(CommunityType$CTU_NAME),
                    selected = CommunityType$CTU_NAME[2]
                  ),
                  numericInput("corridor_distance", "Corridor Distance (Mile)", value = 1),
                  selectInput(
                    "corridor_speed_project_start",
                    "Year",
                    choices = seq(2024, 2050, 1),
                    selected = 2024
                  ),
                  numericInput(
                    "corridor_speed_project_lifetime",
                    "Project Lifetime (in years)",
                    value = 20
                  ),
                  numericInput(
                    "avg_annual_daily_traffic",
                    "Annual average daily traffic under the no-build condition",
                    value = 1
                  ),
                  numericInput(
                    "avg_corridor_speed_no_build",
                    "Average Corridor Speed under the No-Build Condition (mph)",
                    value = 1
                  ),
                  numericInput(
                    "avg_corridor_speed_build",
                    "Average Corridor Speed under the Build Condition (mph)",
                    value = 1.2
                  ),
                  tags$div(
                    class = "info-box",
                    textOutput("selected_community_type_corridorSpeed"),
                  ),
                  tags$div(class = "info-box", textOutput("induced_demand_elasticity"))
                )
              ), card(uiOutput("corridor_speed_ui"))
            ))
          )),
          widths = c(2, 10)
        )
      ),
      
      nav_panel(
        title = tags$span("Methodology and Sources", style = "color: #002b5c; font-weight: bold; font-size: 18px"),
        
        # Custom header placed above the navlistPanel
        div(
          style = "padding: 10px; border-bottom: 2px solid #ddd; margin-bottom: 10px; background-color: transparent;",
          
          # Title text
          tags$span(
            "Project Emissions Quantification Methodologies and Sources",
            style = "color: #002b5c; font-weight: bold; font-size: 18px;"
          ),
          
          # Line break
          tags$br(),
          
          # Informational paragraph with larger font
          tags$p(
            "If you would like to learn more about the methodology behind each project type, please read the Regional Solicitation Recommended Methodology for Estimating GHG Impacts.",
            style = "font-size: 16px; color: #333; margin-top: 5px;"
          )
        ),
        
        tabsetPanel(
          # Tab 1: Methodologies (contains sub-tabs for different project types)
          tabPanel(
            "Methodologies",
            navlistPanel(
              tabPanel(
                "EV Outreach",
                
                tags$br(), 
                
                p("Auto VMT Displaced (annual) = ACCL or H × N × R"),
                tags$ul(
                  tags$li(
                    strong("ACCL or H:"),
                    " Average annual accrual rate of a typical LD or HD vehicle."
                  ),
                  tags$li(strong("N:"), " Number of participants."),
                  tags$li(
                    strong("R:"),
                    " Conversion rate of participants – Default rate used in app is 4%."
                  )
                ),
                p(
                  "GHG Emissions Impacts (annual) = Auto VMT Displaced × (CIL or H – CIE)"
                ),
                tags$ul(
                  tags$li(
                    strong("CIL or H:"),
                    " Carbon intensity (CI) that accounts for LD (gasoline) vehicle WTW emissions."
                  ),
                  tags$li(strong("CIE:"), " Carbon intensity of electricity grid")
                )
              ),
              
              tabPanel(
                "Public Infrastructure",
                tags$br(), 
                
                p(
                  HTML(
                    "Auto VMT Displaced (annual) = &Sigma; N<sub>i</sub> P<sub>i</sub> U<sub>i</sub> H<sub>i</sub> &divide; EVEF &times; FICE"
                  )
                ),
                tags$ul(
                  tags$li(strong("N:"), "Number of chargers of a certain power level"),
                  tags$li(strong("P:"), "Charger power level (e.g., 50 kW, 150 kW)"),
                  tags$li(
                    strong("U:"),
                    "Average charger utilization rate dictated by charger type"
                  ),
                  tags$li(
                    strong("H:"),
                    "Annual total hours that the chargers are online and available for charging"
                  ),
                  tags$li(
                    strong("EVEF:"),
                    "Average energy efficiency values (in Wh per mile)"
                  ),
                  tags$li(strong("FICE:"), "Current fraction of ICE vehicles in the fleet")
                ),
                p(
                  "GHG Emissions Impacts (annual) = Auto VMT Displaced × (CIL or H – CIE)"
                ),
                tags$ul(
                  tags$li(
                    strong("CIL or H:"),
                    " Carbon intensity (CI) that accounts for LD vehicle WTW emissions."
                  ),
                  tags$li(
                    strong("CIE:"),
                    " Carbon intensity of electricity grid; default values can be retrieved from the GREET model."
                  )
                )
              ),
              
              tabPanel("Transit Expansion", p("Content for Methodology 3")),
              tabPanel("Mobility Hubs", p("Content for Methodology 4")),
              tabPanel("Employee Commute", p("Content for Methodology 5")),
              tabPanel("Shared Mobility", p("Content for Methodology 6")),
              tabPanel("Pedestrian Facilities", p("Content for Methodology 7")),
              tabPanel(
                "Multi-Use Trails and Bicycle Facilities",
                p("Content for Methodology 8")
              ),
              tabPanel("Intersection Delay", p("Content for Methodology 9")),
              tabPanel("Corridor Speed", p("Content for Methodology 10"))
            )
          ),
          
          # Tab 2: Sources
          tabPanel("Sources", DTOutput("data_sources_table"))
        )
      )
    )
  )
)
