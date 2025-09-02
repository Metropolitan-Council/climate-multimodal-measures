#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

function(input, output, session) {
  ####################### Employee Commute #############################################################################
  employee_commute_results <- reactive({
    if (is.null(input$project_start)) {
      return()
    }
    employee_commute(
      daily_commute_no = input$daily_commute_no,
      project_start = input$project_start,
      project_lifetime = input$project_lifetime,
      # default of 4 years
      # community_type = input$community_type, #decided to remove and assign based on location, or the map once we get it up and running
      location = input$location,
      working_days = input$working_days,
      # default should be 260 days
      average_commute = input$average_commute 
    )
  })
  
  output$employee_commute_ui <- renderUI({
    tagList(
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
        h4("Employee Commute Results"),
        downloadButton("download_employee_commute", "Download CSV")
      ),
      dataTableOutput("employee_commute_table") # Table renders below the button
    )
  })
  output$employee_commute_table <- renderDataTable({
    if (is.null(input$project_start)) {
      return()
    }
    
    # Render table with HTML enabled, and disable sorting
    DT::datatable(
      employee_commute_results(),
      escape = FALSE, # Enables rendering HTML
      rownames = FALSE,
      options = list(
        dom = "tip", # ✅ Enable pagination, search bar, and info
        scrollX = TRUE, # ✅ Allows horizontal scrolling
        ordering = FALSE, # ✅ Disable sorting buttons on headers
        pageLength = 10, # ✅ Show 10 rows per page by default
        lengthMenu = c(5, 10, 25, 50, 100) # ✅ Allow users to select number of rows
      )
    ) %>%
      formatStyle(
        "Year",
        target = "row",
        fontWeight = styleEqual("Total", "bold")
      )
  })
  
  
  output$download_employee_commute <- downloadHandler(
    filename = function() {
      paste("Employee_Commute_Data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(employee_commute_results(), file, row.names = FALSE)
    }
  )
  
  ####################### EV Outreach #############################################################################
  # observeEvent(input$audience, {
  #   req(input$audience)
  #   updateNumericInput(session,
  #                      "ev_outreach_project_lifetime",
  #                      value = if (input$audience == "Heavy Duty")
  #                        14 else 8)
  # })
  # EV Outreach Reduction Calculation
  
  ev_outreach_results <- reactive({
    if (is.null(input$no_participants) |
        is.null(input$ev_outreach_project_start) |
        is.null(input$ev_outreach_project_lifetime) |
        is.null(input$conversion_rate)) {
      # is.null(input$audience)) {
      return()
    }
    ev_outreach(
      no_participants = input$no_participants,
      project_start = input$ev_outreach_project_start,
      project_lifetime = input$ev_outreach_project_lifetime, # 8 years default for light duty 14 years for heavy duty
      conversion_rate = input$conversion_rate, # default .04
      # audience = input$audience, #LD or HD
      location = input$ev_outreach_location
    )
  })
  
  output$ev_outreach_ui <- renderUI({
    tagList(
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
        h4("EV Outreach Results"),
        downloadButton("download_ev_outreach", "Download CSV")
      ),
      dataTableOutput("ev_outreach_table") # Table renders below the button
    )
  })
  
  output$ev_outreach_table <- renderDataTable({
    if (is.null(input$ev_outreach_project_start)) {
      return()
    }
    
    # Render table with HTML enabled, and disable sorting
    DT::datatable(
      ev_outreach_results(),
      escape = FALSE, # Enables rendering HTML
      rownames = FALSE,
      options = list(
        dom = "tip", # ✅ Enable pagination, search bar, and info
        scrollX = TRUE, # ✅ Allows horizontal scrolling
        ordering = FALSE, # ✅ Disable sorting buttons on headers
        pageLength = 10, # ✅ Show 10 rows per page by default
        lengthMenu = c(5, 10, 25, 50, 100) # ✅ Allow users to select number of rows
      )
    ) %>%
      formatStyle(
        "Year",
        target = "row",
        fontWeight = styleEqual("Total", "bold")
      )
  })
  
  
  output$download_ev_outreach <- downloadHandler(
    filename = function() {
      paste("EV_Outreach_Data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(ev_outreach_results(), file, row.names = FALSE)
    }
  )
  
  ############################ EV Infrastructure ##############################################################
  
  # Reactive expression for ev_infrastructure calculations
  ev_infrastructure_results <- reactive({
    # Ensure project start date is provided
    req(input$ev_infrastructure_project_start)
    
    ev_infrastructure(
      # ev_type = input$ev_type,
      no_chargers = input$no_chargers,
      charger_type = input$charger_type,
      charge_power = input$charge_power,
      annual_hours_available = input$annual_hours_available,
      location = input$ev_infrastructure_location,
      project_start = input$ev_infrastructure_project_start,
      project_lifetime = input$ev_infrastructure_project_lifetime,
      utilization_rate = input$utilization_rate,
      average_energy_efficiency = input$average_energy_efficiency,
      percentage_ICE = input$percentage_ICE
    )
  })
  
  output$ev_infrastructure_ui <- renderUI({
    tagList(
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
        h4("Public Infrastructure Results"),
        downloadButton("download_ev_infrastructure", "Download CSV")
      ),
      dataTableOutput("ev_infrastructure_table") # Table renders below the button
    )
  })
  
  # Render DataTable for ev_infrastructure results
  output$ev_infrastructure_table <- renderDataTable({
    # Ensure project start date is provided
    req(input$ev_infrastructure_project_start)
    
    # Get the results from the reactive expression
    results <- ev_infrastructure_results()
    
    # Check if results are NULL or empty
    if (is.null(results) || nrow(results) == 0) {
      return(DT::datatable(
        data.frame(Message = "No data available to display."),
        escape = FALSE,
        options = list(dom = "t", ordering = FALSE)
      ))
    }
    
    DT::datatable(
      ev_infrastructure_results(),
      escape = FALSE, # Enables rendering HTML
      rownames = FALSE,
      options = list(
        dom = "tip", # ✅ Enable pagination, search bar, and info
        scrollX = TRUE, # ✅ Allows horizontal scrolling
        ordering = FALSE, # ✅ Disable sorting buttons on headers
        pageLength = 10, # ✅ Show 10 rows per page by default
        lengthMenu = c(5, 10, 25, 50, 100) # ✅ Allow users to select number of rows
      )
    ) %>%
      formatStyle(
        "Year",
        target = "row",
        fontWeight = styleEqual("Total", "bold")
      )
  })
  
  output$download_ev_infrastructure <- downloadHandler(
    filename = function() {
      paste("EV_infrastructure_Data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(ev_infrastructure_results(), file, row.names = FALSE)
    }
  )
  ########################## Shared Mobility ################################################
  
  # Shared Mobility
  shared_mobility_results <- reactive({
    if (is.null(input$project_start)) {
      return()
    }
    shared_mobility(
      fleet = input$fleet,
      # Options are scooter or bicycle, non-ev fleet, and ev fleet
      no_vehicles = input$no_vehicles,
      no_trips = input$no_trips,
      project_lifetime = input$shared_mobility_project_lifetime,
      # 8 default
      project_start = input$shared_mobility_project_start,
      location = input$shared_mobility_location,
      adjustment_factor = input$adjustment_factor,
      # default is based on fleet type (.5 for bikes and scooters, .83 for rideshares)
      average_occupancy = input$average_occupancy,
      # default is 1 for bikes and scooters and 1.55 for rideshares
      trip_miles = input$trip_miles,
      # based on fleet assignment and can be found in the TripDistances dataset - bicylce, micromobility for scooters, and Smartphone ridehailing service for rideshares
      prct_deadhead_miles = input$prct_deadhead_miles # default zero for bike and scooter and .4 for rideshares
    )
  })
  
  output$shared_mobility_ui <- renderUI({
    tagList(
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
        h4("Shared Mobility Results"),
        downloadButton("download_shared_mobility", "Download CSV")
      ),
      dataTableOutput("shared_mobility_table") # Table renders below the button
    )
  })
  
  # Render DataTable for ev_infrastructure results
  output$shared_mobility_table <- renderDataTable({
    # Ensure project start date is provided
    req(input$shared_mobility_project_start)
    
    # Get the results from the reactive expression
    results <- shared_mobility_results()
    
    # Check if results are NULL or empty
    if (is.null(results) || nrow(results) == 0) {
      return(DT::datatable(
        data.frame(Message = "No data available to display."),
        escape = FALSE,
        options = list(dom = "t", ordering = FALSE)
      ))
    }
    
    output$shared_mobility_ui <- renderUI({
      tagList(
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
          h4("Shared Mobility Results"),
          downloadButton("download_shared_mobility", "Download CSV")
        ),
        dataTableOutput("shared_mobility_table") # Table renders below the button
      )
    })
    
    DT::datatable(
      shared_mobility_results(),
      escape = FALSE, # Enables rendering HTML
      rownames = FALSE,
      options = list(
        dom = "tip", # ✅ Enable pagination, search bar, and info
        scrollX = TRUE, # ✅ Allows horizontal scrolling
        ordering = FALSE, # ✅ Disable sorting buttons on headers
        pageLength = 10, # ✅ Show 10 rows per page by default
        lengthMenu = c(5, 10, 25, 50, 100) # ✅ Allow users to select number of rows
      )
    ) %>%
      formatStyle(
        "Year",
        target = "row",
        fontWeight = styleEqual("Total", "bold")
      )
  })
  output$download_shared_mobility <- downloadHandler(
    filename = function() {
      paste("Shared_Mobility_Data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(shared_mobility_results(), file, row.names = FALSE)
    }
  )
  #################### Transit Expansion###############################################
  # EV Outreach Reduction Calculation
  transit_expansion_results <- reactive({
    if (is.null(input$project_start)) {
      return()
    }
    transit_expansion(
      ridership_increase = input$ridership_increase,
      fuel_type = input$fuel_type,
      route_type = input$route_type,
      # options in AdjustmentFactorsAndTripLengths
      added_transit = input$added_transit_vmt,
      location = input$transit_expansion_location,
      project_start = input$transit_expansion_project_start,
      project_lifetime = input$transit_expansion_project_lifetime,
      # 20 year default
      average_trip_length = input$average_trip_length,
      # default is based on the route type chosen and maps to AdjustmentFactorsAndTripLengths
      adjustment_factor = input$adjustment_factor # default is based on the route type chosen and maps to AdjustmentFactorsAndTripLengths
    )
  })
  
  output$transit_expansion_ui <- renderUI({
    tagList(
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
        h4("Transit Expansion Results"),
        downloadButton("download_transit_expansion", "Download CSV")
      ),
      dataTableOutput("transit_expansion_table") # Table renders below the button
    )
  })
  
  # Render DataTable for transit_expansion results
  output$transit_expansion_table <- renderDataTable({
    # Ensure project start date is provided
    req(input$transit_expansion_project_start)
    
    # Get the results from the reactive expression
    results <- transit_expansion_results()
    
    # Check if results are NULL or empty
    if (is.null(results) || nrow(results) == 0) {
      return(DT::datatable(
        data.frame(Message = "No data available to display."),
        escape = FALSE,
        options = list(dom = "t", ordering = FALSE)
      ))
    }
    
    DT::datatable(
      transit_expansion_results(),
      escape = FALSE, # Enables rendering HTML
      rownames = FALSE,
      options = list(
        dom = "tip", # ✅ Enable pagination, search bar, and info
        scrollX = TRUE, # ✅ Allows horizontal scrolling
        ordering = FALSE, # ✅ Disable sorting buttons on headers
        pageLength = 10, # ✅ Show 10 rows per page by default
        lengthMenu = c(5, 10, 25, 50, 100) # ✅ Allow users to select number of rows
      )
    ) %>%
      formatStyle(
        "Year",
        target = "row",
        fontWeight = styleEqual("Total", "bold")
      )
  })
  output$download_transit_expansion <- downloadHandler(
    filename = function() {
      paste("Transit_Expansion_Data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(transit_expansion_results(), file, row.names = FALSE)
    }
  )
  
  observeEvent(input$route_type, {
    selected_factor <- AdjustmentFactorsAndTripLengths$adjustment_factor[AdjustmentFactorsAndTripLengths$route_type == input$route_type]
    
    selected_length <- AdjustmentFactorsAndTripLengths$average_trip_length_mi_trip[AdjustmentFactorsAndTripLengths$route_type == input$route_type]
    
    updateNumericInput(session, "transit_expansion_adjustment_factor", value = selected_factor)
    
    updateNumericInput(session, "average_trip_length", value = selected_length)
  })
  ################################ Corridor Speed Improvement######################################################################
  
  # Corridor Speed Improvement Results
  corridor_speed_improvement_results <- reactive({
    if (is.null(input$project_start)) {
      return()
    }
    corridor_speed_improvements(
      corridor_distance = input$corridor_distance,
      avg_annual_daily_traffic = input$avg_annual_daily_traffic,
      avg_corridor_speed_no_build = input$avg_corridor_speed_no_build,
      avg_corridor_speed_build = input$avg_corridor_speed_build,
      location = input$corridor_speed_location,
      project_start = input$corridor_speed_project_start,
      project_lifetime = input$corridor_speed_project_lifetime 
    )
  })
  
  output$corridor_speed_ui <- renderUI({
    tagList(
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
        h4("Corridor Speed Increases Results"),
        downloadButton("download_corridor_speed", "Download CSV")
      ),
      dataTableOutput("corridor_speed_table") # Table renders below the button
    )
  })
  
  # Render DataTable for transit_expansion results
  output$corridor_speed_table <- renderDataTable({
    # Ensure project start date is provided
    req(input$corridor_speed_project_start)
    
    # Get the results from the reactive expression
    results <- corridor_speed_improvement_results()
    
    # Check if results are NULL or empty
    if (is.null(results) || nrow(results) == 0) {
      return(DT::datatable(
        data.frame(Message = "No data available to display."),
        escape = FALSE,
        options = list(dom = "t", ordering = FALSE)
      ))
    }
    
    DT::datatable(
      corridor_speed_improvement_results(),
      escape = FALSE, # Enables rendering HTML
      rownames = FALSE,
      options = list(
        dom = "tip", # ✅ Enable pagination, search bar, and info
        scrollX = TRUE, # ✅ Allows horizontal scrolling
        ordering = FALSE, # ✅ Disable sorting buttons on headers
        pageLength = 10, # ✅ Show 10 rows per page by default
        lengthMenu = c(5, 10, 25, 50, 100) # ✅ Allow users to select number of rows
      )
    ) %>%
      formatStyle(
        "Year",
        target = "row",
        fontWeight = styleEqual("Total", "bold")
      )
  })
  output$download_corridor_speed <- downloadHandler(
    filename = function() {
      paste("Corridor_Speed_Data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(corridor_speed_improvement_results(), file, row.names = FALSE)
    }
  )
  
  ######################################## Intersection Delay###########################################
  
  # Intersection Delay Results
  intersection_delay_results <- reactive({
    if (is.null(input$project_start)) {
      return()
    }
    intersection_delay_reductions(
      number_peak_hours = input$number_peak_hours,
      vehicle_per_hour = input$vehicle_per_hour,
      peak_hour_delay_noBuild = input$peak_hour_delay_noBuild,
      peak_hour_delay_build = input$peak_hour_delay_build,
      location = input$intersection_delay_location,
      project_start = input$intersection_delay_project_start,
      project_lifetime = input$intersection_delay_project_lifetime 
    )
  })
  
  output$intersection_delay_ui <- renderUI({
    tagList(
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
        h4("Intersection Delay Results"),
        downloadButton("download_intersection_delay", "Download CSV")
      ),
      dataTableOutput("intersection_delay_table") # Table renders below the button
    )
  })
  
  # Render DataTable for transit_expansion results
  output$intersection_delay_table <- renderDataTable({
    # Ensure project start date is provided
    req(input$intersection_delay_project_start)
    
    # Get the results from the reactive expression
    results <- intersection_delay_results()
    
    # Check if results are NULL or empty
    if (is.null(results) || nrow(results) == 0) {
      return(DT::datatable(
        data.frame(Message = "No data available to display."),
        escape = FALSE,
        options = list(dom = "t", ordering = FALSE)
      ))
    }
    
    DT::datatable(
      intersection_delay_results(),
      escape = FALSE, # Enables rendering HTML
      rownames = FALSE,
      options = list(
        dom = "tip", # ✅ Enable pagination, search bar, and info
        scrollX = TRUE, # ✅ Allows horizontal scrolling
        ordering = FALSE, # ✅ Disable sorting buttons on headers
        pageLength = 10, # ✅ Show 10 rows per page by default
        lengthMenu = c(5, 10, 25, 50, 100) # ✅ Allow users to select number of rows
      )
    ) %>%
      formatStyle(
        "Year",
        target = "row",
        fontWeight = styleEqual("Total", "bold")
      )
  })
  output$download_intersection_delay <- downloadHandler(
    filename = function() {
      paste("Intersection_Delay_Data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(intersection_delay_results(), file, row.names = FALSE)
    }
  )
  
  ########################## Mobility Hub######################################################
  # Mobility Hub Results
  mobility_hub_results <- reactive({
    if (is.null(input$project_start)) {
      return()
    }
    mobility_hubs(
      mobility_mode = input$mobility_mode,
      # Allow for multiple selections options are in TotalVMTReductionPotential DF
      added_vmt = input$added_vmt,
      project_lifetime = input$hub_project_lifetime,
      # Default is 20 years
      project_start = input$hub_project_start,
      location = input$hub_location,
      population_3mile = input$population_3mile,
      # Auto populate with 3 mile population based on map selection
      reduction_potential = input$reduction_potential,
      # Auto calculate based on mobility modes chosen (add all total vmt redcution from the TotalVMTReductionPotential DF)
      annual_vmt = input$annual_vmt # Auto populate with VMT per capita based on community type of chosen location
    )
  })
  
  output$mobility_hub_ui <- renderUI({
    tagList(
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
        h4("Mobility Hub Results"),
        downloadButton("download_mobility_hub", "Download CSV")
      ),
      dataTableOutput("mobility_hub_table") # Table renders below the button
    )
  })
  
  # Render DataTable for transit_expansion results
  output$mobility_hub_table <- renderDataTable({
    # Ensure project start date is provided
    req(input$hub_project_start)
    
    # Get the results from the reactive expression
    results <- mobility_hub_results()
    
    # Check if results are NULL or empty
    if (is.null(results) || nrow(results) == 0) {
      return(DT::datatable(
        data.frame(Message = "No data available to display."),
        escape = FALSE,
        options = list(dom = "t", ordering = FALSE)
      ))
    }
    
    DT::datatable(
      mobility_hub_results(),
      escape = FALSE, # Enables rendering HTML
      rownames = FALSE,
      options = list(
        dom = "tip", # ✅ Enable pagination, search bar, and info
        scrollX = TRUE, # ✅ Allows horizontal scrolling
        ordering = FALSE, # ✅ Disable sorting buttons on headers
        pageLength = 10, # ✅ Show 10 rows per page by default
        lengthMenu = c(5, 10, 25, 50, 100) # ✅ Allow users to select number of rows
      )
    ) %>%
      formatStyle(
        "Year",
        target = "row",
        fontWeight = styleEqual("Total", "bold")
      )
  })
  output$download_mobility_hub <- downloadHandler(
    filename = function() {
      paste("Mobility_Hub_Data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(mobility_hub_results(), file, row.names = FALSE)
    }
  )
  
  ################################### Pedestrian Facilites##################################################
  # Pedestrian Facilities Results
  pedestrian_facilities_results <- reactive({
    if (is.null(input$project_start)) {
      return()
    }
    pedestrian_facilities(
      average_daily_traffic = input$average_daily_traffic,
      one_way_facility_length = input$one_way_facility_length,
      no_key_destinations_25 = input$no_key_destinations_25,
      no_key_destinations_50 = input$no_key_destinations_50,
      location = input$pedestrian_location,
      project_start = input$pedestrian_project_start,
      project_lifetime = input$pedestrian_project_lifetime,
      annual_use_days = input$annual_use_days,
      # Default is 214
      average_trip_replaced = input$average_trip_replaced # Default based on community type distinction
    )
  })
  
  output$pedestrian_facility_ui <- renderUI({
    tagList(
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
        h4("Pedestrian Facility Result"),
        downloadButton("download_mobility_hub", "Download CSV")
      ),
      dataTableOutput("pedestrian_facility_table") # Table renders below the button
    )
  })
  
  # Render DataTable for transit_expansion results
  output$pedestrian_facility_table <- renderDataTable({
    # Ensure project start date is provided
    req(input$pedestrian_project_start)
    
    # Get the results from the reactive expression
    results <- pedestrian_facilities_results()
    
    # Check if results are NULL or empty
    if (is.null(results) || nrow(results) == 0) {
      return(DT::datatable(
        data.frame(Message = "No data available to display."),
        escape = FALSE,
        options = list(dom = "t", ordering = FALSE)
      ))
    }
    
    DT::datatable(
      pedestrian_facilities_results(),
      escape = FALSE, # Enables rendering HTML
      rownames = FALSE,
      options = list(
        dom = "tip", # ✅ Enable pagination, search bar, and info
        scrollX = TRUE, # ✅ Allows horizontal scrolling
        ordering = FALSE, # ✅ Disable sorting buttons on headers
        pageLength = 10, # ✅ Show 10 rows per page by default
        lengthMenu = c(5, 10, 25, 50, 100) # ✅ Allow users to select number of rows
      )
    ) %>%
      formatStyle(
        "Year",
        target = "row",
        fontWeight = styleEqual("Total", "bold")
      )
  })
  output$download_pedestrian_facility <- downloadHandler(
    filename = function() {
      paste("Pedestrian_Facility_Data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(pedestrian_facility_results(), file, row.names = FALSE)
    }
  )
  
  ############################## Multi-use Trails###########################################################
  
  # Multi-Use Trails and Bicycle Facilities Results
  trails_bike_facilities_results <- reactive({
    if (is.null(input$project_start)) {
      return()
    }
    trails_bike_facilities(
      average_daily_traffic = input$trails_bike_average_daily_traffic,
      facility_length_range = input$trails_bike_facility_length_range,
      no_key_destinations_25 = input$trails_bike_no_key_destinations_25,
      no_key_destinations_50 = input$trails_bike_no_key_destinations_50,
      location = input$trails_bike_location,
      facility_type = input$trails_bike_facility_type,
      # options are "on_street", "new_multiuse", or "conversion"
      project_start = input$trails_bike_project_start,
      project_lifetime = input$trails_bike_project_lifetime,
      days_open = input$trails_bike_days_open,
      # Default is 214
      length_trip_replaced_biking = input$length_trip_replaced_biking # Default is 3.6
    )
  })
  
  output$trails_bike_ui <- renderUI({
    tagList(
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
        h4("Multi-use Trails and Bicycle Facilities Result"),
        downloadButton("download_trails_bike", "Download CSV")
      ),
      dataTableOutput("trails_bike_table") # Table renders below the button
    )
  })
  
  # Render DataTable for transit_expansion results
  output$trails_bike_table <- renderDataTable({
    # Ensure project start date is provided
    req(input$trails_bike_project_start)
    
    # Get the results from the reactive expression
    results <- trails_bike_facilities_results()
    
    # Check if results are NULL or empty
    if (is.null(results) || nrow(results) == 0) {
      return(DT::datatable(
        data.frame(Message = "No data available to display."),
        escape = FALSE,
        options = list(dom = "t", ordering = FALSE)
      ))
    }
    
    DT::datatable(
      trails_bike_facilities_results(),
      escape = FALSE, # Enables rendering HTML
      rownames = FALSE,
      options = list(
        dom = "tip", # ✅ Enable pagination, search bar, and info
        scrollX = TRUE, # ✅ Allows horizontal scrolling
        ordering = FALSE, # ✅ Disable sorting buttons on headers
        pageLength = 10, # ✅ Show 10 rows per page by default
        lengthMenu = c(5, 10, 25, 50, 100) # ✅ Allow users to select number of rows
      )
    ) %>%
      formatStyle(
        "Year",
        target = "row",
        fontWeight = styleEqual("Total", "bold")
      )
  })
  output$download_trails_bike <- downloadHandler(
    filename = function() {
      paste("Multi-Use_Trails_Data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(trails_bike_facilities_results(), file, row.names = FALSE)
    }
  )
  
  ################################ Map ######################################################################
  
  foundational.map <- shiny::reactive({
    leaflet() %>%
      addTiles(urlTemplate = "https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png") %>%
      # Add population layer
      addPolygons(
        data = population,
        fillOpacity = 0,
        opacity = 0,
        color = "#000000",
        weight = 2,
        layerId = population$GEOID
      ) %>%
      # Add mpo_area layer
      addPolygons(
        data = mpo_area,
        fillColor = "#DDEBF8",
        fillOpacity = 0,
        color = "#002b5c",
        weight = 3.5,
        layerId = "mpo_area",
        label = ~ paste("Other"),
        labelOptions = labelOptions(
          style = list("color" = "black"),
          textsize = "12px",
          direction = "auto"
        )
      ) %>%
      # Add locations layer
      addPolygons(
        data = locations,
        fillColor = "#5CABFF",
        fillOpacity = 0.3,
        color = "#002b5c",
        weight = .5,
        layerId = locations$CTU_NAME,
        label = ~CTU_NAME,
        labelOptions = labelOptions(
          style = list("color" = "black"),
          textsize = "12px",
          direction = "auto"
        )
      ) %>%
      # Fit bounds to the mpo_area extent
      fitBounds(
        lng1 = -94.01256,
        lat1 = 44.47124,
        lng2 = -92.73191,
        lat2 = 45.41455
      ) %>%
      # Add the reset view button
      addEasyButton(
        easyButton(
          icon = "fa-home",
          title = "Reset Map View",
          position = "topright",
          onClick = JS("function(btn, map) {
                        Shiny.setInputValue('reset_map', Math.random());  // Send reset signal to R
                      }")
        )
      ) %>%
      # Add the information icon
      addControl(
        html = as.character(
          tags$i(
            class = "fas fa-question-circle",
            style = "font-size: 18px; cursor: pointer;",
            `data-toggle` = "popover",
            `data-placement` = "bottom",
            title = "Emission factors are determined based on the community type of your proposed project’s location.

            If your project involves a facility or hub, select its placement where it will be built, as the calculation accounts for the population within its surrounding radius."
          )
        ),
        position = "topleft"
      ) %>%
      # Ensure popovers work
      htmlwidgets::onRender("
      function(el, x) {
        $(el).find('[data-toggle=\"popover\"]').popover({ trigger: 'hover', html: true });
      }
    ")
  })
  
  
  output$myMap <- renderLeaflet({
    foundational.map()
  })
  
  # Observe the reset signal and re-render the map
  observeEvent(input$reset_map, {
    output$myMap <- renderLeaflet({
      foundational.map() # This will reset the map
    })
  })
  
  
  shiny::observeEvent(input$myMap_shape_click, {
    click <- input$myMap_shape_click
    
    if (is.null(click$id)) {
      req(click$id)
    } else {
      print(click)
      # Create an sf point from the click coordinates
      clicked_point <- st_sfc(st_point(c(click$lng, click$lat)), crs = 4326) # Create point geometry in WGS84 (EPSG:4326)
      
      # Now, transform the point to a projected CRS like EPSG:3857 for accurate buffering in meters
      clicked_point_projected <- st_transform(clicked_point, crs = 3857)
      
      # Create a buffer (circle) around the point with the given radius in meters
      buffer_circle <- st_buffer(clicked_point_projected, dist = 1609) # Buffer in meters (1 miles)
      
      # Transform back to WGS84 for visualization/intersection (if needed)
      buffer_circle_wgs84 <- st_transform(buffer_circle, crs = 4326)
      
      # Perform the intersection with your spatial dataset (population)
      intersections <- st_intersection(
        st_transform(population, crs = 4326),
        buffer_circle_wgs84
      )
      
      intersection_calcs <- intersections %>%
        left_join(
          population %>%
            filter(GEOID %in% intersections$GEOID) %>%
            mutate(total_tract_area = st_area(geometry)) %>%
            select(GEOID, total_tract_area) %>%
            st_drop_geometry(),
          by = "GEOID"
        ) %>%
        mutate(
          area_in_circle = st_area(.),
          area_share = area_in_circle / total_tract_area,
          estimated_pop = estimate * area_share
        )
      
      hold_population <- isolate(round(sum(intersection_calcs$estimated_pop)))
      updateNumericInput(session, "population_3mile", value = as.numeric(hold_population))
      
      # output$tract_info <- renderText(paste(click$lng, click$lat, hold_population))
      leaflet::leafletProxy(mapId = "myMap") %>%
        clearGroup(group = "circle") %>%
        addCircles(
          lng = click$lng,
          lat = click$lat,
          radius = 1609, # 1 mile
          group = "circle"
        )
    }
  })
  
  map_selected_location <- reactiveVal()
  # First observeEvent to capture the map click and store the CTU_NAME
  observeEvent(input$myMap_shape_click, {
    click <- input$myMap_shape_click
    
    if (!is.null(click)) {
      clicked_CTU_NAME <- click$id
      map_selected_location(clicked_CTU_NAME)
      # print(paste("Clicked CTU_NAME:", clicked_CTU_NAME))
    }
  })
  
  observe({
    CTU_NAME <- map_selected_location()
    
    # Check if CTU_NAME is NULL or "mpo_area" and replace it with "Other"
    if (is.null(CTU_NAME) || CTU_NAME == "mpo_area") {
      CTU_NAME <- "Other"
      community_type <- "Rural / Non-Council"
    }
    
    output$map_tab_label <- renderText(paste0("Map (selected ", CTU_NAME, ")"))
    
    updateSelectInput(session, "location", selected = CTU_NAME)
    updateSelectInput(session, "ev_infrastructure_location", selected = CTU_NAME)
    updateSelectInput(session, "shared_mobility_location", selected = CTU_NAME)
    updateSelectInput(session, "transit_expansion_location", selected = CTU_NAME)
    updateSelectInput(session, "hub_location", selected = CTU_NAME)
    updateSelectInput(session, "pedestrian_location", selected = CTU_NAME)
    updateSelectInput(session, "ev_outreach_location", selected = CTU_NAME)
    updateSelectInput(session, "intersection_delay_location", selected = CTU_NAME)
    updateSelectInput(session, "corridor_speed_location", selected = CTU_NAME)
    
    # If you have a text output to show the community type
    output$community_type_label <- renderText(paste("Community Type:", community_type))
  })
  
  observeEvent(input$fleet, {
    updateNumericInput(
      session,
      "shared_mobility_project_lifetime",
      value = ifelse(input$fleet %in% c("Bike", "Scooter"), 5, 8)
    )
  })
  ###################### Sources#########################################################
  
  output$data_sources_table <- renderDT({
    datatable(data.frame(
      Source = c(
        "Imagine 2050 Community Designations",
        "U.S. Census",
        "Met Council Scenario Planning Tool ",
        "GREET 2023",
        "Metro Transit",
        "CARB",
        "Met Council Transit Experience and Satisfaction Survey",
        "Mobility Hub Planning and Implementation Guidebook",
        "CARB 2018 Clean Miles Standard",
        "CARB 2018 Clean Miles Standard ",
        "EV WATTS Charging Station Dashboard Q4-23",
        "Barr, Lawrence C. Testing for the significance of induced highway travel demand in metropolitan areas"
      ),
      Description = c(
        "Community Designations Data",
        "Population Data",
        "Vehicle Stock Data, VMT Data, Direct GHG Emissions from Transportation",
        "Electricity Emissions",
        "Average Auto Trip Replaced",
        "Transit Dependency Adjustment Factors",
        "Transit Dependency Adjustment Factors",
        "Total VMT Reduction Potential",
        "Percentage of deadhead miles",
        "Average Occupancy per Vehicle",
        "EV Charge Utilization Rates",
        "Elasticity of induced VMT due to improved corridor speed"
      )
    ))
  })
  ################################# Reactions/Events#########################################
  
  observe({
    req(input$location)
    
    default_commute <- 10.9
    
    community_type <- CommunityType %>%
      filter(CTU_NAME == input$location) %>%
      pull(MappedCommunity) %>%
      na.omit() %>%
      unique()
    
    if (length(community_type) != 1) {
      avg_commute <- default_commute
    } else {
      commute_row <- VMTByCommunityType %>% filter(CD == community_type)
      
      avg_commute <- if (nrow(commute_row) > 0) {
        commute_row$vmt / 2
      } else {
        default_commute
      }
    }
    
    updateNumericInput(session, "average_commute", value = round(avg_commute, 1))
  })
  
  
  observeEvent(input$transit_expansion_location, {
    # Get the selected community type
    community_type <- CommunityTypeShape %>%
      filter(CTU_NAME == input$transit_expansion_location) %>%
      pull(MappedCommunity)
    
    # Update the text output for the community type
    output$selected_community_type <- renderText({
      paste("Selected Community Type:", community_type)
    })
  })
  
  observeEvent(input$shared_mobility_location, {
    # Get the selected community type
    community_type <- CommunityTypeShape %>%
      filter(CTU_NAME == input$shared_mobility_location) %>%
      pull(MappedCommunity)
    
    # Update the text output for the community type
    output$selected_community_type_sharedMobility <- renderText({
      paste("Selected Community Type:", community_type)
    })
  })
  
  
  observeEvent(
    {
      input$trails_bike_location
      input$trails_bike_average_daily_traffic
      input$trails_bike_one_way_facility_length
      input$trails_bike_no_key_destinations_25
      input$trails_bike_no_key_destinations_50
    },
    {
      # Get the selected community type
      community_type <- CommunityTypeShape %>%
        filter(CTU_NAME == input$trails_bike_location) %>%
        pull(MappedCommunity)
      
      # Update the text output for the community type
      output$selected_community_type_trailsBikes <- renderText({
        paste("Selected Community Type:", community_type)
      })
      
      # Determine traffic and facility length ranges for mode shift factor calculation
      average_daily_traffic <- input$trails_bike_average_daily_traffic
      one_way_facility_length <- input$trails_bike_facility_length_range
      
      # Use case_when for traffic range
      traffic_range <- case_when(
        average_daily_traffic <= 12000 ~ "1 to 12,000",
        average_daily_traffic <= 24000 ~ "12,001 to 24,000",
        average_daily_traffic <= 30000 ~ "24,001 to 30,000",
        TRUE ~ NA_character_
      )
      
      # Use case_when for facility length range
      facility_length_range <- case_when(
        one_way_facility_length == 1 ~ "1",
        one_way_facility_length > 1 &
          one_way_facility_length <= 2 ~ "1.01",
        one_way_facility_length > 2 ~ "2",
        TRUE ~ NA_character_
      )
      
      # Calculate mode shift factor
      mode_shift_factor <- ModeShiftFactor %>%
        filter(
          average_daily_traffic_vehicle_trips_per_day == traffic_range,
          one_way_facility_length_miles_low == facility_length_range
        ) %>%
        pull(mode_shift_factor_m)
      
      destination_category_50 <- case_when(
        input$trails_bike_no_key_destinations_50 <= 2 ~ "0 to 2",
        input$trails_bike_no_key_destinations_50 == 3 ~ "3",
        input$trails_bike_no_key_destinations_50 >= 4 & input$trails_bike_no_key_destinations_50 <= 6 ~ "4 to 6",
        input$trails_bike_no_key_destinations_50 >= 7 ~ "7 or more"
      )
      
      # Determine the category for the number of key destinations
      destination_category_25 <- case_when(
        input$trails_bike_no_key_destinations_25 <= 2 ~ "0 to 2",
        input$trails_bike_no_key_destinations_25 == 3 ~ "3",
        input$trails_bike_no_key_destinations_25 >= 4 & input$trails_bike_no_key_destinations_25 <= 6 ~ "4 to 6",
        input$trails_bike_no_key_destinations_25 >= 7 ~ "7 or more"
      )
      
      # Filter the CreditForKeyDestinations dataframe to get the relevant rows
      credit_25 <- CreditForKeyDestinations %>%
        filter(number_of_key_destinations == destination_category_25) %>%
        pull(credit_within_1_4_mile_of_facility_c)
      
      credit_50 <- CreditForKeyDestinations %>%
        filter(number_of_key_destinations == destination_category_50) %>%
        pull(credit_within_1_2_mile_of_facility_c)
      
      # Compare and assign the larger credit value
      key_destination_credit <- max(credit_25, credit_50, na.rm = TRUE)
      
      # Update the text output for mode shift factor
      output$mode_shift_factor_trailsBike <- renderText({
        paste("Mode Shift Factor:", ifelse(length(mode_shift_factor) > 0, mode_shift_factor, "Not Found"))
      })
      
      # Update the text output for key destination credit
      output$credit_key_destinations_trailsBike <- renderText({
        paste("Key Destination Credit:", ifelse(length(key_destination_credit) > 0, key_destination_credit, "Not Found"))
      })
    }
  )
  
  
  observeEvent(input$ev_outreach_location, {
    # Get the selected community type
    community_type <- CommunityTypeShape %>%
      filter(CTU_NAME == input$ev_outreach_location) %>%
      pull(MappedCommunity)
    
    average_annual_accrual <- round(PerVehicleVMT %>% filter(MappedCommunity == community_type) %>% pull(PerVehicleVMT), 0)
    
    # Update the text output for the community type
    output$selected_community_type_EVOutreach <- renderText({
      paste("Selected Community Type:", community_type)
    })
    
    # Update the text output for the community type
    output$average_annual_accrual <- renderText({
      paste("Average Annual Accrual:", average_annual_accrual)
    })
  })
  
  observeEvent(list(input$hub_location, input$mobility_mode), {
    # Get the selected community type
    community_type <- CommunityTypeShape %>%
      filter(CTU_NAME == input$hub_location) %>%
      pull(MappedCommunity)
    
    # Calculate the reduction potential based on all selected mobility modes
    reduction_potential <- TotalVMTReductionPotential %>%
      filter(mobility_mode %in% input$mobility_mode) %>%
      summarise(total_vmt_reduction_potential = sum(total_vmt_reduction_potential, na.rm = TRUE)) %>%
      pull(total_vmt_reduction_potential)
    
    # Update the numeric input directly with the new value
    updateNumericInput(session,
                       "reduction_potential",
                       value = reduction_potential
    )
    output$selected_community_type_mobilityHub <- renderText({
      paste("Selected Community Type:", community_type)
    })
  })
  
  
  
  observeEvent(
    {
      input$pedestrian_location
      input$average_daily_traffic
      input$one_way_facility_length
      input$no_key_destinations_25
      input$no_key_destinations_50
    },
    {
      # Get the selected community type
      community_type <- CommunityTypeShape %>%
        filter(CTU_NAME == input$pedestrian_location) %>%
        pull(MappedCommunity)
      
      # Update the text output for the community type
      output$selected_community_type_pedestrianFacility <- renderText({
        paste("Selected Community Type:", community_type)
      })
      
      # Determine traffic and facility length ranges for mode shift factor calculation
      average_daily_traffic <- input$average_daily_traffic
      one_way_facility_length <- input$one_way_facility_length
      
      # Use case_when for traffic range
      traffic_range <- case_when(
        average_daily_traffic <= 12000 ~ "1 to 12,000",
        average_daily_traffic <= 24000 ~ "12,001 to 24,000",
        average_daily_traffic <= 30000 ~ "24,001 to 30,000",
        TRUE ~ NA_character_
      )
      
      # Use case_when for facility length range
      facility_length_range <- case_when(
        one_way_facility_length == 1 ~ "1",
        one_way_facility_length > 1 &
          one_way_facility_length <= 2 ~ "1.01",
        one_way_facility_length > 2 ~ "2",
        TRUE ~ NA_character_
      )
      
      # Calculate mode shift factor
      mode_shift_factor <- ModeShiftFactor %>%
        filter(
          average_daily_traffic_vehicle_trips_per_day == traffic_range,
          one_way_facility_length_miles_low == facility_length_range
        ) %>%
        pull(mode_shift_factor_m)
      
      # Determine the number of key destinations
      if (input$no_key_destinations_25 > input$no_key_destinations_50) {
        no_key_destinations <- input$no_key_destinations_25
      } else {
        no_key_destinations <- input$no_key_destinations_50
      }
      
      # Calculate key destination credit
      key_destination_credit <- CreditForKeyDestinations %>%
        filter(
          case_when(
            no_key_destinations <= 2 ~ number_of_key_destinations == "0 to 2",
            no_key_destinations == 3 ~ number_of_key_destinations == "3",
            no_key_destinations >= 4 & no_key_destinations <= 6 ~ number_of_key_destinations == "4 to 6",
            no_key_destinations >= 7 ~ number_of_key_destinations == "7 or more"
          )
        ) %>%
        pull(credit_within_1_2_mile_of_facility_c)
      
      # Update the text output for mode shift factor
      output$mode_shift_factor_pedestrianFacility <- renderText({
        paste("Mode Shift Factor:", ifelse(length(mode_shift_factor) > 0, mode_shift_factor, "Not Found"))
      })
      
      # Update the text output for key destination credit
      output$credit_key_destinations_pedestrianFacility <- renderText({
        paste("Key Destination Credit:", ifelse(length(key_destination_credit) > 0, key_destination_credit, "Not Found"))
      })
    }
  )
  
  observeEvent(input$location, {
    # Get the selected community type
    community_type <- CommunityTypeShape %>%
      filter(CTU_NAME == input$location) %>%
      pull(MappedCommunity)
    
    # Update the text output for the community type
    output$selected_community_type_employeeCommute <- renderText({
      paste("Selected Community Type:", community_type)
    })
  })
  
  observeEvent(input$intersection_delay_location, {
    # Get the selected community type
    community_type <- CommunityTypeShape %>%
      filter(CTU_NAME == input$intersection_delay_location) %>%
      pull(MappedCommunity)
    
    # Update the text output for the community type
    output$selected_community_type_intersectionDelay <- renderText({
      paste("Selected Community Type:", community_type)
    })
  })
  
  observeEvent(
    {
      input$corridor_speed_location
      input$avg_corridor_speed_no_build
      input$avg_corridor_speed_build
    },
    {
      # Get the selected community type
      community_type <- CommunityTypeShape %>%
        filter(CTU_NAME == input$corridor_speed_location) %>%
        pull(MappedCommunity)
      
      # Update the text output for the community type
      output$selected_community_type_corridorSpeed <- renderText({
        paste("Selected Community Type:", community_type)
      })
      
      # Calculate k1 values for build and no-build scenarios
      # TODO Make sure these match with items listed in MetCouncilTables.xlsx
      # TODO document what each value means, 
      k1_speed_build <- 0.000019137 * input$avg_corridor_speed_build^2 - 0.0020660 * input$avg_corridor_speed_build + 0.088916
      k1_speed_no_build <- 0.000019137 * input$avg_corridor_speed_no_build^2 - 0.0020660 * input$avg_corridor_speed_no_build + 0.088916
      
      # Calculate speed improvement percentage
      speed_improvement_prct <- ((input$avg_corridor_speed_build - input$avg_corridor_speed_no_build) / input$avg_corridor_speed_no_build)
      
      # Determine induced demand elasticity based on speed improvement percentage
      # TODO Make sure these match with items listed in MetCouncilTables.xlsx
      if (speed_improvement_prct <= .05) {
        induced_demand_elasticity <- 0
      } else if (speed_improvement_prct > .05 & speed_improvement_prct <= .2) {
        induced_demand_elasticity <- 2 * speed_improvement_prct - 0.1
      } else if (speed_improvement_prct > .20) {
        induced_demand_elasticity <- 0.3
      }
      
      # Update the text output for induced demand elasticity
      output$induced_demand_elasticity <- renderText({
        paste("Induced Demand Elasticity:", round(induced_demand_elasticity, 2))
      })
    }
  )
  
  observeEvent(input$fleet, {
    if (input$fleet == "Scooter") {
      updateNumericInput(session, "average_occupancy", value = 1)
      updateNumericInput(session, "shared_mobility_adjustment_factor", value = 0.5)
      updateNumericInput(session, "prct_deadhead_miles", value = 0)
      updateNumericInput(session, "trip_miles", value = 0.71)
    } else if (input$fleet == "Bike") {
      updateNumericInput(session, "average_occupancy", value = 1)
      updateNumericInput(session, "shared_mobility_adjustment_factor", value = 0.5)
      updateNumericInput(session, "prct_deadhead_miles", value = 0)
      updateNumericInput(session, "trip_miles", value = 2.97)
    } else {
      updateNumericInput(session, "average_occupancy", value = 1.5)
      updateNumericInput(session, "shared_mobility_adjustment_factor", value = 0.83)
      updateNumericInput(session, "prct_deadhead_miles", value = 0.4)
      updateNumericInput(session, "trip_miles", value = 5.87)
    }
  })
  
  
  observe({
    req(input$ev_infrastructure_location, input$ev_infrastructure_project_start, input$charger_type)
    
    # Get the selected community type
    community_type <- CommunityTypeShape %>%
      filter(CTU_NAME == input$ev_infrastructure_location) %>%
      pull(MappedCommunity)
    
    # Update the text output for the community type
    output$selected_community_type_EVInfrastructure <- renderText({
      paste("Selected Community Type:", community_type)
    })
    
    # # Ensure current_year is defined based on the selected project start year
    current_year <- as.numeric(input$ev_infrastructure_project_start)
    
    # Filter FleetData based on the selected community type
    filtered_FleetData <- FleetData %>%
      filter(MappedCommunity == community_type) %>%
      mutate(year = as.numeric(year))
    
    # Determine the closest year
    closest_year <- filtered_FleetData %>%
      summarise(closest_year = year[which.min(abs(year - current_year))]) %>%
      pull(closest_year)
    
    # Filter the dataset to get the fleet proportions from the closest year
    fleet_proportion <- filtered_FleetData %>%
      filter(year == closest_year)
    
    # Calculate percentage_ICE and round to 2 decimals
    percentage_ICE <- round(fleet_proportion$electricity, 2) * 100
    
    # Update the numeric input for percentage_ICE
    updateNumericInput(session, "percentage_ICE", value = percentage_ICE)
    
    # Dynamically update average_energy_efficiency
    average_energy_efficiency <- FuelEfficiency %>%
      filter(`Vehicle Type` == "Light-Duty") %>%
      pull(`Fuel Efficiency (Wh/mi)`)
    
    # Convert Wh/mi to kWh/mi by dividing by 1000
    average_energy_efficiency <- round(average_energy_efficiency / 1000, 2)
    
    # Update the numeric input for average_energy_efficiency
    updateNumericInput(session, "average_energy_efficiency", value = average_energy_efficiency)
    
    # Dynamically update charge_power based on the selected charger_type
    charge_power <- if (input$charger_type == "DCFC") {
      150 # Default power for DCFC
    } else {
      19.2 # Default power for Level 2 chargers
    }
    
    # Update the numeric input for charge_power
    updateNumericInput(session, "charge_power", value = charge_power)
  })
  
  observeEvent(input$charger_type, {
    updateNumericInput(session, "utilization_rate",
                       value = ifelse(input$charger_type == "DCFC", 4, 15)
    )
  })
}
