#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

function(input, output, session) {
  # Employee Commute Reduction Calculation
  employee_commute_results <- reactive({
    if (is.null(input$project_start)) {
      return ()
    }
    employee_commute(
      daily_commute_no = input$daily_commute_no,
      project_start = input$project_start,
      project_lifetime = input$project_lifetime,
      #default of 4 years
      # community_type = input$community_type, #decided to remove and assign based on location, or the map once we get it up and running
      location = input$location,
      working_days = input$working_days,
      #default should be 260 days
      average_commute = input$average_commute #default should be based on the mapping the location and what that maps to in CommunityTypeShape
    )
  })
  
  output$employee_commute_table <- renderDataTable({
    if (is.null(input$project_start)) {
      return ()
    }
    met_council_datatable(employee_commute_results())
  })
  
  # EV Outreach Reduction Calculation
  ev_outreach_results <- reactive({
    if (is.null(input$no_participants) |
        is.null(input$ev_outreach_project_start) |
        is.null(input$ev_outreach_project_lifetime) |
        is.null(input$conversion_rate) |
        is.null(input$audience)) {
      return ()
    }
    ev_outreach(
      no_participants = input$no_participants,
      project_start = input$ev_outreach_project_start,
      project_lifetime = input$ev_outreach_project_lifetime,
      #8 years default for light duty 14 years for heavy duty
      conversion_rate = input$conversion_rate,
      #default .04
      audience = input$audience,
      #LD or HD
      location = input$ev_outreach_location
    )
  })
  
  output$ev_outreach_table <- renderDataTable({
    if (is.null(input$ev_outreach_project_start)) {
      return()
    }
    
    # Render table with HTML enabled, and disable sorting
    DT::datatable(
      ev_outreach_results(),
      escape = FALSE,  # Enables rendering HTML
      options = list(
        dom = 't',      # Table layout without search box
        scrollX = TRUE, # Allows horizontal scrolling
        ordering = FALSE # Disable sorting buttons on headers
      )
    )
  })
  
  # EV Outreach Reduction Calculation
  ev_infrastructure_results <- reactive({
    if (is.null(input$ev_infrastructure_project_start)) {
      return ()
    }
    ev_infrastructure(
      ev_type = input$ev_type,
      #LD or HD
      no_chargers = input$no_chargers,
      charger_type = input$charger_type,
      #newly added - options are level 2 chargers of DCFC chargers
      charge_power = input$charge_power,
      #19.2kwh for level 2 and 150 kwh for DCDC chargers
      annual_hours_available = input$annual_hours_available,
      location = input$ev_infrastructure_location,
      #all locations can be extracted from CommunityTypeShape
      project_start = input$ev_infrastructure_project_start,
      project_lifetime = input$ev_infrastructure_project_lifetime,
      #10 year default
      utilization_rate = input$utilization_rate,
      #default should be based on the charger_type in the ChargerUtilizationRates dataset
      average_energy_efficiency = input$average_energy_efficiency,
      percentage_ICE = input$percentage_ICE
    )
  })
  
  output$ev_infrastructure_table <- renderDataTable({
    if (is.null(input$ev_infrastructure_project_start)) {
      return ()
    }
    # Render table with HTML enabled, and disable sorting
    DT::datatable(
      ev_infrastructure_results(),
      escape = FALSE,  # Enables rendering HTML
      options = list(
        dom = 't',      # Table layout without search box
        scrollX = TRUE, # Allows horizontal scrolling
        ordering = FALSE # Disable sorting buttons on headers
      )
    )
    # met_council_datatable(ev_infrastructure_results())
  })
  
  
  # Shared Mobility
  # double checking this name?
  shared_mobility_results <- reactive({
    if (is.null(input$project_start)) {
      return ()
    }
    shared_mobility(
      fleet = input$fleet,
      #Options are scooter or bicycle, non-ev fleet, and ev fleet
      no_vehicles = input$no_vehicles,
      no_trips = input$no_trips,
      project_lifetime = input$shared_mobility_project_lifetime,
      # 8 default
      project_start = input$shared_mobility_project_start,
      location = input$shared_mobility_location,
      adjustment_factor = input$adjustment_factor,
      #default is based on fleet type (.5 for bikes and scooters, .83 for rideshares)
      average_occupancy = input$average_occupancy,
      #default is 1 for bikes and scooters and 1.55 for rideshares
      trip_miles = input$trip_miles,
      # based on fleet assignment and can be found in the TripDistances dataset - bicylce, micromobility for scooters, and Smartphone ridehailing service for rideshares
      prct_deadhead_miles = input$prct_deadhead_miles #default zero for bike and scooter and .4 for rideshares
    )
  })
  
  output$shared_mobility_table <- renderDataTable({
    if (is.null(input$project_start)) {
      return ()
    }
    met_council_datatable(shared_mobility_results())
  })
  
  # EV Outreach Reduction Calculation
  transit_expansion_results <- reactive({
    if (is.null(input$project_start)) {
      return ()
    }
    transit_expansion(
      ridership_increase = input$ridership_increase,
      #no default in Task 4 Memo
      route_type = input$route_type,
      #options in AdjustmentFactorsAndTripLengths
      added_transit = input$added_transit,
      #no default in Task 4 Memo CHANGE UI NAME TO ADDED TRANSIT VMT
      location = input$transit_expansion_location,
      project_start = input$transit_expansion_project_start,
      project_lifetime = input$transit_expansion_project_lifetime,
      #20 year default
      average_trip_length = input$average_trip_length,
      #default is based on the route type chosen and maps to AdjustmentFactorsAndTripLengths
      adjustment_factor = input$adjustment_factor #default is based on the route type chosen and maps to AdjustmentFactorsAndTripLengths
      # ADD TEXT TO UI TO EXPLAIN ADJUSTMENT FACTOR
    )
  })
  
  output$transit_expansion_table <- renderDataTable({
    if (is.null(input$project_start)) {
      return ()
    }
    met_council_datatable(transit_expansion_results())
  })
  
  # Corridor Speed Improvement Results
  corridor_speed_improvement_results <- reactive({
    if (is.null(input$project_start)) {
      return ()
    }
    corridor_speed_improvements(
      corridor_distance = input$corridor_distance,
      avg_annual_daily_traffic = input$avg_annual_daily_traffic,
      avg_corridor_speed_no_build = input$avg_corridor_speed_no_build,
      avg_corridor_speed_build = input$avg_corridor_speed_build,
      location = input$corridor_speed_location,
      project_start = input$corridor_speed_project_start,
      project_lifetime = input$corridor_speed_project_lifetime #Default is 7 I think if this corresponds to traffic management technologies
    )
  })
  
  output$corridor_speed_improvements_table <- renderDataTable({
    if (is.null(input$project_start)) {
      return ()
    }
    met_council_datatable(corridor_speed_improvement_results())
  })
  
  # Intersection Delay Results
  intersection_delay_results <- reactive({
    if (is.null(input$project_start)) {
      return ()
    }
    intersection_delay_reductions(
      number_peak_hours = input$number_peak_hours,
      vehicle_per_hour = input$vehicle_per_hour,
      peak_hour_delay_noBuild = input$peak_hour_delay_noBuild,
      peak_hour_delay_build = input$peak_hour_delay_build,
      location = input$intersection_delay_location,
      project_start = input$intersection_delay_project_start,
      project_lifetime = input$intersection_delay_project_lifetime #Default is 7 I think if this corresponds to traffic management technologies
    )
  })
  
  output$intersection_delay_reductions_table <- renderDataTable({
    if (is.null(input$project_start)) {
      return ()
    }
    met_council_datatable(intersection_delay_results())
  })
  
  # Mobility Hub Results
  mobility_hub_results <- reactive({
    if (is.null(input$project_start)) {
      return ()
    }
    mobility_hubs(
      mobility_mode = input$mobility_mode,
      #Allow for multiple selections options are in TotalVMTReductionPotential DF
      added_vmt = input$added_vmt,
      project_lifetime = input$hub_project_lifetime,
      #Default is 20 years
      project_start = input$hub_project_start,
      location = input$hub_location,
      population_3mile = input$population_3mile,
      #Auto populate with 3 mile population based on map selection
      reduction_potential = input$reduction_potential,
      #Auto calculate based on mobility modes chosen (add all total vmt redcution from the TotalVMTReductionPotential DF)
      annual_vmt = input$annual_vmt #Auto populate with VMT per capita based on community type of chosen location
    )
  })
  
  output$mobility_hub_table <- renderDataTable({
    if (is.null(input$project_start)) {
      return ()
    }
    met_council_datatable(mobility_hub_results())
  })
  
  # Pedestrian Facilities Results
  pedestrian_facilities_results <- reactive({
    if (is.null(input$project_start)) {
      return ()
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
  
  output$pedestrian_facilities_table <- renderDataTable({
    if (is.null(input$project_start)) {
      return ()
    }
    met_council_datatable(pedestrian_facilities_results())
  })
  
  # Multi-Use Trails and Bicycle Facilities Results
  trails_bike_facilities_results <- reactive({
    if (is.null(input$project_start)) {
      return ()
    }
    trails_bike_facilities(
      average_daily_traffic = input$trails_bike_average_daily_traffic,
      facility_length_range = input$trails_bike_facility_length_range,
      no_key_destinations_25 = input$trails_bike_no_key_destinations_25,
      no_key_destinations_50 = input$trails_bike_no_key_destinations_50,
      location = input$trails_bike_location,
      facility_type = input$trails_bike_facility_type,
      #options are "on_street", "new_multiuse", or "conversion"
      project_start = input$trails_bike_project_start,
      project_lifetime = input$trails_bike_project_lifetime,
      days_open = input$trails_bike_days_open,
      # Default is 214
      length_trip_replaced_biking = input$length_trip_replaced_biking #Default is 3.6
    )
  })
  
  output$trails_bike_facilities_table <- renderDataTable({
    if (is.null(input$project_start)) {
      return ()
    }
    met_council_datatable(trails_bike_facilities_results())
  })
  
  foundational.map <- shiny::reactive({
    leaflet() %>%
      addTiles(urlTemplate = "https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png") %>%
      addPolygons(
        data = population
        ,
        fillOpacity = 0
        # , opacity = 0.2
        ,
        opacity = 0
        ,
        color = "#000000"
        ,
        weight = 2
        ,
        layerId = population$GEOID
      ) %>%
      addPolygons(
        data = locations,
        fillColor = "#0062cc",
        fillOpacity = 0.3,
        color = "black",
        weight = 1,
        layerId = locations$CTU_NAME,
        label = ~ CTU_NAME,
        labelOptions = labelOptions(
          style = list("color" = "black"),
          textsize = "12px",
          direction = "auto"
        )
      ) %>%
      fitBounds(
        lng1 = -94.01256,
        lat1 = 44.47124,
        lng2 = -92.73191,
        lat2 = 45.41455
      )
  })
  
  output$myMap <- renderLeaflet({
    foundational.map()
    
  })
  
  shiny::observeEvent(input$myMap_shape_click, {
    click <- input$myMap_shape_click
    
    if (is.null(click$id)) {
      req(click$id)
      
    } else {
      print(click)
      # Create an sf point from the click coordinates
      clicked_point <- st_sfc(st_point(c(click$lng, click$lat)), crs = 4326)  # Create point geometry in WGS84 (EPSG:4326)
      
      # Now, transform the point to a projected CRS like EPSG:3857 for accurate buffering in meters
      clicked_point_projected <- st_transform(clicked_point, crs = 3857)
      
      # Create a buffer (circle) around the point with the given radius in meters
      buffer_circle <- st_buffer(clicked_point_projected, dist = 16093)  # Buffer in meters
      
      # Transform back to WGS84 for visualization/intersection (if needed)
      buffer_circle_wgs84 <- st_transform(buffer_circle, crs = 4326)
      
      # Perform the intersection with your spatial dataset (population)
      intersections <- st_intersection(st_transform(population, crs = 4326),
                                       buffer_circle_wgs84)
      
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
          radius = 4828,
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
    
    if (!is.null(CTU_NAME)) {
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
    } else {
      output$map_tab_label <- renderText("Map")
    }
  })
  
  observeEvent(input$route_type, {
    selected_factor <- AdjustmentFactorsAndTripLengths$adjustment_factor[AdjustmentFactorsAndTripLengths$route_type == input$route_type]
    
    selected_length <- AdjustmentFactorsAndTripLengths$average_trip_length_mi_trip[AdjustmentFactorsAndTripLengths$route_type == input$route_type]
    
    updateNumericInput(session, "transit_expansion_adjustment_factor", value = selected_factor)
    
    updateNumericInput(session, "average_trip_length", value = selected_length)
  })
  
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
    
    # Ensure current_year is defined based on the selected project start year
    current_year <- as.numeric(format(input$ev_infrastructure_project_start, "%Y"))
    
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
    percentage_ICE <- round(fleet_proportion$electricity, 2)
    
    # Update the numeric input for percentage_ICE
    updateNumericInput(session, "percentage_ICE", value = percentage_ICE)
    
    # Dynamically update average_energy_efficiency based on the selected ev_type
    average_energy_efficiency <- FuelEfficiency %>%
      filter(`Vehicle Type` == input$ev_type) %>%
      pull(`Fuel Efficiency (Wh/mi)`)
    
    # Convert Wh/mi to kWh/mi by dividing by 1000
    average_energy_efficiency <- round(average_energy_efficiency / 1000, 2)
    
    # Update the numeric input for average_energy_efficiency
    updateNumericInput(session, "average_energy_efficiency", value = average_energy_efficiency)
    
    # Dynamically update charge_power based on the selected charger_type
    charge_power <- if (input$charger_type == "DCFC") {
      150  # Default power for DCFC
    } else {
      19.2  # Default power for Level 2 chargers
    }
    
    # Update the numeric input for charge_power
    updateNumericInput(session, "charge_power", value = charge_power)
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
  
  
  observeEvent({
    input$trails_bike_location
    input$trails_bike_average_daily_traffic
    input$trails_bike_one_way_facility_length
    input$trails_bike_no_key_destinations_25
    input$trails_bike_no_key_destinations_50
  }, {
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
  })
  
  
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
                       value = reduction_potential)
  })
  
  
  
  observeEvent({
    input$pedestrian_location
    input$average_daily_traffic
    input$one_way_facility_length
    input$no_key_destinations_25
    input$no_key_destinations_50
  }, {
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
  })
  
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
  
  observeEvent({
    input$corridor_speed_location
    input$avg_corridor_speed_no_build
    input$avg_corridor_speed_build
  }, {
    # Get the selected community type
    community_type <- CommunityTypeShape %>%
      filter(CTU_NAME == input$corridor_speed_location) %>%
      pull(MappedCommunity)
    
    # Update the text output for the community type
    output$selected_community_type_corridorSpeed <- renderText({
      paste("Selected Community Type:", community_type)
    })
    
    # Calculate k1 values for build and no-build scenarios
    k1_speed_build <- 0.000019137 * input$avg_corridor_speed_build^2 - 0.0020660 * input$avg_corridor_speed_build + 0.088916
    k1_speed_no_build <- 0.000019137 * input$avg_corridor_speed_no_build^2 - 0.0020660 * input$avg_corridor_speed_no_build + 0.088916
    
    # Calculate speed improvement percentage
    speed_improvement_prct <- ((input$avg_corridor_speed_build - input$avg_corridor_speed_no_build) / input$avg_corridor_speed_no_build)
    
    # Determine induced demand elasticity based on speed improvement percentage
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
  })
  
  
  
}


  
  
