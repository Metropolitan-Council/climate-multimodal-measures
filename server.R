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
      project_lifetime = input$project_lifetime, #default of 4 years
      # community_type = input$community_type, #decided to remove and assign based on location, or the map once we get it up and running 
      location = input$location
      # working_days = input$working_days, #default should be 260 days
      # average_commute = input$average_commute #default should be based on the mapping the location and what that maps to in CommunityTypeShape
    )
  })
  
  output$employee_commute_table <- renderDataTable({
    if (is.null(input$project_start)) {
      return ()
    }
    met_council_datatable(
      employee_commute_results()
    )
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
      project_lifetime = input$ev_outreach_project_lifetime, #8 years default for light duty 14 years for heavy duty
      conversion_rate = input$conversion_rate, #default .04
      audience = input$audience #LD or HD
    )
  })
  
  output$ev_outreach_table <- renderDataTable({
    if (is.null(input$ev_outreach_project_start)) {
      return ()
    }
    met_council_datatable(
      ev_outreach_results()
    )
  })
  
  
  # EV Outreach Reduction Calculation
  ev_infrastructure_results <- reactive({
    if (is.null(input$ev_infrastructure_project_start)) {
      return ()
    }
    ev_infrastructure(
      ev_type = input$ev_type, #LD or HD
      no_chargers = input$no_chargers,
      charger_type = input$charger_type, #newly added - options are level 2 chargers of DCFC chargers
      charge_power = input$charge_power, #19.2kwh for level 2 and 150 kwh for DCDC chargers 
      annual_hours_available = input$annual_hours_available,
      location = input$ev_infrastructure_location, #all locations can be extracted from CommunityTypeShape
      project_start = input$ev_infrastructure_project_start,
      project_lifetime = input$ev_infrastructure_project_lifetime #10 year default
      #ChargerUtilizationRates = input#ChargerUtilizationRates #default should be based on the charger_type in the ChargerUtilizationRates dataset
    )
  })
  
  output$ev_infrastructure_table <- renderDataTable({
    if (is.null(input$ev_infrastructure_project_start)) {
      return ()
    }
    met_council_datatable(
      ev_infrastructure_results()
    )
  })
  
  
  # Shared Mobility
  # double checking this name?
  shared_mobility_results <- reactive({
    if (is.null(input$project_start)) {
      return ()
    }
    shared_mobility(
      fleet = input$fleet, #Options are scooter or bicycle, non-ev fleet, and ev fleet
      no_vehicles = input$no_vehicles,
      no_trips = input$no_trips,
      project_lifetime = input$shared_mobility_project_lifetime, # 8 default
      project_start = input$shared_mobility_project_start,
      location = input$shared_mobility_location
      # adjustment_factor = input$adjustment_factor, #default is based on fleet type (.5 for bikes and scooters, .83 for rideshares)
      # average_occupancy = input$average_occupancy, #default is 1 for bikes and scooters and 1.55 for rideshares
      # trip_miles = input$trip_miles, # based on fleet assignment and can be found in the TripDistances dataset - bicylce, micromobility for scooters, and Smartphone ridehailing service for rideshares
      # prct_deadhead_miles = input$prct_deadhead_miles #default zero for bike and scooter and .4 for rideshares
    )
  })
  
  output$shared_mobility_table <- renderDataTable({
    if (is.null(input$project_start)) {
      return ()
    }
    met_council_datatable(
      shared_mobility_results()
    )
  })
  
  # EV Outreach Reduction Calculation
  transit_expansion_results <- reactive({
    if (is.null(input$project_start)) {
      return ()
    }
    transit_expansion(
      ridership_increase = input$ridership_increase, #no default in Task 4 Memo
      route_type = input$route_type, #options in AdjustmentFactorsAndTripLengths
      added_transit = input$added_transit, #no default in Task 4 Memo CHANGE UI NAME TO ADDED TRANSIT VMT
      location = input$transit_expansion_location, 
      project_start = input$transit_expansion_project_start,
      project_lifetime = input$transit_expansion_project_lifetime, #20 year default
      average_trip_length = input$average_trip_length, #default is based on the route type chosen and maps to AdjustmentFactorsAndTripLengths
      adjustment_factor = input$adjustment_factor #default is based on the route type chosen and maps to AdjustmentFactorsAndTripLengths
      # ADD TEXT TO UI TO EXPLAIN ADJUSTMENT FACTOR
    )
  })
  
  output$transit_expansion_table <- renderDataTable({
    if (is.null(input$project_start)) {
      return ()
    }
    met_council_datatable(
      transit_expansion_results()
    )
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
      location = input$location, 
      project_start = input$project_start,
      project_lifetime = input$project_lifetime, #Default is 7 I think if this corresponds to traffic management technologies
      fleet_ratio = input$fleet_ratio #Default is based on the fleet ratio of community type
    )
  })
  
  output$corridor_speed_improvements_table <- renderDataTable({
    if (is.null(input$project_start)) {
      return ()
    }
    met_council_datatable(
      corridor_speed_improvement_results()
    )
  })
  
  # Intersection Delay Results
  intersection_delay_results <- reactive({
    if (is.null(input$project_start)) {
      return ()
    }
    intersection_delay_reductions(
      number_peak_hours = input$number_peak_hours,
      vehicle_per_hour = input$vehicle_per_hour,
      location = input$location, 
      project_start = input$project_start,
      project_lifetime = input$project_lifetime, #Default is 7 I think if this corresponds to traffic management technologies
      fleet_ratio = input$fleet_ratio #Default is based on the fleet ratio of community type
    )
  })
  
  output$intersection_delay_reductions_table <- renderDataTable({
    if (is.null(input$project_start)) {
      return ()
    }
    met_council_datatable(
      intersection_delay_results()
    )
  })
  
  # Mobility Hub Results
  mobility_hub_results <- reactive({
    if (is.null(input$project_start)) {
      return ()
    }
    mobility_hub(
      mobility_mode = input$mobility_mode, #Allow for multiple selections options are in TotalVMTReductionPotential DF
      added_vmt = input$added_vmt,
      project_lifetime = input$project_lifetime, #Default is 20 years
      project_start = input$project_start,
      location = input$location,
      population_3mile = input$population_3mile, #Auto populate with 3 mile population based on map selection
      reduction_potential = input$reduction_potential, #Auto calculate based on mobility modes chosen (add all total vmt redcution from the TotalVMTReductionPotential DF)
      annual_vmt = input$annual_vmt #Auto populate with VMT per capita based on community type of chosen location
    )
  })
  
  output$mobility_hub_table <- renderDataTable({
    if (is.null(input$project_start)) {
      return ()
    }
    met_council_datatable(
      mobility_hub_results()
    )
  })
  
  # Pedestrian Facilities Results
  pedestrian_facilities_results <- reactive({
    if (is.null(input$project_start)) {
      return ()
    }
    pedestrian_facilities(
      average_daily_traffic,
      one_way_facility_length,
      no_key_destinations_25,
      no_key_destinations_50,
      location,
      project_start,
      project_lifetime,
      annual_use_days = input$annual_use_days, # Default is 214
      average_trip_replaced = input$average_trip_replaced # Default based on community type distinction
    )
  })
  
  output$pedestrian_facilities_table <- renderDataTable({
    if (is.null(input$project_start)) {
      return ()
    }
    met_council_datatable(
      pedestrian_facilities_results()
    )
  })
  
  foundational.map <- shiny::reactive({
    leaflet() %>%
      addTiles( urlTemplate = "https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png") %>%
      addPolygons( data = population
                   , fillOpacity = 0
                   # , opacity = 0.2
                   , opacity = 0
                   , color = "#000000"
                   , weight = 2
                   , layerId = population$GEOID
      ) %>%
      addPolygons(
        data = locations,
        fillColor = "#0062cc",  
        fillOpacity = 0.3,  
        color = "black",       
        weight = 1,          
        layerId = locations$CTU_NAME,  
        label = ~CTU_NAME,  
        labelOptions = labelOptions(
          style = list("color" = "black"),
          textsize = "12px",
          direction = "auto"
        )
      ) %>%
      fitBounds(
        lng1 = -94.01256, lat1 = 44.47124,
        lng2 = -92.73191, lat2 = 45.41455
      )
  })
  
  output$myMap <- renderLeaflet({
    
    foundational.map()
    
  }) 
  
  shiny::observeEvent( input$myMap_shape_click, {
    
    click <- input$myMap_shape_click
    
    if( is.null( click$id ) ){
      req( click$id )
      
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
        left_join(population %>%
                    filter(GEOID %in% intersections$GEOID) %>%
                    mutate(total_tract_area = st_area(geometry)) %>%
                    select(GEOID, total_tract_area) %>% 
                    st_drop_geometry(),
                  by = "GEOID") %>%
        mutate(area_in_circle = st_area(.),
               area_share = area_in_circle / total_tract_area,
               estimated_pop = estimate * area_share)
      
      output$tract_info <- renderText(paste(click$lng, click$lat, sum(intersection_calcs$estimated_pop)))
      leaflet::leafletProxy( mapId = "myMap" ) %>%
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
    } else {
      output$map_tab_label <- renderText("Map")
    }
  })
  
}
