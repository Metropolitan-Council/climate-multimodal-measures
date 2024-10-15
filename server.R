#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

function(input, output) {
  # output$dt <- renderDataTable({
  #   datatable(
  #     mtcars, fillContainer = TRUE
  #   )
  # })
  
  # Employee Commute Reduction Calculation
  employee_commute_results <- reactive({
    if (is.null(input$project_start)) {
      return ()
    }
    employee_commute(
      daily_commute_no = input$daily_commute_no,
      project_start = input$project_start,
      project_lifetime = input$project_lifetime,
      community_type = input$community_type, 
      location = input$location
      # ,
      # average_commute = input$average_commute
    )
  })
  
  output$employee_commute_table <- renderDataTable({
    if (is.null(input$project_start)) {
      return ()
    }
    datatable(
      employee_commute_results(), fillContainer = TRUE
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
      project_lifetime = input$ev_outreach_project_lifetime,
      conversion_rate = input$conversion_rate,
      audience = input$audience #LD or HD
    )
  })
  
  output$ev_outreach_table <- renderDataTable({
    if (is.null(input$ev_outreach_project_start)) {
      return ()
    }
    datatable(
      ev_outreach_results(), fillContainer = TRUE
    )
  })

  
  # EV Outreach Reduction Calculation
  ev_infrastructure_results <- reactive({
    if (is.null(input$project_start)) {
      return ()
    }
    ev_infrastructure(
      ev_type = input$ev_type, #LD or HD
      no_chargers = input$no_chargers,
      charge_power = input$charge_power,
      annual_hours_available = input$annual_hours_available,
      location = input$ev_infrastructure_location, #all locations can be extracted from CommunityTypeShape
      project_start = input$ev_infrastructure_project_start,
      project_lifetime = input$ev_infrastructure_project_lifetime
      )
  })
  
  output$ev_infrastructure_table <- renderDataTable({
    if (is.null(input$project_start)) {
      return ()
    }
    datatable(
      ev_infrastructure_results(), fillContainer = TRUE
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
      project_lifetime = input$shared_mobility_project_lifetime,
      project_start = input$shared_mobility_project_start,
      location = input$shared_mobility_location
    )
  })
  
  output$shared_mobility_table <- renderDataTable({
    if (is.null(input$project_start)) {
      return ()
    }
    datatable(
      shared_mobility_results(), fillContainer = TRUE
    )
  })
  
  # EV Outreach Reduction Calculation
  transit_expansion_results <- reactive({
    if (is.null(input$project_start)) {
      return ()
    }
    transit_expansion(
      ridership_increase = input$ridership$increase,
      route_type = input$route_type, #options in AdjustmentFactorsAndTripLengths
      added_transit = input$added_transit,
      fleet_type = input$fleet_type, #gasoline, diesel, or electricity for now 
      location = input$location,
      project_start = input$project_start,
      project_lifetime = input$project_lifetime
    )
  })
  
  output$transit_expansion_table <- renderDataTable({
    if (is.null(input$project_start)) {
      return ()
    }
    datatable(
      transit_expansion_results(), fillContainer = TRUE
    )
  })
  
  foundational.map <- shiny::reactive({
    leaflet() %>%
      addTiles( urlTemplate = "https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png") %>%
      addPolygons( data = population
                   , fillOpacity = 0
                   , opacity = 0.2
                   , color = "#000000"
                   , weight = 2
                   , layerId = population$GEOID
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
          radius = 16093,
          group = "circle"
        )
    } 
  }) 
}

