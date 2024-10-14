#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

function(input, output) {
  output$dt <- renderDataTable({
    datatable(
      mtcars, fillContainer = TRUE
    )
  })
  
  # Employee Commute Reduction Calculation
  employee_commute_results <- reactive({
    if (is.null(input$project_start)) {
      return ()
    }
    employee_commute(
      daily_commute_no = input$daily_commute_no,
      project_start = input$project_start,
      project_lifetime = input$project_lifetime,
      community_type = "Urban", #input$community_type
      location = "Andover" #input$location
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
  
  # First DataTable output
  output$dt <- renderDataTable({
    datatable(
      mtcars, fillContainer = TRUE
    )
  })
  
  # EV Outreach Reduction Calculation
  ev_outreach_results <- reactive({
    if (is.null(input$project_start)) {
      return ()
    }
    ev_outreach(
      no_participants = input$no_participants,
      project_start = input$project_start,
      project_lifetime = input$project_lifetime,
      conversion_rate = input$conversion_rate,
      audience = input$audience #LD or HD
    )
  })
  
  output$ev_outreach_table <- renderDataTable({
    if (is.null(input$project_start)) {
      return ()
    }
    datatable(
      ev_outreach_results(), fillContainer = TRUE
    )
  })

  # First DataTable output
  output$dt <- renderDataTable({
    datatable(
      mtcars, fillContainer = TRUE
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
      location = input$location, #all locations can be extracted from CommunityTypeShape
      project_start = input$project_start,
      project_lifetime = input$project_lifetime
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

  # First DataTable output
  output$dt <- renderDataTable({
    datatable(
      mtcars, fillContainer = TRUE
    )
  })
  
  # EV Outreach Reduction Calculation
  shared_mobility_results <- reactive({
    if (is.null(input$project_start)) {
      return ()
    }
    shared_mobility(
      fleet = input$fleet, #Options are scooter or bicycle, non-ev fleet, and ev fleet
      no_vehicles = input$no_vehicles,
      no_trips = input$no_trips,
      project_lifetime = input$project_lifetime,
      project_start = input$project_start,
      location = input$location
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
  
  foundational.map <- shiny::reactive({
    leaflet() %>%
      addTiles( urlTemplate = "https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png") %>%
      # setView( lng = -87.567215
      #          , lat = 41.822582
      #          , zoom = 11 ) %>%
      addPolygons( data = population
                   , fillOpacity = 0
                   , opacity = 0.2
                   , color = "#000000"
                   , weight = 2
                   , layerId = population$GEOID
                   # , group = "click.list"
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
      
      # weight_area_by_pop <- intersections %>%
      #   mutate(area = as.numeric(st_area(.))) %>% 
      #   group_by(GEOID) %>% 
      #   mutate(weight = area / sum(area),
      #          proportionate_population = round(weight * ACS17_Occupied_Housing_Units_Es)) %>%
      #   ungroup() %>%
      #   group_by(ZIP_CODE) %>%
      #   mutate(estimated_population = sum(proportionate_population),
      #          population_weight = proportionate_population / estimated_population) %>% 
      #   summarize(zip_urban = sum(population_weight * UPSAI_urban),
      #             zip_suburban = sum(population_weight * UPSAI_suburban),
      #             zip_rural = sum(population_weight * UPSAI_rural)) %>%
      #   mutate(zip_code_type = case_when(
      #     pmax(zip_urban, zip_suburban, zip_rural) == zip_urban ~ "Urban",
      #     pmax(zip_urban, zip_suburban, zip_rural) == zip_suburban ~ "Suburban",
      #     pmax(zip_urban, zip_suburban, zip_rural) == zip_rural ~ "Rural"))
      
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

