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
  
}

function(input, output) {
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
}

function(input, output) {
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
}
