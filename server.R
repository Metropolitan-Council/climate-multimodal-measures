server <- function(input, output) {
  # Employee Commute Reduction Calculation
  employee_commute_results <- reactive({
    employee_commute(
      daily_commute_no = input$daily_commute_no,
      project_start = input$project_start,
      project_lifetime = input$project_lifetime,
      average_commute = input$average_commute
    )
  })
}