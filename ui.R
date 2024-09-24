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
              # nav_panel(title = "Employee Commute Reduction", !!!employee_commute_reduction),
              nav_panel(title = "Two",
                        page_fillable(
                          card(
                            layout_column_wrap(
                              width = 1/2,
                              numericInput("daily_commute_no", 
                                           "Number of Daily One-Way Commute Trips Reduced", 
                                           value = 1000),
                              dateInput("project_start", 
                                        "Project Start", 
                                        value = "2024-01-01"),
                              numericInput("project_lifetime", 
                                           "Project Lifetime (in years)", 
                                           value = 4),
                              numericInput("average_commute", 
                                           "Average Commute Distance (in miles)", 
                                           value = 10.9)
                            )
                          ),
                          card(dataTableOutput("employee_commute_table"))
                          # card(dataTableOutput("dt"))
                        )),
              # nav_panel(title = "Three", p("Third tab content"))
              widths = c(2, 10)
            )),
  nav_panel(title = "Sources", p("Links to resources etc."))   #,
  # nav_spacer(),
  # nav_menu(
  #   title = "Links",
  #   align = "right"
  # ,
  # nav_item(link_shiny),
  # nav_item(link_posit)
  # )
)