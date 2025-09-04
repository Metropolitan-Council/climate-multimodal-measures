# You must have credentials loaded for the Met Council shinyapps.io account
# to run this script
rsconnect::deployApp(
  appDir = "../climate-multimodal-measures",
  account = "metrotransitmn",
  server = "shinyapps.io",
  appName = "beta-climate-multimodal-measures",
  appTitle = "beta-climate-multimodal-measures",
  launch.browser = function(url) {
    message("Deployment completed: ", url)
  },
  lint = FALSE, metadata = list(asMultiple = FALSE, asStatic = FALSE),
  logLevel = "verbose"
)
