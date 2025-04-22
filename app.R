# Main entry point for Shiny application

source("global.R")
source("ui.R")
source("server.R")
shinyApp(ui = ui, server = server)