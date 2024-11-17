# inst/SaltsR_app/app.R
library(shiny)
library(bslib)


# Load UI and server files
source("ui.R")
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)
