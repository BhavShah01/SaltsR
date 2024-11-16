# inst/SaltsR_app/app.R

library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "ConSciR example",
  sidebar = sidebar(
    title = "Select",
    uiOutput("sel_var"),
    uiOutput("bins_select")
  ),
  # card(
  #   card_header("Histogram"),
  #   plotOutput("gg_histogram")
  # ),
)
