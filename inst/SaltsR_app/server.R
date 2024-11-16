# inst/SaltsR_app/app.R

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

server <- function(input, output) {

  output$sel_var <- renderUI({
    varSelectInput(
      "sel_var", "Select variable",
      dplyr::select_if(mydata, is.numeric)
    )
  })
  output$bins_select <- renderUI({
    numericInput("bins_select", "Number of bins", 30)
  })

  # output$gg_histogram <- renderPlot({
  #   mydata |>
  #     ggplot() +
  #     geom_histogram(aes(!!input$sel_var), bins = input$bins_select) +
  #     theme_bw(base_size = 20)
  # })



}
