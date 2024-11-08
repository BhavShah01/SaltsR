# inst/shiny/SaltsR/app.R
## SaltsR app

library(ggplot2)

data(penguins, package = "palmerpenguins")

server <- function(input, output) {
  output$var <- renderUI({
    varSelectInput(
      "var", "Select variable",
      dplyr::select_if(penguins, is.numeric))
  })

  output$bins <- renderUI({
    numericInput("bins", "Number of bins", 30)
  })


  output$p <- renderPlot({
    ggplot(penguins) +
      geom_histogram(aes(!!input$var), bins = input$bins) +
      theme_bw(base_size = 20)
  })
}
