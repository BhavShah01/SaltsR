# inst/shiny/SaltsR/app.R
## SaltsR app


ui <- page_sidebar(
  title = "SaltsR",

  sidebar = sidebar(
    title = "Histogram controls",
    uiOutput("var"),
    uiOutput("bins")
  ),

  card(height = 1000,
    card_header("Histogram"),
    plotOutput("p")
  ),
  card(
    card_header("Histogram2")
  )
)
