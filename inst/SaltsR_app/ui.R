# inst/SaltsR_app/app.R

library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "SaltsR",
  theme = bs_theme(bootswatch = "shiny"),
  sidebar = sidebar(
    title = "Start balance",
    uiOutput("sel_sample"),
    textOutput("salts_pathway"),
    textOutput(" salts_warning"),
  ),
  card(
    full_screen = TRUE,
    card_header("Salt balance (single)"),

    card_body(
      layout_column_wrap(
        width = 1/9,
        uiOutput("sel_dry_g"),
        uiOutput("sel_water_ml"),
        uiOutput("sel_chloride"),
        uiOutput("sel_nitrate"),
        uiOutput("sel_sulfate"),
        uiOutput("sel_sodium"),
        uiOutput("sel_potassium"),
        uiOutput("sel_calcium"),
        uiOutput("sel_magnesium")
      ),
      fluidRow(
      h5("Molar inputs for ECOS Runsalt"),
      DT::dataTableOutput("salts_x_ECOS"),
      h5("Weight inputs for ECOS Runsalt"),
      DT::dataTableOutput("salts_wt_ECOS")
      ))
    ),
)
