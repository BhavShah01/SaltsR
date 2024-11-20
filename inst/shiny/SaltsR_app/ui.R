# inst/SaltsR_app/app.R

library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "SaltsR - Tool for generating input for ECOS Runsalt",
  theme = bs_theme(bootswatch = "shiny"),
  sidebar = sidebar(
    title = "Salt balance",
    uiOutput("sel_sample")
  ),
  card(
    full_screen = TRUE,
    card_header("Enter ion chromatography data"),

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
        h5("Inputs for ECOS Runsalt"),
        textOutput("salts_pathway"),
        textOutput(" salts_warning"),
        DT::dataTableOutput("ECOS_table")
      ))
  ),
)
