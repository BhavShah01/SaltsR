# inst/SaltsR_app/app.R

library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "SaltsR",
  theme = bs_theme(bootswatch = "shiny"),
  sidebar = sidebar(
    title = "Select",
  ),
  card(
    height = 350,
    full_screen = TRUE,
    card_header("Salt balance (single)"),

    card_body(
     #min_height = 10,
      layout_column_wrap(
        width = 1/5,
        uiOutput("sel_sample"),
        uiOutput("sel_dry_g"),
        uiOutput("sel_water_ml"),
        textOutput("salts_pathway"),
        textOutput(" salts_warning")
      ),
      layout_column_wrap(
        width = 1/7,
        uiOutput("sel_chloride"),
        uiOutput("sel_nitrate"),
        uiOutput("sel_sulfate"),
        uiOutput("sel_sodium"),
        uiOutput("sel_potassium"),
        uiOutput("sel_calcium"),
        uiOutput("sel_magnesium")
      ),
      "Results",
      "Molar inputs for ECOS Runsalt",
      DT::dataTableOutput("salts_x_ECOS"),
      "Weight inputs for ECOS Runsalt",
      DT::dataTableOutput("salts_wt_ECOS")
      )
    ),
)
