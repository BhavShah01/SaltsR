# inst/SaltsR_app/app.R

library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "SaltsR - Tool for ECOS Runsalt",
  theme = bs_theme(bootswatch = "shiny"),
  sidebar = sidebar(
    title = "Salt balance",
    uiOutput("sel_sample"),
    layout_column_wrap(
      width = 1/2,
      uiOutput("sel_dry_g"),
      uiOutput("sel_water_ml"),
      uiOutput("sel_sodium"),
      uiOutput("sel_chloride"),
      uiOutput("sel_potassium"),
      uiOutput("sel_nitrate"),
      uiOutput("sel_magnesium"),
      uiOutput("sel_sulfate"),
      uiOutput("sel_calcium"),

    )
  ),

  navset_card_tab(
    title = "Results",
    height = 300,
    nav_panel(
      "Input for Runsalt",
      card_title("Input for ECOS Runsalt"),
      layout_column_wrap(
        width = 1/2,
        height = 300,
        card(full_screen = TRUE,
             card_header("Input for Runsalt"),
             DT::dataTableOutput("ECOS_table"),
             textOutput("salts_messages"),
             ),
        card(full_screen = TRUE,
             card_header("Input results"),
             card_body(
               class = "p-0",
               plotOutput("salts_results_graph"),
               downloadButton("salts_download", "Download complete results"),
               )
             ),
      )),
    nav_panel(
      "Output from Runsalt",
      card_title("Output from Runsalt"),
      "The output from Runsalt can uploaded.",
      layout_column_wrap(
        # width = 1/2,
        height = 300,
        card(full_screen = TRUE,
             card_header("ECOS output"),
             "Under development..."),
        card(full_screen = TRUE,
             card_header("Development..."),
             card_body(class = "p-0", "Under development..."))
      ))
  )
)
