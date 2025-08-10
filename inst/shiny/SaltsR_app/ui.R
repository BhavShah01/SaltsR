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
    height = 600,
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
             card_header("Salt ion correction"),
             card_body(
               class = "p-0",
               plotOutput("salts_corrected_graph"),
               downloadButton("salts_download", "Download complete results"),
               )
             ),
      )),
    nav_panel(
      "Output from Runsalt",
      fileInput("ECOS_file_upload", "Choose a Runsalt Output File"),
      layout_column_wrap(
        # width = 1/2,
        height = 300,

        card(full_screen = TRUE,
             card_header("ECOS output"),
             DT::dataTableOutput("ECOSoutput_table")
             ),
        card(full_screen = TRUE,
             card_header("ECOS output2"),
             card_body(class = "p-0", plotOutput("ECOSoutput_graph")))
      )),
    nav_panel(
      "Help",
      tags$a(
        "Link to tools KIK-IRPA PREDICT project",
        href = "https://predict.kikirpa.be/index.php/tools/",
        target = "_blank"
      ),
      layout_column_wrap(
        height = 300,
        card(full_screen = TRUE,
             card_header("ECOS output"),
             "Under development...")
      ))
  )
)
