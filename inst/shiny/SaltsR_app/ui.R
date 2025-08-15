# inst/SaltsR_app/app.R

library(shiny)
library(bslib)

ui <- page_sidebar(
  title =
    div(
      span("SaltsR - Tool for ECOS Runsalt",
           style = "font-size: 1.5em; font-weight: bold; margin-right: auto;"),
      input_dark_mode(id = "mode", mode = "light"),
      style = "display: flex; align-items: center; justify-content: space-between; width: 100%;"
    ),
  theme = bs_theme(bootswatch = "bootstrap"),
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
      uiOutput("sel_calcium")
    )
  ),

  navset_card_tab(
    title = "ECOS Runsalt tools",
    nav_panel(
      "Input for Runsalt",
      layout_columns(
        card_title("Input for ECOS Runsalt"),
        markdown("[PREDICT Salt Content Calculator (recommended)](https://predict.kikirpa.be/index.php/tools/moisture-and-salt-sample-data-analysis-tool/)"),
        col_widths = c(3, 9)
      ),
      layout_column_wrap(
        width = 1/2,
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
      title = "Output from Runsalt",
      # card_title("Output from ECOS Runsalt"),
      layout_column_wrap(
        card(full_screen = TRUE,
             card_header("Runsalt Output File"),
             fileInput("ECOS_file_upload", "1) Load a Runsalt Output File"),
             "[Runsalt: Plot > Export Plot Data...]",
             uiOutput("ECOS_temperature"),
             DT::dataTableOutput("ECOSoutput_table")
        ),
        card(full_screen = TRUE,
             card_header("Runsalt Output Graph"),
             card_body(class = "p-0", plotOutput("ECOSoutput_graph")))
      )),
    nav_panel(
      "Output - Multiple Temperature Files",
      layout_columns(
        col_widths = c(5, 7),
        layout_column_wrap(
          width = 1,
          card(
            full_screen = TRUE,
            style = "max-height: 400px; overflow-y: auto;",
            card_header("Multiple Runsalt Output Files by Temperature"),
            card_body(
              "1) Upload Runsalt Output Files by Temperature",
              fileInput("ECOS_upload00C", "Runsalt Output (0°C)"),
              fileInput("ECOS_upload05C", "Runsalt Output (5°C)"),
              fileInput("ECOS_upload10C", "Runsalt Output (10°C)"),
              fileInput("ECOS_upload15C", "Runsalt Output (15°C)"),
              fileInput("ECOS_upload20C", "Runsalt Output (20°C)"),
              fileInput("ECOS_upload25C", "Runsalt Output (25°C)"),
              fileInput("ECOS_upload30C", "Runsalt Output (30°C)"),
              fileInput("ECOS_upload35C", "Runsalt Output (35°C)"),
              fileInput("ECOS_upload40C", "Runsalt Output (40°C)"),
              DT::dataTableOutput("ECOSmultiple_table"))
          ),
          card(
            full_screen = TRUE,
            style = "max-height: 300px; overflow-y: auto;",
            fileInput("TRH_upload", "2) Upload TRH data"),
            '["TEMPERATURE", "HUMIDITY" columns required as CSV]',
            DT::dataTableOutput("TRH_table")
          )),
        layout_column_wrap(
          width = 1,
          card(
            full_screen = TRUE,
            card_header("Runsalt Output Graph"),
            card_body(class = "p-0",
                      plotOutput("ECOSmultiple_graph"))
          ),
          card(
            full_screen = TRUE,
            card_header("Runsalt Output Graph + TRH data"),
            card_body(class = "p-0",
                      plotOutput("ECOSmultiple_TRHgraph"))
          )),
      )),
    nav_panel(
      "Worldmet Weather data",
      layout_column_wrap(
        card(
          fill = TRUE,
          card_header("Download Worldmet weather data"),
          uiOutput("select_worldmet_sites"),
          uiOutput("select_lat"),
          uiOutput("select_lon"),
          uiOutput("select_worldmet_year"),
          downloadButton("worldmet_download", "Download Weather Data"),
          tableOutput("worldmet_siteTable"),
          leaflet::leafletOutput("worldmet_leafletmap")),
      )),
    nav_panel(
      "Help",
      markdown(
        '

      - [Runsalt software](http://science.sdf-eu.org/runsalt/): Prediction of salt mixture behaviour under changing climate conditions using the ECOS thermodynamic model.
      - [KIK-IRPA PREDICT tools](https://predict.kikirpa.be/index.php/tools/): Suite of calculators for hygroscopic moisture content, ion balance, and climate data visualisation.
      - [SaltsR R package](https://bhavshah01.github.io/SaltsR/): Data science tools in development for interoperability with Runsalt software.

      **Input for Runsalt**

      This tool is designed for data analysis and modelling of salt behaviour in cultural heritage materials.
      Calculations are based on [Godts et al. 2022](https://www.nature.com/articles/s41597-022-01445-9).

      Input the ion data using the left-hand data input tab; corrected ion values will be returned for use with Runsalt software.
      Please review any warnings or messages listed in the output.

      For the most up-to-date calculations, use the
      [PREDICT Salts Content Calculator](https://predict.kikirpa.be/index.php/tools/moisture-and-salt-sample-data-analysis-tool/).

      **Output from Runsalt**

      The output from Runsalt can be uploaded and visualised. Within Runsalt, calculations can be saved using “Runsalt: Plot > Export Plot Data...”.

      **Output – Multiple Temperature Files**

      Multiple Runsalt output files at 5°C temperature intervals can be uploaded and graphed.
      You may also upload data from temperature and humidity loggers—files must have columns labelled TEMPERATURE and HUMIDITY in the header.

      **Worldmet Weather data**

      Under development to return data from the NOAA Integrated Surface Database (ISD).
      Uses the [worldmet R package, Carslaw and Davidson 2025](https://openair-project.github.io/worldmet/).

      **Disclaimer:** This application is under development.
      The author does not accept responsibility for decisions made based on its output.

      ')
    )
  )
)
