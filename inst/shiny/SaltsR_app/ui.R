# inst/SaltsR_app/app.R
# https://oceanonline.shinyapps.io/SaltsRApp/

library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "SaltsR Application - Tools for ECOS Runsalt",
  theme = bs_theme(bootswatch = "bootstrap"),
  sidebar = sidebar(
    title = "SaltsRApp",
    open = FALSE,
    markdown("[Runsalt software](http://science.sdf-eu.org/runsalt/)"),
    markdown("[KIK-IRPA PREDICT project](https://predict.kikirpa.be/)"),
    markdown("[SaltsR Github](https://bhavshah01.github.io/SaltsR/)"),
  ),

  navset_card_tab(
    title = "ECOS Runsalt tools",
    nav_panel(
      "Input for Runsalt",
      layout_columns(
        card_title("Salt Balance for ECOS Runsalt"),
        markdown("**[PREDICT Salt Content Calculator:](https://predict.kikirpa.be/index.php/tools/moisture-and-salt-sample-data-analysis-tool/)**
                Recommended for details and the most up to date calculations."),
        col_widths = c(4, 8)
      ),
      layout_column_wrap(
        width = 1/3,
        card(
          full_screen = TRUE,
          card_header("1) Enter ion data"),
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
          ),
        ),
        card(
          full_screen = TRUE,
          card_header("2)  Check ion correction"),
          card_body(
            class = "p-0",
            textOutput("salts_messages"),
            plotOutput("salts_corrected_graph_wt"),
            # plotOutput("salts_corrected_graph_mol"),
          )
        ),
        card(
          full_screen = TRUE,
          card_header("3) Balanced data for input into Runsalt"),
          DT::dataTableOutput("ECOS_table"),
          downloadButton("download_runsalt", "4) Download Runsalt File"),
          "5) Upload in Runsalt software: File > Open...",
          # downloadButton("salts_download", "Download complete results"),
        ),
      )),
    nav_panel(
      title = "Output from Runsalt",
      card_title("Graph the output from ECOS Runsalt"),
      layout_columns(
        col_widths = c(4, 8),
        layout_column_wrap(
          card(full_screen = TRUE,
               card_header("Runsalt Output File"),
               "1) Export the graph data from Runsalt: Plot > Export Plot Data...",
               fileInput("ECOS_file_upload", "2) Load a Runsalt Output File"),
               uiOutput("ECOS_temperature"),
               checkboxInput("filter_crystal", "Add crystallisation points", value = TRUE),
               checkboxInput("filter_eqm", "Add equilibrium points", value = FALSE),
               DT::dataTableOutput("ECOSoutput_table")
          )),
        layout_column_wrap(
          card(full_screen = TRUE,
               card_header("Runsalt Output Graph"),
               card_body(class = "p-0", plotOutput("ECOSoutput_graph"))
          ))
      )),
    nav_panel(
      title = "Output - Multiple Temperature Files",
      card_title("Upload multiple temperature files in 5C steps and overlay TRH data"),
      layout_columns(
        col_widths = c(4, 8),
        layout_column_wrap(
          width = 1,
          card(
            full_screen = TRUE,
            style = "max-height: 400px; overflow-y: auto;",
            card_header("1) Upload Runsalt Output Files by Temperature"),
            card_body(
              fileInput("ECOS_upload00C", "Runsalt Output (0°C)"),
              fileInput("ECOS_upload05C", "Runsalt Output (5°C)"),
              fileInput("ECOS_upload10C", "Runsalt Output (10°C)"),
              fileInput("ECOS_upload15C", "Runsalt Output (15°C)"),
              fileInput("ECOS_upload20C", "Runsalt Output (20°C)"),
              fileInput("ECOS_upload25C", "Runsalt Output (25°C)"),
              fileInput("ECOS_upload30C", "Runsalt Output (30°C)"),
              fileInput("ECOS_upload35C", "Runsalt Output (35°C)"),
              fileInput("ECOS_upload40C", "Runsalt Output (40°C)"),
              h3("Add Additional Temperature inputs"),
              fileInput("ECOS_uploadX1C", "Runsalt Output (Specify Temp°C below)"),
              uiOutput("ECOS_tempX1"),
              fileInput("ECOS_uploadX2C", "Runsalt Output (Specify Temp°C below)"),
              uiOutput("ECOS_tempX2"),
              fileInput("ECOS_uploadX3C", "Runsalt Output (Specify Temp°C below)"),
              uiOutput("ECOS_tempX3"),
              fileInput("ECOS_uploadX4C", "Runsalt Output (Specify Temp°C below)"),
              uiOutput("ECOS_tempX4"),
              fileInput("ECOS_uploadX5C", "Runsalt Output (Specify Temp°C below)"),
              uiOutput("ECOS_tempX5"),
              fileInput("ECOS_uploadX6C", "Runsalt Output (Specify Temp°C below)"),
              uiOutput("ECOS_tempX6"),
              fileInput("ECOS_uploadX7C", "Runsalt Output (Specify Temp°C below)"),
              uiOutput("ECOS_tempX7"),
              fileInput("ECOS_uploadX8C", "Runsalt Output (Specify Temp°C below)"),
              uiOutput("ECOS_tempX8"),
              dataTableOutput("ECOS_multiple_table"),
            )),
          card(
            full_screen = TRUE,
            style = "max-height: 300px; overflow-y: auto;",
            card_header("2) Filter salts"),
            uiOutput("salt_filter"),
          ),
          card(
            full_screen = TRUE,
            style = "max-height: 300px; overflow-y: auto;",
            card_header("3) Upload TRH data if available"),
            fileInput("TRH_upload", "Upload TRH data"),
            '"TEMPERATURE", "HUMIDITY" columns are required in a CSV file',
          )),
        layout_column_wrap(
          width = 1,
          card(
            full_screen = TRUE,
            card_header("Runsalt Output Table"),
            DT::dataTableOutput("ECOS_multiple_summary_table"),
          ),
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
      Issues with the SaltsR app can be raised on the [Github issues page](https://github.com/BhavShah01/SaltsR/issues).

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

      '
      ))
  )
)
