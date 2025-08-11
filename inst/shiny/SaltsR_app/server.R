# inst/SaltsR_app/app.R

library(shiny)
library(bslib)
library(dplyr)
library(tibble)
library(tidyr)
library(readr)
library(DT)
library(ggplot2)
library(ggrepel)
library(SaltsR)
library(leaflet)
library(worldmet)
library(shinyWidgets)


server <- function(input, output) {
  # bs_themer()


  # ECOS input ----

  output$sel_sample <- renderUI({
    textInput("sel_sample", "Sample Input", value = "Sample name")
  })
  output$sel_dry_g <- renderUI({
    numericInput("sel_dry_g", "Weight (g)", value = 0.801, min = 0)
  })
  output$sel_water_ml <- renderUI({
    numericInput("sel_water_ml", "Water (ml)", value = 100, min = 0)
  })
  output$sel_chloride <- renderUI({
    numericInput("sel_chloride", "Cl (ppm)", value = 14.651, min = 0)
  })
  output$sel_nitrate <- renderUI({
    numericInput("sel_nitrate", "NO3 (ppm)", value = 17.339, min = 0)
  })
  output$sel_sulfate <- renderUI({
    numericInput("sel_sulfate", "SO4  (ppm)", value = 39.923, min = 0)
  })
  output$sel_sodium <- renderUI({
    numericInput("sel_sodium", "Na (ppm)", value = 2.027, min = 0)
  })
  output$sel_potassium <- renderUI({
    numericInput("sel_potassium", "K (ppm)", value = 2.04, min = 0)
  })
  output$sel_calcium <- renderUI({
    numericInput("sel_calcium", "Ca (ppm)", value = 49.809, min = 0)
  })
  output$sel_magnesium <- renderUI({
    numericInput("sel_magnesium", "Mg (ppm)", value = 0.581, min = 0)
  })

  salts_corrected <- reactive({
    fun_salt_balance(
      sample_name = input$sel_sample,
      dry_g = input$sel_dry_g,
      water_ml = input$sel_water_ml,
      chloride_ppm = input$sel_chloride,
      nitrate_ppm = input$sel_nitrate,
      sulfate_ppm = input$sel_sulfate,
      sodium_ppm = input$sel_sodium,
      potassium_ppm = input$sel_potassium,
      calcium_ppm = input$sel_calcium,
      magnesium_ppm = input$sel_magnesium)
  })

  ECOS_input <- reactive({
    salts_corrected() |>
      select(ends_with("_ECOS_mol")) |> # ends_with("_ECOS_weight"),
      pivot_longer(cols = everything(), names_to = c("salt", "ECOS"), names_sep = "_ECOS_") |>
      pivot_wider(names_from = "salt", values_from = "value")
  })

  output$ECOS_table <- renderDT({
    ECOS_input() |>
      column_to_rownames("ECOS") |>
      t() |>
      DT::datatable(extensions = c("Buttons"),
                    list(dom = 'tB', buttons = list("copy")))
  })

  output$salts_messages <- renderText({
    pathway = salts_corrected() |> pull(ECOS_pathway)
    warnings = salts_corrected() |> pull(ECOS_warnings)
    paste(pathway, warnings, sep = ". ")
  })

  output$salts_download <- downloadHandler(
    filename = "salts_corrected.csv",
    content = function(file) {
      readr::write_excel_csv(salts_corrected(), file)
    }
  )

  output$salts_corrected_graph <- renderPlot({
    salts_corrected() |>
      ggplot() +
      geom_col(aes("Sodium original", sodium_wt), fill = "darkred", alpha = 0.3) +
      geom_col(aes("Sodium ECOS", sodium_ECOS_weight), fill = "darkred", alpha = 0.7) +
      geom_col(aes("Potassium original", potassium_wt), fill = "purple", alpha = 0.3) +
      geom_col(aes("Potassium ECOS", potassium_ECOS_weight), fill = "purple", alpha = 0.7) +
      geom_col(aes("Magnesium original", magnesium_wt), fill = "darkblue", alpha = 0.3) +
      geom_col(aes("Magnesium ECOS", magnesium_ECOS_weight), fill = "darkblue", alpha = 0.7) +
      geom_col(aes("Calcium original", calcium_wt), fill = "gold", alpha = 0.3) +
      geom_col(aes("Calcium ECOS", calcium_ECOS_weight), fill = "gold", alpha = 0.7) +
      geom_col(aes("Chloride original", chloride_wt), fill = "darkorange", alpha = 0.3) +
      geom_col(aes("Chloride ECOS", chloride_ECOS_weight), fill = "darkorange", alpha = 0.7) +
      geom_col(aes("Nitrate original", nitrate_wt), fill = "darkgreen", alpha = 0.3) +
      geom_col(aes("Nitrate ECOS", nitrate_ECOS_weight), fill = "darkgreen", alpha = 0.7) +
      geom_col(aes("Sulfate original", sulfate_wt), fill = "hotpink", alpha = 0.3) +
      geom_col(aes("Sulfate ECOS", sulfate_ECOS_weight), fill = "hotpink", alpha = 0.7) +
      labs(x = NULL, y = "Weight") +
      coord_flip() +
      theme_classic(base_size = 16)
  })

  # ECOS output ----

  # ECOS output upload
  ECOS_tidy <- reactive({
    req(input$ECOS_file_upload)

    ECOS_file <- input$ECOS_file_upload
    ext <- tools::file_ext(ECOS_file$datapath)
    validate(need(ext == "txt", "Please upload a Runsalt output txt file"))
    tidyRunsalt(ECOS_file$datapath)
  })

  ECOS_output <- reactive({
    req(ECOS_tidy())
    ECOS_tidy() |>
      group_by(Salt) |>
      mutate(
        Crystallisation = ifelse(X == max(X, na.rm = TRUE), X, NA),
        diffY = Y - lag(Y, 1),
        diffY2 = diffY - lag(diffY, 1),
        RH_eqm = ifelse(diffY2 > 0, lag(X, 1), ""),
        RH_eqm = ifelse(diffY2 == "NA", NA, RH_eqm)) |>
      dplyr::select(-diffY, -diffY2)
  })

  # Data table
  output$ECOSoutput_table <- DT::renderDataTable({
    req(ECOS_output())
    ECOS_output() |>
      DT::datatable(extensions = c("Buttons"),
                    list(dom = 'Bt', buttons = list("excel"), pageLength = nrow(ECOS_output())))
  })

  # Graph
  output$ECOSoutput_graph <- renderPlot({
    req(ECOS_output())
    ECOS_output() |>
      ggplot(aes(X, Y, col = Salt, fill = Salt)) +
      geom_line(alpha = 0.7, size = 1.5) +
      geom_point(alpha = 0.7, size = 1) +
      ggrepel::geom_text_repel(aes(label = Crystallisation)) +
      labs(x = "Humidity (%RH)", y = "Amount of substance (mol)",
           title = NULL,
           subtitle = paste0("Temperature ", unique(ECOS_output()$Temp), "C"),
           caption = "Price (2000) and Bionda (2005)") +
      theme_classic(base_size = 16)
  })


  # Worldmet weather data ----

  output$select_worldmet_year <- renderUI({
    shinyWidgets::airYearpickerInput(
      "select_worldmet_year", "Select years of data", value = "2025-01-01",
      multiple = TRUE, clearButton = TRUE)
  })

  output$select_lat <- renderUI({
    numericInput("select_lat", "Latitude", value = 51.5)
  })

  output$select_lon <- renderUI({
    numericInput("select_lon", "Longitude", value = -0.17)
  })

  worldmet_sites <- reactive({
    worldmet::getMeta(
      lat = input$select_lat,
      lon = input$select_lon)
  })

  worldmet_site_names <- reactive({
    worldmet_sites() |> pull(station)
  })

  output$select_worldmet_sites <- renderUI({
    selectInput("select_worldmet_sites", "Select Station", multiple = FALSE,
                choices = worldmet_site_names(),
                selected = worldmet_site_names()[1])
  })

  worldmet_site_code <- reactive({
    worldmet_sites() |>
      filter(station %in% input$select_worldmet_sites) |>
      pull(code)
  })

  worldmet_data <- reactive({
    worldmet::importNOAA(
      code = c(worldmet_site_code()),
      year = lubridate::year(input$select_worldmet_year))
  })

  output$worldmet_leafletmap <- renderLeaflet({
    worldmet::getMeta(
      plot = TRUE,
      site = c(worldmet_site_names()),
      lat = input$select_lat, lon = input$select_lon
      )
  })

  output$worldmet_siteTable <- renderTable({
    worldmet_sites()
  })

  output$locationdata_rawDownload <- downloadHandler(
    filename = "worldmet_data.csv",
    content = function(con) {
      readr::write_excel_csv(worldmet_data(), con)
    })


}
