# inst/SaltsR_app/app.R
# https://oceanonline.shinyapps.io/SaltsRApp/

# devtools::install_github("BhavShah01/SaltsR")
# library(SaltsR)
source("R.R")

library(shiny)
library(bslib)
library(dplyr)
library(tibble)
library(tidyr)
library(readr)
library(DT)
library(ggplot2)
library(ggrepel)
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
    numericInput("sel_dry_g", "Weight (g)", value = 0.1, min = 0)
  })
  output$sel_water_ml <- renderUI({
    numericInput("sel_water_ml", "Water (ml)", value = 5, min = 0)
  })
  output$sel_chloride <- renderUI({
    numericInput("sel_chloride", "Cl (ppm)", value = 102.4688, min = 0)
  })
  output$sel_nitrate <- renderUI({
    numericInput("sel_nitrate", "NO3 (ppm)", value = 76.6618, min = 0)
  })
  output$sel_sulfate <- renderUI({
    numericInput("sel_sulfate", "SO4  (ppm)", value = 71.0603, min = 0)
  })
  output$sel_sodium <- renderUI({
    numericInput("sel_sodium", "Na (ppm)", value = 131.3564, min = 0)
  })
  output$sel_potassium <- renderUI({
    numericInput("sel_potassium", "K (ppm)", value = 31.9037, min = 0)
  })
  output$sel_calcium <- renderUI({
    numericInput("sel_calcium", "Ca (ppm)", value = 224.9508, min = 0)
  })
  output$sel_magnesium <- renderUI({
    numericInput("sel_magnesium", "Mg (ppm)", value = 11.2682, min = 0)
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
      pivot_longer(cols = everything(),
                   names_to = c("salt", "ECOS"),
                   names_sep = "_ECOS_") |>
      pivot_wider(names_from = "salt", values_from = "value")
  })

  output$ECOS_table <- renderDT({
    ECOS_input() |>
      column_to_rownames("ECOS") |>
      t() |>
      DT::datatable(extensions = c("Buttons"),
                    list(dom = 't', buttons = list("copy")))
  })

  output$salts_messages <- renderText({
    pathway = salts_corrected() |> pull(ECOS_pathway)
    warnings = salts_corrected() |> pull(ECOS_warnings)
    paste(pathway, warnings, sep = ". ")
  })

  output$salts_download <- downloadHandler(
    filename = paste0("salts_corrected_", input$sel_sample, ".csv"),
    content = function(file) {
      readr::write_excel_csv(salts_corrected(), file)
    }
  )

  output$download_runsalt <- downloadHandler(
    filename = function() {
      paste0("runsalt_upload_", input$sel_sample, ".txt")
    },
    content = function(file) {
      df <- salts_corrected()

      lines <- c(
        paste(df$sodium_ECOS_mol, "  ; Na"),
        paste(df$potassium_ECOS_mol, "  ; K"),
        paste(df$magnesium_ECOS_mol, "  ; Mg"),
        paste(df$calcium_ECOS_mol, "  ; Ca"),
        paste(df$chloride_ECOS_mol, "  ; Cl"),
        paste(df$nitrate_ECOS_mol, "  ; NO3"),
        paste(df$sulfate_ECOS_mol, "  ; SO4"),
        paste(20, "  ; Tconst"),
        paste(15, "  ; RHmin"),
        paste(98, "  ; RHmax"),
        paste(50, "  ; RHconst"),
        paste(-30, "  ; Tmin"),
        paste(50, "  ; Tmax"),
        paste(0, "  ; unit (0=mol, 1=weight)"),
        paste0("\"", df$sample_name, "\"   ; sample name")
      )
      writeLines(lines, file)
    }
  )




  output$salts_corrected_graph_wt <- renderPlot({
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
      labs(x = NULL, y = "Weight (mol/kg)",
           title = "Weight corrected for Runsalt",
           subtitle = paste(input$sel_sample),
           caption = NULL) +
      coord_flip() +
      theme_classic(base_size = 16)
  })

  output$salts_corrected_graph_mol <- renderPlot({
    salts_corrected() |>
      ggplot() +
      geom_col(aes("Sodium original", sodium_mol), fill = "darkred", alpha = 0.3) +
      geom_col(aes("Sodium ECOS", sodium_ECOS_mol), fill = "darkred", alpha = 0.7) +
      geom_col(aes("Potassium original", potassium_mol), fill = "purple", alpha = 0.3) +
      geom_col(aes("Potassium ECOS", potassium_ECOS_mol), fill = "purple", alpha = 0.7) +
      geom_col(aes("Magnesium original", magnesium_mol), fill = "darkblue", alpha = 0.3) +
      geom_col(aes("Magnesium ECOS", magnesium_ECOS_mol), fill = "darkblue", alpha = 0.7) +
      geom_col(aes("Calcium original", calcium_mol), fill = "gold", alpha = 0.3) +
      geom_col(aes("Calcium ECOS", calcium_ECOS_mol), fill = "gold", alpha = 0.7) +
      geom_col(aes("Chloride original", chloride_mol), fill = "darkorange", alpha = 0.3) +
      geom_col(aes("Chloride ECOS", chloride_ECOS_mol), fill = "darkorange", alpha = 0.7) +
      geom_col(aes("Nitrate original", nitrate_mol), fill = "darkgreen", alpha = 0.3) +
      geom_col(aes("Nitrate ECOS", nitrate_ECOS_mol), fill = "darkgreen", alpha = 0.7) +
      geom_col(aes("Sulfate original", sulfate_mol), fill = "hotpink", alpha = 0.3) +
      geom_col(aes("Sulfate ECOS", sulfate_ECOS_mol), fill = "hotpink", alpha = 0.7) +
      labs(x = NULL, y = "Mol",
           title = "Mol corrected for Runsalt",
           subtitle = paste(input$sel_sample),
           caption = NULL) +
      coord_flip() +
      theme_classic(base_size = 16)
  })



  # ECOS output ----

  # ECOS output upload
  ECOS_tidy <- reactive({
    req(input$ECOS_file_upload)
    #
    ECOS_file <- input$ECOS_file_upload
    ext <- tools::file_ext(ECOS_file$datapath)
    # validate(need(ext == "txt", "Please upload a Runsalt output txt file"))
    tidyRunsalt(ECOS_file$datapath, Temp_value = input$ECOS_temperature)
  })

  output$ECOS_temperature <- renderUI({
    numericInput("ECOS_temperature", "2) Enter Temperature (C)", value = 20)
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
      DT::datatable(
        extensions = c("Buttons"),
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
           title = "ECOS model output",
           subtitle = paste0("Temperature ", unique(ECOS_output()$Temp), "C"),
           caption = "Crystallisation points are labelled") +
      theme_classic(base_size = 16)
  })


  # ECOS output (multiple) ----

  ECOS_tidy00C <- reactive({
    req(input$ECOS_upload00C)
    ECOS_file00 <- input$ECOS_upload00C
    ext <- tools::file_ext(ECOS_file00$datapath)
    # validate(need(ext == "txt", "Please upload a Runsalt output txt file"))
    tidyRunsalt(ECOS_file00$datapath, Temp_value = 0)
  })
  ECOS_tidy05C <- reactive({
    req(input$ECOS_upload05C)
    ECOS_file05 <- input$ECOS_upload05C
    ext <- tools::file_ext(ECOS_file05$datapath)
    # validate(need(ext == "txt", "Please upload a Runsalt output txt file"))
    tidyRunsalt(ECOS_file05$datapath, Temp_value = 5)
  })
  ECOS_tidy10C <- reactive({
    req(input$ECOS_upload10C)
    ECOS_file10 <- input$ECOS_upload10C
    ext <- tools::file_ext(ECOS_file10$datapath)
    # validate(need(ext == "txt", "Please upload a Runsalt output txt file"))
    tidyRunsalt(ECOS_file10$datapath, Temp_value = 10)
  })
  ECOS_tidy15C <- reactive({
    req(input$ECOS_upload015C)
    ECOS_file15 <- input$ECOS_upload15C
    ext <- tools::file_ext(ECOS_file15$datapath)
    # validate(need(ext == "txt", "Please upload a Runsalt output txt file"))
    tidyRunsalt(ECOS_file15$datapath, Temp_value = 15)
  })
  ECOS_tidy20C <- reactive({
    req(input$ECOS_upload20C)
    ECOS_file20 <- input$ECOS_upload20C
    ext <- tools::file_ext(ECOS_file20$datapath)
    # validate(need(ext == "txt", "Please upload a Runsalt output txt file"))
    tidyRunsalt(ECOS_file20$datapath, Temp_value = 20)
  })
  ECOS_tidy25C <- reactive({
    req(input$ECOS_upload025C)
    ECOS_file25 <- input$ECOS_upload25C
    ext <- tools::file_ext(ECOS_file25$datapath)
    # validate(need(ext == "txt", "Please upload a Runsalt output txt file"))
    tidyRunsalt(ECOS_file25$datapath, Temp_value = 25)
  })
  ECOS_tidy30C <- reactive({
    req(input$ECOS_upload30C)
    ECOS_file30 <- input$ECOS_upload30C
    ext <- tools::file_ext(ECOS_file30$datapath)
    # validate(need(ext == "txt", "Please upload a Runsalt output txt file"))
    tidyRunsalt(ECOS_file30$datapath, Temp_value = 30)
  })
  ECOS_tidy35C <- reactive({
    req(input$ECOS_upload35C)
    ECOS_file <- input$ECOS_upload35C
    ext <- tools::file_ext(ECOS_file$datapath)
    # validate(need(ext == "txt", "Please upload a Runsalt output txt file"))
    tidyRunsalt(ECOS_file35$datapath, Temp_value = 35)
  })
  ECOS_tidy40C <- reactive({
    req(input$ECOS_upload40C)
    ECOS_file40 <- input$ECOS_upload40C
    ext <- tools::file_ext(ECOS_file40$datapath)
    # validate(need(ext == "txt", "Please upload a Runsalt output txt file"))
    tidyRunsalt(ECOS_file40$datapath, Temp_value = 40)
  })

  ECOS_multiple <- reactive({
    lst <- list(
      `00C` = purrr::safely(~.x) (ECOS_tidy00C())$result,
      `05C` = purrr::safely(~.x) (ECOS_tidy05C())$result,
      `10C` = purrr::safely(~.x) (ECOS_tidy10C())$result,
      `15C` = purrr::safely(~.x) (ECOS_tidy15C())$result,
      `20C` = purrr::safely(~.x) (ECOS_tidy20C())$result,
      `25C` = purrr::safely(~.x) (ECOS_tidy25C())$result,
      `30C` = purrr::safely(~.x) (ECOS_tidy30C())$result,
      `35C` = purrr::safely(~.x) (ECOS_tidy35C())$result,
      `40C` = purrr::safely(~.x) (ECOS_tidy40C())$result
    )
    lst <- purrr::compact(lst)
    if (length(lst) == 0) return(NULL)
    dplyr::bind_rows(lst, .id = "temp_label")
  })


  output$salt_filter <- shiny::renderUI({
    df <- ECOS_multiple()
    shiny::selectInput(
      inputId = "salt_filter",
      label = "Select Salt(s):",
      selected = sort(unique(df$Salt)),
      choices = sort(unique(df$Salt)),
      multiple = TRUE
    )
  })

  ECOS_multiple_filtered <- reactive({
    req(input$salt_filter)
    df <- ECOS_multiple()
    df_filtered <- df[df$Salt %in% input$salt_filter, ]
    df_filtered
  })

  ECOS_multiple_tabledata <- reactive({
    req(ECOS_multiple_filtered())
    ECOS_multiple_filtered() |>
      group_by(Salt, Temp) |>
      summarise(
        RHmin = min(RH, na.rm = TRUE),
        RHmax = max(RH, na.rm = TRUE),
        RHrange = RHmax - RHmin
      )
  })


  # Data table multiple
  output$ECOSmultiple_table <- DT::renderDataTable({
    req(ECOS_multiple_tabledata())
    ECOS_multiple_tabledata() |>
      DT::datatable(
        extensions = c("Buttons"),
        list(dom = 'Bt', buttons = list("excel"), pageLength = nrow(ECOS_multiple())))
  })


  # Graph multiple
  output$ECOSmultiple_graph <- renderPlot({
    req(ECOS_multiple_filtered())

    ecos_data <- ECOS_multiple_filtered() |>
      group_by(Salt, Temp, filename) |>
      mutate(RH_lower = min(RH), RH_upper = max(RH))

    p <-
      ggplot(ecos_data, aes(x = Temp, ymin = RH_lower, ymax = RH_upper, fill = Salt)) +
      geom_ribbon(alpha = 0.2) +
      coord_cartesian(ylim = c(15, 100)) +
      labs(title = NULL, x = "Temperature (°C)", y = "Humidity (%RH)") +
      facet_wrap(~Salt) +
      theme_classic(base_size = 16)

    return(p)
  })


  # TRH data
  TRHdata <- reactive({
    req(input$TRH_upload)
    TRH_file <- input$TRH_upload
    ext <- tools::file_ext(TRH_file$datapath)
    read_csv(TRH_file$datapath,
             col_types = cols(DATE = col_datetime(format = "%Y-%m-%d %H:%M:%S")))
  })

  # Graph multiple + TRH data
  output$ECOSmultiple_TRHgraph <- renderPlot({
    req(ECOS_multiple_filtered())

    trh <- TRHdata()
    ecos_data <- ECOS_multiple_filtered() |>
      group_by(Salt, Temp, filename) |>
      mutate(RH_lower = min(RH), RH_upper = max(RH))

    p <-
      ecos_data |>
      ggplot() +
      coord_cartesian(xlim = c(0, 40), ylim = c(15, 100)) +
      labs(x = "Temperature (°C)", y = "Humidity (%RH)") +
      theme_classic(base_size = 16) +
      geom_ribbon(aes(x = Temp, ymin = RH_lower, ymax = RH_upper, fill = Salt), alpha = 0.4)

    # Only add TRH points if TRHdata is available and not empty
    if (!is.null(trh) && nrow(trh) > 0) {
      p <- p +
        geom_point(data = trh, aes(x = TEMPERATURE, y = HUMIDITY), alpha = 0.7, colour = "plum")
    }
    return(p)
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
    req(worldmet_site_names())
    selectInput("select_worldmet_sites", "Select Station", multiple = FALSE,
                choices = worldmet_site_names(),
                selected = worldmet_site_names()[1])
  })

  worldmet_site_code <- reactive({
    worldmet_sites() |>
      filter(station == input$select_worldmet_sites) |>
      pull(code)
  })

  output$worldmet_siteTable <- renderTable({
    req(worldmet_sites())
    worldmet_sites()
  })

  output$worldmet_leafletmap <- renderLeaflet({
    req(worldmet_site_names())
    worldmet::getMeta(
      plot = TRUE,
      site = c(worldmet_site_names()),
      lat = input$select_lat, lon = input$select_lon
    )
  })

  worldmet_data <- reactive({
    worldmet::importNOAA(
      code = worldmet_site_code(),
      year = lubridate::year(input$select_worldmet_year))
  })

  output$locationdata_rawDownload <- downloadHandler(
    filename = function() { "worldmet_data.csv" },
    content = function(file) {
      readr::write_excel_csv(worldmet_data(), file)
    }
  )



}
