# inst/SaltsR_app/app.R

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tibble)
library(DT)


server <- function(input, output) {
  # bs_themer()

  output$sel_sample <- renderUI({
    textInput("sel_sample", "Sample name", value = "Name to link to metadata")
  })
  output$sel_dry_g <- renderUI({
    numericInput("sel_dry_g", "Sample (g)", value = 0.801, min = 0)
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

  salts_results <- reactive({
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

  output$salts_x_ECOS <- renderDT({
    salts_results() |>
      select(ends_with("_x_ECOS")) |>
      DT::datatable(extensions = c("Buttons"),
                    list(dom = 'tB', buttons = list("copy", "excel")))
  })
  output$salts_wt_ECOS <- renderDT({
    salts_results() |>
      select(ends_with("_wt_ECOS")) |>
      DT::datatable(extensions = c("Buttons"),
                    list(dom = 'tB', buttons = list("copy", "excel")))
  })

  output$salts_pathway <- renderText({
    salts_results() |> pull(ECOS_pathway)
  })
  output$salts_warning <- renderText({
    salts_results() |> pull(ECOS_warnings)
  })

}
