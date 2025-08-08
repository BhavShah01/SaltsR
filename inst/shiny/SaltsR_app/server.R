# inst/SaltsR_app/app.R

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tibble)
library(tidyr)
library(readr)
library(DT)
library(SaltsR)


server <- function(input, output) {
  # bs_themer()

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

  ECOS_results <- reactive({
    salts_results() |>
      select(ends_with("_ECOS_mol")) |> # ends_with("_ECOS_weight"),
      pivot_longer(cols = everything(), names_to = c("salt", "ECOS"), names_sep = "_ECOS_") |>
      pivot_wider(names_from = "salt", values_from = "value")
  })

  output$ECOS_table <- renderDT({
    ECOS_results() |>
      column_to_rownames("ECOS") |>
      t() |>
      DT::datatable(extensions = c("Buttons"),
                    list(dom = 'tB', buttons = list("copy")))
  })

  output$salts_messages <- renderText({
    pathway = salts_results() |> pull(ECOS_pathway)
    warnings = salts_results() |> pull(ECOS_warnings)
    paste(pathway, warnings, sep = ". ")
  })

  output$salts_download <- downloadHandler(
    filename = "salts_results.csv",
    content = function(file) {
      readr::write_excel_csv(salts_results(), file)
    }
  )

  output$salts_results_graph <- renderPlot({
    salts_results() |>
      ggplot() +
      geom_col(aes("Sodium original", sodium_wt), fill = "darkred", alpha = 0.3) +
      geom_col(aes("Sodium ECOS", sodium_ECOS_weight), fill = "darkred", alpha = 0.7) +
      geom_col(aes("Potassium original", potassium_wt), fill = "darkgreen", alpha = 0.3) +
      geom_col(aes("Potassium ECOS", potassium_ECOS_weight), fill = "darkgreen", alpha = 0.7) +
      geom_col(aes("Magnesium original", magnesium_wt), fill = "blue", alpha = 0.3) +
      geom_col(aes("Magnesium ECOS", magnesium_ECOS_weight), fill = "blue", alpha = 0.7) +
      geom_col(aes("Calcium original", calcium_wt), fill = "gold", alpha = 0.3) +
      geom_col(aes("Calcium ECOS", calcium_ECOS_weight), fill = "gold", alpha = 0.7) +
      geom_col(aes("Chloride original", chloride_wt), fill = "orange", alpha = 0.3) +
      geom_col(aes("Chloride ECOS", chloride_ECOS_weight), fill = "orange", alpha = 0.7) +
      geom_col(aes("Nitrate original", nitrate_wt), fill = "purple", alpha = 0.3) +
      geom_col(aes("Nitrate ECOS", nitrate_ECOS_weight), fill = "purple", alpha = 0.7) +
      geom_col(aes("Sulfate original", sulfate_wt), fill = "hotpink", alpha = 0.3) +
      geom_col(aes("Sulfate ECOS", sulfate_ECOS_weight), fill = "hotpink", alpha = 0.7) +
      labs(x = NULL, y = "Weight") +
      coord_flip() +
      theme_classic()
  })

}
