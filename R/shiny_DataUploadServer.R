#' Shiny Module Server for Data Upload and Processing from Runsalt output
#'
#' @description
#' This function creates a Shiny module server for uploading .txt files from Runsalt
#'
#'
#' @param id A character string that corresponds to the ID used in the UI function for this module.
#'
#' @returns A reactive expression containing the tidied data frame'
#' @export
#'
#' @import shiny
#' @importFrom readr read_table
#' @importFrom SaltsR tidyRunsalt
#'
#' @examples
#' \dontrun{
#' # In a Shiny app:
#' ui <- fluidPage(
#'   shiny_dataUploadUI("dataUpload")
#'  )
#'
#'  server <- function(input, output, session) {
#'    data <- shiny_dataUploadServer("dataUpload")
#'    }
#'
#' }
#'
#'
#'
shiny_dataUploadServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # UI element: File input control
    output$file_upload <- renderUI({
      fileInput(session$ns("file"), "Choose Runsalt Output File",
                accept = ".txt")
    })

    # Reactive expression that processes the uploaded file and returns tidy data
    tidied_data <- reactive({
      req(input$file)

      # Defensive checks
      if (!file.exists(input$file$datapath)) {
        showNotification("Invalid file path. Please re-upload your .txt file.", type = "error")
        return(NULL)
      }

      file_ext <- tools::file_ext(input$file$name)
      validate(need(tolower(file_ext) == "txt", "Please upload a .txt Runsalt output file."))

      # Attempt to read and tidy the Runsalt output
      tryCatch({
        SaltsR::tidyRunsalt(input$file$datapath)
      }, error = function(e) {
        showNotification(
          paste("Error reading file:", e$message,
                ". Ensure the file follows ECOS Runsalt .txt format."),
          type = "error"
        )
        return(NULL)
      })
    })

    # Return the reactive data frame
    tidied_data
  })
}
