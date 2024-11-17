#' Run SaltsR Shiny Application
#'
#' @description
#' Run this application to test the functions and perform a salt balance
#'
#'
#' @param example
#'
#' @return Shiny object
#' @export
#'
#' @examples
#' \dontrun{
#' runSaltsR_app()
#' }
#'
#'
runSaltsR_app <- function() {
    app_dir <- system.file("shiny", "SaltsR_app", package = "SaltsR")
    if (app_dir == "") {
      stop("Could not find example directory. Try re-installing SaltsR from Github", call. = FALSE)
    }

    shiny::runApp(app_dir, display.mode = "normal")
  }
