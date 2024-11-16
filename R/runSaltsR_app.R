#' Run SaltsR Shiny Application
#'
#' @param example
#'
#' @return Shiny object
#' @export
#'
#' @examples
#' # runSaltsR_app()
#'
runSaltsR_app <- function(example) {
  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("SaltsR_app", package = "SaltsR"))

  validExamplesMsg <-
    paste0(
      "Valid examples are: '",
      paste(validExamples, collapse = "', '"),
      "'")

  # if an invalid example is given, throw an error
  if (missing(example) || !nzchar(example) ||
      !example %in% validExamples) {
    stop(
      'Please run `runSaltsR_app()` with a valid example app as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }

  # find and launch the app
  appDir <- system.file("SaltsR_app", example, package = "SaltsR")
  shiny::runApp(appDir, display.mode = "normal")
}
