#' Read and tidy Runsalt output data
#'
#' @description
#' Reads a Runsalt output file and returns a tidy tibble.
#' The function processes the data by separating salt and variable information,
#' pivoting it to long format, dropping missing values, and adding columns for
#' relative humidity (RH), moles (mol), temperature, filename, and additional
#' crystallisation and equilibrium indicators.
#'
#' @param ECOS_output A character string specifying the path to the Runsalt output file.
#'   This file is typically obtained via Runsalt plot menu: Plot > Export Plot Data...
#' @param Temp_value Numeric. Temperature in degrees Celsius. Default is 20.
#'
#' @return A tibble with the columns:
#' \itemize{
#'   \item \strong{Salt}: Salt type (character).
#'   \item \strong{RH}: Relative Humidity (numeric).
#'   \item \strong{mol}: Moles of salt (numeric).
#'   \item \strong{Temp}: Temperature value (numeric).
#'   \item \strong{filename}: Input file name (character).
#'   \item \strong{Crystallisation}: Relative Humidity value at crystallisation point (numeric; NA otherwise).
#'   \item \strong{RH_eqm}: Relative Humidity value at equilibrium points (numeric or empty string).
#' }
#'
#' @export
#'
#' @importFrom readr read_table
#' @importFrom tidyr pivot_longer separate drop_na pivot_wider unnest
#' @importFrom dplyr mutate group_by lag select
#'
#' @examples
#' \dontrun{
#' # Read Runsalt output file and tidy data
#' result <- tidyRunsalt("path/to/runsalt/output.txt")
#' head(result)
#'
#' # Interactive file selection with custom temperature
#' tidyRunsalt(file.choose(), Temp_value = 15)
#' }
tidyRunsalt <- function(ECOS_output, Temp_value = 20) {
  FILENAME <- basename(ECOS_output)

  runsalt_output <-
    readr::read_table(ECOS_output, col_names = FALSE) |>
    tidyr::pivot_longer(-1) |>
    tidyr::separate(X1, sep = "_", into = c("Salt", "Variable")) |>
    tidyr::drop_na(value) |>
    tidyr::pivot_wider(names_from = Variable, values_from = value) |>
    tidyr::unnest(c(X, Y)) |>
    dplyr::mutate(
      Temp = Temp_value,
      RH = X,
      mol = Y,
      filename = FILENAME
    ) |>
    dplyr::group_by(Salt) |>
    dplyr::mutate(
      Crystallisation = ifelse(X == max(X, na.rm = TRUE), X, NA),
      diffY = Y - dplyr::lag(Y, 1),
      diffY2 = diffY - dplyr::lag(diffY, 1),
      RH_eqm = ifelse(diffY2 > 0, dplyr::lag(X, 1), ""),
      RH_eqm = ifelse(is.na(diffY2), NA, RH_eqm)
    ) |>
    dplyr::select(-diffY, -diffY2)

  return(runsalt_output)
}
