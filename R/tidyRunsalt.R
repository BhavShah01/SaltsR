#' Read and tidy Runsalt output data
#'
#' @description
#' This function reads a Runsalt output file, processes it, and returns a tidy dataframe.
#' It separates salt and variable information, pivots the data, and adds columns for
#' relative humidity (RH) and moles (mol).
#'
#'
#' @param file_path A character string specifying the path to the Runsalt output file.
#'
#' @return A tibble with columns:
#'
#' \itemize{
#'   \item Salt The type of salt
#'   \item RH Relative Humidity
#'   \item mol Moles of salt
#'   \item filename The name of the input file
#' }
#'
#' @export
#'
#' @importFrom readr read_table
#' @importFrom tidyr pivot_longer separate drop_na pivot_wider unnest
#' @importFrom dplyr mutate select
#'
#' @examples
#'
#' \dontrun{
#' result <- tidyRunsalt("path/to/runsalt/output.txt")
#' head(result)
#' }
#'
#'
#'
tidyRunsalt <- function(file_path) {
  FILENAME = basename(file_path)

  runsalt_output <-
    read_table(file_path, col_names = FALSE) |>
    pivot_longer(-1) |>
    separate(X1, sep = "_", into = c("Salt", "Variable")) |>
    drop_na(value) |>
    pivot_wider(names_from = Variable, values_from = value) |>
    unnest(c(X, Y)) |>
    mutate(filename = FILENAME, RH = X, mol = Y) |>
    select(-X, -Y)

  return(runsalt_output)
}
