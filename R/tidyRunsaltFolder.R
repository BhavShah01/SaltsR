#' Process multiple Runsalt output files in a folder
#'
#' @description
#' Reads all Runsalt output files in a specified folder, applies the
#' tidyRunsalt logic to each file, and combines the results into a single tidy dataframe.
#' Handles potential errors gracefully and binds all results row-wise for analysis.
#'
#' @param folder_path Character. Path to the folder containing Runsalt output files.
#' @param pattern Character. Regex pattern to match output files (default is "\\.txt$").
#' @param Temp_value Numeric. Temperature in degrees Celsius (default is 20).
#'
#' @return A tibble with columns:
#' \itemize{
#'   \item Salt The type of salt
#'   \item RH Relative Humidity
#'   \item mol Moles of salt
#'   \item filename Name of the input file
#'   \item Temp Temperature
#'   \item Crystallisation Crystallisation point label
#'   \item RH_eqm Equilibrium RH label
#' }
#' Additional columns may be present depending on processing.
#'
#' @export
#'
#' @importFrom readr read_table
#' @importFrom tidyr pivot_longer separate drop_na pivot_wider unnest
#' @importFrom dplyr mutate select bind_rows group_by lag
#'
#' @examples
#' \dontrun{
#' # Combine and tidy all Runsalt output files from a folder
#' combined <- tidyRunsaltFolder("path/to/folder/with/runsalt/files")
#' head(combined)
#' }
tidyRunsaltFolder <- function(folder_path, pattern = "\\.txt$", Temp_value = 20) {
  file_list <- list.files(path = folder_path, pattern = pattern, full.names = TRUE)

  if (length(file_list) == 0) {
    stop("No matching files found in the specified folder")
  }

  combined_data <- lapply(file_list, function(file_path) {
    FILENAME <- basename(file_path)
    tryCatch({
      runsalt_output <-
        readr::read_table(file_path, col_names = FALSE) |>
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
    }, error = function(e) {
      warning(paste("Error processing file:", file_path, "-", e$message))
      return(NULL)
    })
  }) |>
    dplyr::bind_rows()

  return(combined_data)
}
