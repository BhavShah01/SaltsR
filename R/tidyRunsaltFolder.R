#' Process multiple Runsalt output files in a folder
#'
#' @description
#' This function reads all Runsalt output files in a specified folder,
#' processes them, and returns a combined tidy dataframe. It applies the
#' tidyRunsalt logic to each file, handling potential errors, and then
#' row-binds all results into a single dataframe.
#'
#'
#' @param folder_path folder_path A character string specifying the path to the folder
#' @param X_col A character string specifying the X column variable. Default is "RH".
#' @param Y_col A character string specifying the Y column variable. Default is "mol".
#' @param pattern A character string specifying the file pattern to match.
#'
#' @return A tibble with columns:
#'
#' \itemize{
#'   \item Salt The type of salt
#'   \item RH Relative Humidity (or the specified X_col)
#'   \item mol Moles of salt (or the specified Y_col)
#'   \item filename The name of the input file
#' }
#'
#' @export
#'
#' @importFrom readr read_table
#' @importFrom tidyr pivot_longer separate drop_na pivot_wider unnest
#' @importFrom dplyr mutate select bind_rows
#'
#' @examples
#' \dontrun{
#' result <- tidyRunsaltFolder("path/to/folder/with/runsalt/outputs")
#' head(result)
#' }
#'
#'
#'
tidyRunsaltFolder <- function(folder_path, X_col = "RH", Y_col = "mol", pattern = "\\.txt$") {
  # List all files in the folder matching the pattern
  file_list <- list.files(path = folder_path, pattern = pattern, full.names = TRUE)

  # Check if any files were found
  if (length(file_list) == 0) {
    stop("No matching files found in the specified folder")
  }

  # Process each file and combine results
  combined_data <- lapply(file_list, function(file_path) {
    FILENAME <- basename(file_path)

    tryCatch({
      runsalt_output <-
        read_table(file_path, col_names = FALSE) |>
        pivot_longer(-1) |>
        separate(X1, sep = "_", into = c("Salt", "Variable")) |>
        drop_na(value) |>
        pivot_wider(names_from = Variable, values_from = value) |>
        unnest(c(X, Y)) |>
        mutate(filename = FILENAME, !!X_col := X, !!Y_col := Y) |>
        select(-X, -Y)

      return(runsalt_output)
    }, error = function(e) {
      warning(paste("Error processing file:", file_path, "-", e$message))
      return(NULL)
    })
  }) |>
    bind_rows()

  return(combined_data)
}
