#' Graph the output from Runsalt
#'
#' @description
#' This function generates a graph from the ECOS Runsalt model output data when temperature is held constant.
#'
#' This file is obtained via Runsalt plot menu: Plot > Export Plot Data...
#'
#'
#' @param ECOS_output A character string specifying the path to the Runsalt output file.
#'   This file is obtained via Runsalt plot menu: Plot > Export Plot Data...
#' @param Temp_value Numeric. Temperature in degrees Celsius. Default is 20.
#' @param add_crystal Logical. Whether to add crystallisation point labels to the graph. Default is TRUE.
#' @param add_eqm Logical. Whether to add equilibrium point labels to the graph. Default is FALSE.
#'
#' @seealso \code{\link{tidyRunsalt}} for tidying the Runsalt output before graphing.
#'
#' @return A ggplot2 object representing the Runsalt model output graph.
#' @export
#'
#' @importFrom ggplot2 ggplot geom_line geom_point lims labs aes theme_classic
#' @importFrom ggrepel geom_text_repel
#'
#' @examples
#' \dontrun{
#' # Graph Runsalt output file with default temperature (20 C)
#' graph_salt_balance("path/to/runsalt/output.txt")
#'
#' # Interactive file selection, custom temperature
#' graph_salt_balance(file.choose(), Temp_value = 15)
#'
#' # Graph with no crystallisation labels but equilibrium labels
#' graph_salt_balance(file.choose(), Temp_value = 15, add_crystal = FALSE, add_eqm = TRUE)
#' }
graph_salt_balance <- function(ECOS_output, Temp_value = 20, add_crystal = TRUE, add_eqm = FALSE) {

  data <- ECOS_output |>
    tidyRunsalt()

  p <-
    ggplot(data, aes(X, Y, col = Salt, fill = Salt)) +
    geom_line(alpha = 0.7, size = 1.5) +
    geom_point(alpha = 0.7, size = 1) +
    labs(
      x = "Humidity (%RH)",
      y = "Amount of substance (mol)",
      title = "ECOS model output",
      subtitle = paste0("Temperature ", Temp_value, "C"),
      caption = "Price (2000) and Bionda (2005)"
    ) +
    theme_classic(base_size = 16)

  if (add_crystal) {
    p <- p + ggrepel::geom_text_repel(
      aes(label = Crystallisation),
      na.rm = TRUE
    )
  }

  if (add_eqm) {
    p <- p + ggrepel::geom_text_repel(
      aes(label = RH_eqm),
      na.rm = TRUE
    )
  }

  return(p)
}
