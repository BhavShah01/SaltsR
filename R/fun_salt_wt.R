#' Ion content in the sample (w\%)
#'
#' @description
#' Calculate weight fraction of each individual ion from concentration (ppm or mg/L),
#' volume of water (mL) and the dry mass of the sample (g).
#'
#'
#' @details
#' Eqn 1. Weight fraction (w_i), preparation for charge balance equations.
#'
#'
#' @param salt_ppm Ion concentration in ppm or mg/L
#' @param dry_g Sample weight, g
#' @param water_ml Amount of water, mL
#'
#' @return salt_wt Ion content in the sample (w\%)
#' @export
#'
#' @examples
#' # Chloride test data
#' fun_salt_wt(66.824, 1.128, 100)
#'
#' \dontrun{
#' salt_test |> fun_salt_wt(chloride_ppm, dry_g, water_ml)
#' }
#'
fun_salt_wt <- function(salt_ppm, dry_g, water_ml) {
  salt_wt <-
    (salt_ppm * water_ml) / (10000 * dry_g)
  return(salt_wt)
}
