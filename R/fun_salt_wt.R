#' Ion content in the sample (w\%)
#'
#' @param salt_ppm Ion concentration in ppm
#' @param dry_g Sample weight, g
#' @param water_ml Amount of water, ml
#'
#' @return salt_wt Ion content in the sample
#' @export
#'
#' @examples
fun_salt_wt <- function(salt_ppm, dry_g, water_ml) {
  salt_wt <- (salt_ppm * water_ml) / (10000 * dry_g)
  return(salt_wt)
}
