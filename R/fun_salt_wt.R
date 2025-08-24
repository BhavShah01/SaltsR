#' Ion content in the sample (weight \%)
#'
#' @description
#' Calculate the weight fraction of each individual ion in the sample (w\%),
#' based on ion concentration (ppm or mg/L), volume of water (mL),
#' and the dry mass of the sample (g).
#'
#' @details
#' Eqn 1. Weight fraction (w_i), as preparation for charge balance equations.
#'
#' The calculation is:
#' \deqn{
#' w_i = \frac{C_{\mathrm{ppm}} \times V_{\mathrm{water}}}{10000 \times m_{\mathrm{dry}}}
#' }
#' where:
#' \itemize{
#'   \item \eqn{C_{\mathrm{ppm}}}: ion concentration in ppm or mg/L
#'   \item \eqn{V_{\mathrm{water}}}: volume of water in millilitres
#'   \item \eqn{m_{\mathrm{dry}}}: dry mass of the sample in grams
#' }
#'
#' @param salt_ppm Ion concentration in ppm or mg/L
#' @param dry_g Dry sample mass in grams
#' @param water_ml Volume of water in millilitres
#'
#' @return Numeric value for ion content in the sample expressed as weight percentage (w\%)
#' @export
#'
#' @examples
#' # Chloride test data with scalar inputs
#' fun_salt_wt(66.824, 1.128, 100)
#'
#' # Vectorized usage with salt_test and mol_wts data frames/lists
#' fun_salt_wt(
#'   salt_test$chloride_ppm,
#'   salt_test$dry_g,
#'   salt_test$water_ml
#' )
#'
fun_salt_wt <- function(salt_ppm, dry_g, water_ml) {
  salt_wt <-
    (salt_ppm * water_ml) / (10000 * dry_g)
  return(salt_wt)
}
