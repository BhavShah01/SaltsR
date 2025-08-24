#' Milliequivalents per kilogram (mEq/kg)
#'
#' @description
#' Calculate the initial amount of substance per ion (Eq/kg).
#'
#' @details
#' Eqn 2. Equivalents per kilogram (e_i):
#'
#' \deqn{ e_i = \frac{C_{\mathrm{ppm}} \times V_{\mathrm{water}}}
#' {m_{\mathrm{dry}} \times \left(\frac{M}{z}\right)} }
#'
#' where:
#' \itemize{
#'   \item \eqn{C_{\mathrm{ppm}}} = ion concentration in ppm
#'   \item \eqn{V_{\mathrm{water}}} = water volume (mL)
#'   \item \eqn{m_{\mathrm{dry}}} = sample dry mass (g)
#'   \item \eqn{M} = molecular weight
#'   \item \eqn{z} = ionic charge
#' }
#'
#' @param salt_ppm Ion concentration in ppm
#' @param dry_g Sample weight in grams
#' @param water_ml Amount of water in millilitres
#' @param mol_wts Molecular weight
#' @param salt_charges_z Salt charge, z
#'
#' @return Numeric milliequivalents per kilogram (mEq/kg)
#' @export
#'
#' @examples
#' # Chloride test data with scalar inputs
#' fun_salt_mileq(66.824, 1.128, 100, 35.453, 1)
#'
#' # Vectorized usage with salt_test and mol_wts data frames/lists
#' fun_salt_mileq(
#'   salt_test$chloride_ppm,
#'   salt_test$dry_g,
#'   salt_test$water_ml,
#'   mol_wts$chloride,
#'   salt_charges_z$chloride
#' )
fun_salt_mileq <- function(salt_ppm, dry_g, water_ml, mol_wts, salt_charges_z) {
  salt_mileq <-
    (salt_ppm * water_ml) / (dry_g * (mol_wts / salt_charges_z))
  return(salt_mileq)
}
