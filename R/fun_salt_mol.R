#' Molar concentration (mols)
#'
#' @description
#' Calculate ion values expressed as mole fraction (mols).
#'
#' @details
#' Calculates the molar concentration from the ion concentration in ppm,
#' volume of water, molecular weight, and ionic charge.
#'
#' The calculation used is:
#' \deqn{
#' \text{mols} = \frac{0.001 \times C_{\mathrm{ppm}} \times V_{\mathrm{water}}}
#'                {\frac{M}{z}}
#' }
#' where:
#' \itemize{
#'   \item \eqn{C_{\mathrm{ppm}}}: ion concentration in ppm
#'   \item \eqn{V_{\mathrm{water}}}: volume of water in millilitres
#'   \item \eqn{M}: molecular weight
#'   \item \eqn{z}: ionic charge
#' }
#'
#' @param salt_ppm Ion concentration in ppm
#' @param water_ml Amount of water in millilitres
#' @param mol_wts Molecular weight
#' @param salt_charges_z Ionic charge of the salt
#'
#' @return Numeric value giving the molar concentration (mols)
#' @export
#'
#' @examples
#' # Chloride test data with scalar inputs
#' fun_salt_mol(66.824, 100, 35.453, 1)
#'
#' # Vectorized usage with salt_test and mol_wts data frames/lists
#' fun_salt_mol(
#'   salt_test$chloride_ppm,
#'   salt_test$water_ml,
#'   mol_wts$chloride,
#'   salt_charges_z$chloride
#' )
#'
fun_salt_mol <- function(salt_ppm, water_ml, mol_wts, salt_charges_z) {
  salt_mol <-
    (0.001 * salt_ppm * water_ml) / (mol_wts / salt_charges_z)
  return(salt_mol)
}
