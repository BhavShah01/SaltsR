#' Milliequivalents per kilogram (mEq/kg)
#'
#' @description
#' Calculate initial amount of substance per ion (Eq/kg)
#'
#'
#' @details
#' Eqn 2. Equivalents per kilogram (e_i).
#'
#'
#' @param salt_ppm Ion concentration in ppm
#' @param dry_g Sample weight, g
#' @param water_ml Amount of water, ml
#' @param mol_wts Molecular weight
#' @param salt_charges_z Salt charge, z
#'
#' @return salt_mileq, milliequivalents per kilogram (mEq/kg)
#' @export
#'
#' @examples
#' # Chloride test data
#' fun_salt_mileq(66.824, 1.128, 100, 35.453, 1)
#'
#' \dontrun{
#' salt_test |> fun_salt_mileq(chloride_ppm, dry_g, water_ml, mol_wts = mol_wts$chloride, salt_charges_z = salt_charges_z$chloride)
#'
#' fun_salt_mileq(39.923, 0.801, 100, mol_wts$sulfate, salt_charges_z$sulfate)
#' }
#'
#'
fun_salt_mileq <- function(salt_ppm, dry_g, water_ml, mol_wts, salt_charges_z) {
  salt_mileq <-
    (salt_ppm * water_ml) / (dry_g * (mol_wts / salt_charges_z))
  return(salt_mileq)
}
