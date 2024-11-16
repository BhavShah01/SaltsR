#' Molar concentration, mols
#'
#' @description
#' Calculate ion values as mole fraction (mols)
#'
#'
#' @details
#' Eqn
#'
#'
#' @param salt_ppm Ion concentration in ppm
#' @param water_ml Amount of water, ml
#' @param mol_wts Molecular weight
#' @param salt_charges_z Salt charge, z
#'
#' @return salt_mol Molar concentration, mols
#' @export
#'
#' @examples
#' # Chloride test data
#' fun_salt_mol(66.824, 100, 35.453, 1)
#'
#' \dontrun{
#' salt_test |> fun_salt_mol(chloride_ppm, mol_wts$chloride, salt_charges_z$chloride)
#'
#' fun_salt_mol(14.651, 0.801, 100, mol_wts$chloride, salt_charges_z$chloride)
#' fun_salt_mol(17.339, 0.801, 100, mol_wts$nitrate, salt_charges_z$nitrate)
#' fun_salt_mol(39.923, 0.801, 100, mol_wts$sulfate, salt_charges_z$sulfate)
#' }
#'
#'
fun_salt_mol <- function(salt_ppm, water_ml, mol_wts, salt_charges_z) {
  salt_mol <-
    (0.001 * salt_ppm * water_ml) / (mol_wts / salt_charges_z)
  return(salt_mol)
}
