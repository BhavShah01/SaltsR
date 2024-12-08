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
#'
fun_salt_mol <- function(salt_ppm, water_ml, mol_wts, salt_charges_z) {
  salt_mol <-
    (0.001 * salt_ppm * water_ml) / (mol_wts / salt_charges_z)
  return(salt_mol)
}
