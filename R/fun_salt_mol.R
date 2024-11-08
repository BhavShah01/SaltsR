#' Molar concentration, mols
#'
#' @param salt_ppm Ion concentration in ppm
#' @param water_ml Amount of water, ml
#' @param mol_wts Molecular weight. Divide mol_wt by 2 if sulfate (48.03) and calcium (20.04)
#'
#' @return salt_mol Molar concentration, mols
#' @export
#'
#' @examples
#'
#'
fun_salt_mol <- function(salt_ppm, water_ml, mol_wts) {
  salt_mol <- (0.001 * salt_ppm * water_ml) / mol_wts
  return(salt_mol)
}
