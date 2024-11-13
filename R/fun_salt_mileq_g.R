#' Milliequivalents per gram (mEq/g)
#'
#' @description
#' Calculate initial amount of substance per ion (Eq/g). Use fun_salt_mileq for charge balance.
#'
#'
#' @details
#' Eqn 2. Equivalents per gram (e_i_g).
#'
#'
#' @param salt_ppm Ion concentration in ppm
#' @param dry_g Sample weight, g
#' @param water_ml Amount of water, ml
#' @param mol_wts Molecular weight. Divide mol_wt by 2 if sulfate (48.03) and calcium (20.04)
#'
#' @return salt_mileq_g, milliequivalents per gram (mEq/g)
#' @export
#'
#' @examples
#' # Chloride test data
#' fun_salt_mileq_g(66.824, 1.128, 100, 35.453)
#'
fun_salt_mileq_g <- function(salt_ppm, dry_g, water_ml, mol_wts) {
  salt_mileq_g <-
    (salt_ppm * water_ml) / (1000 * dry_g * mol_wts)
  return(salt_mileq_g)
}
