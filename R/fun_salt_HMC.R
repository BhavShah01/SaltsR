#' Hygroscopic Moisture Content
#'
#' @description
#' Calculate the Hygroscopic Moisture Content from sample measurement data
#'
#'
#'
#'
#'
#' @param sample_container_mass Sample container Mass, m_{c} (g)
#' @param dry_sample_container_mass Dry sample including container Mass, m_{dcs} (g)
#' @param wet_sample_container_mass Sample mass including container at 95\%RH and 20C, m_{cs95} (g)
#'
#' @return HMC, Hygroscopic Moisture Content (wt \%)
#' @export
#'
#' @examples
#' fun_salt_HMC(8.45, 15.198, 15.837)
#'
#'
fun_salt_HMC <- function(
    sample_container_mass,
    dry_sample_container_mass,
    wet_sample_container_mass) {

  # Hygroscopic Moisture Content (wt %)
  # wet_sample_container_mass, Sample mass including container at 95%RH and 20C
  HMC =
    (wet_sample_container_mass - dry_sample_container_mass) /
    (dry_sample_container_mass - sample_container_mass)
  HMC = HMC * 100
  return(HMC)
}
