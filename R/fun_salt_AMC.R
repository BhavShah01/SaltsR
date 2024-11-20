#' Actual Moisture Content
#'
#' @description
#' Calculate the Actual Moisture Content from sample measurement data
#'
#'
#'
#'
#'
#' @param sample_container_mass Sample container Mass, m_c (g)
#' @param initial_sample_container_mass Initilial sample  including container Mass, m_ics (g)
#' @param dry_sample_container_mass Dry sample including container Mass, m_dcs (g)
#'
#' @return AMC, Actual Moisture Content (wt \%)
#' @export
#'
#' @examples
#' fun_salt_AMC(8.45, 15.619, 15.198)
#'
#'
fun_salt_AMC <- function(
    sample_container_mass,
    initial_sample_container_mass,
    dry_sample_container_mass) {

  # Actual Moisture Content (wt %)
  AMC =
    (initial_sample_container_mass - dry_sample_container_mass) /
    (dry_sample_container_mass - sample_container_mass)
  AMC = AMC * 100
  return(AMC)
}
