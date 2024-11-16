#' Calculating salt balance from ion chromatography data (now deprecated for updated version)
#'
#' @description
#' Performs a salt balance on ion chromatography data and returns outputs for ECOS Runsalt software.
#'
#' This function is now deprecated and is being replaced with a new function.
#'
#'
#' @param salt_input Dataframe containing ion chromatography data
#' @param sample_name Sample name
#' @param dry_g Dry mass of sample, g
#' @param water_ml Water added for ion chromatography, ml
#' @param chloride_ppm Chloride concentration, ppm
#' @param nitrate_ppm Nitrate concentration, ppm
#' @param sulfate_ppm Sulfate concentration, ppm
#' @param sodium_ppm Sodium concentration, ppm
#' @param potassium_ppm Potassium concentration, ppm
#' @param calcium_ppm Calcium concentration, ppm
#' @param magnesium_ppm Magnesium concentration, ppm
#'
#' @return Dataframe of balanced ions for ECOS Runsalt software
#' @export
#'
#' @importFrom dplyr mutate
#'
#' @examples
#' salt_test |> fun_salt_calcs_old()
#'
#'
fun_salt_calcs_old <- function(
    salt_input,
    sample_name,
    dry_g,
    water_ml,
    chloride_ppm,
    nitrate_ppm,
    sulfate_ppm,
    sodium_ppm,
    potassium_ppm,
    calcium_ppm,
    magnesium_ppm) {
  #
  salt_input |>
    mutate(
      sample_name   = sample_name,
      dry_g = dry_g,
      water_ml  = water_ml,
      chloride_ppm  = chloride_ppm,
      nitrate_ppm   = nitrate_ppm,
      sulfate_ppm   = sulfate_ppm,
      sodium_ppm    = sodium_ppm,
      potassium_ppm = potassium_ppm,
      calcium_ppm   = calcium_ppm,
      magnesium_ppm = magnesium_ppm,
      # chloride_wt, nitrate_wt, sulfate_wt, sodium_wt, potassium_wt, calcium_wt, magnesium_wt, total_salt_wt
      Chloride_wt = fun_salt_wt(chloride_ppm, dry_g, water_ml),
      Nitrate_wt = fun_salt_wt(nitrate_ppm, dry_g, water_ml),
      Sulfate_wt = fun_salt_wt(sulfate_ppm, dry_g, water_ml),
      Sodium_wt = fun_salt_wt(sodium_ppm, dry_g, water_ml),
      Potassium_wt = fun_salt_wt(potassium_ppm, dry_g, water_ml),
      Calcium_wt = fun_salt_wt(calcium_ppm, dry_g, water_ml),
      Magnesium_wt = fun_salt_wt(magnesium_ppm, dry_g, water_ml),
      total_salt_wt =
        Chloride_wt + Nitrate_wt + Sulfate_wt +
        Sodium_wt + Potassium_wt + Calcium_wt + Magnesium_wt,
      #
      chloride_mEq = fun_salt_mileq(chloride_ppm, dry_g, water_ml, mol_wts$chloride),
      nitrate_mEq = fun_salt_mileq(nitrate_ppm, dry_g, water_ml, mol_wts$nitrate),
      sulfate_mEq = fun_salt_mileq(sulfate_ppm, dry_g, water_ml, mol_wts$sulfate),
      sodium_mEq = fun_salt_mileq(sodium_ppm, dry_g, water_ml, mol_wts$sodium),
      potassium_mEq = fun_salt_mileq(potassium_ppm, dry_g, water_ml, mol_wts$potassium),
      calcium_mEq = fun_salt_mileq(calcium_ppm, dry_g,  water_ml, mol_wts$calcium),
      magnesium_mEq = fun_salt_mileq(magnesium_ppm, dry_g, water_ml, mol_wts$magnesium),
      total_anions_mEq = chloride_mEq + nitrate_mEq + sulfate_mEq,
      total_cations_mEq = sodium_mEq + potassium_mEq + calcium_mEq + magnesium_mEq,
      xs_cations_mEq = total_cations_mEq - total_anions_mEq,
      ave_ions_mEq = (total_anions_mEq + total_cations_mEq) / 2,
      abs_xs_ions_mEq = abs(total_anions_mEq - total_cations_mEq),
      xs_CaSO4_mEq =
        ifelse(calcium_mEq ==
                 max(sodium_mEq, potassium_mEq, calcium_mEq, magnesium_mEq) &
                 (calcium_mEq >= abs_xs_ions_mEq) &
                 (total_anions_mEq < total_cations_mEq),
               TRUE, FALSE),
      analytical_uncert = ifelse(
        abs_xs_ions_mEq > 0.02 * (max(total_anions_mEq, total_cations_mEq)), TRUE, FALSE),
      #
      chloride_mEq_cor =
        ifelse(analytical_uncert == TRUE, chloride_mEq,
               chloride_mEq * abs_xs_ions_mEq / total_anions_mEq),
      nitrate_mEq_cor =
        ifelse(analytical_uncert == TRUE, nitrate_mEq,
               nitrate_mEq * abs_xs_ions_mEq / total_anions_mEq),
      sulfate_mEq_cor =
        ifelse(analytical_uncert == TRUE, sulfate_mEq,
               sulfate_mEq * abs_xs_ions_mEq / total_anions_mEq),
      sodium_mEq_cor =
        ifelse(analytical_uncert == TRUE, sodium_mEq,
               sodium_mEq * abs_xs_ions_mEq / total_anions_mEq),
      potassium_mEq_cor =
        ifelse(analytical_uncert == TRUE, potassium_mEq,
               potassium_mEq * abs_xs_ions_mEq / total_anions_mEq),
      calcium_mEq_cor =
        ifelse(analytical_uncert == TRUE, calcium_mEq - abs_xs_ions_mEq,
               calcium_mEq * ave_ions_mEq / total_cations_mEq),
      magnesium_mEq_cor =
        ifelse(analytical_uncert == TRUE, magnesium_mEq,
               magnesium_mEq * abs_xs_ions_mEq / total_anions_mEq),
      #
      ion_check_chloride =
        ifelse(total_anions_mEq > total_cations_mEq &
                 chloride_mEq_cor > nitrate_mEq_cor &
                 chloride_mEq_cor > sulfate_mEq_cor, TRUE, FALSE),
      ion_check_nitrate =
        ifelse(total_anions_mEq > total_cations_mEq &
                 nitrate_mEq_cor > chloride_mEq_cor &
                 nitrate_mEq_cor > sulfate_mEq_cor, TRUE, FALSE),
      ion_check_sulfate =
        ifelse(total_anions_mEq > total_cations_mEq &
                 sulfate_mEq_cor > chloride_mEq_cor &
                 sulfate_mEq_cor > nitrate_mEq_cor, TRUE, FALSE),
      ion_check_sodium =
        ifelse(total_cations_mEq > total_anions_mEq &
                 sodium_mEq_cor > potassium_mEq_cor &
                 sodium_mEq_cor > calcium_mEq_cor &
                 sodium_mEq_cor > magnesium_mEq_cor, TRUE, FALSE),
      ion_check_potassium =
        ifelse(total_cations_mEq > total_anions_mEq &
                 potassium_mEq_cor > sodium_mEq_cor &
                 potassium_mEq_cor > calcium_mEq_cor &
                 potassium_mEq_cor > magnesium_mEq_cor, TRUE, FALSE),
      ion_check_calcium =
        ifelse(total_cations_mEq > total_anions_mEq &
                 calcium_mEq_cor > sodium_mEq_cor &
                 calcium_mEq_cor > magnesium_mEq_cor &
                 calcium_mEq_cor > potassium_mEq_cor, TRUE, FALSE),
      ion_check_magnesium =
        ifelse(total_cations_mEq > total_anions_mEq &
                 magnesium_mEq_cor > sodium_mEq_cor &
                 magnesium_mEq_cor > calcium_mEq_cor &
                 magnesium_mEq_cor > potassium_mEq_cor, TRUE, FALSE),
      #
      chloride_mEq_SO4 =
        ifelse(total_anions_mEq > total_cations_mEq &
                 ave_ions_mEq > 0 & ion_check_chloride == TRUE,
               chloride_mEq_cor - ave_ions_mEq, chloride_mEq_cor),
      nitrate_mEq_SO4 =
        ifelse(total_anions_mEq > total_cations_mEq &
                 ave_ions_mEq > 0 & ion_check_nitrate == TRUE,
               nitrate_mEq_cor - ave_ions_mEq, nitrate_mEq_cor),
      sulfate_mEq_SO4 =
        ifelse(total_anions_mEq > total_cations_mEq &
                 ave_ions_mEq > 0 & ion_check_sulfate == TRUE,
               sulfate_mEq_cor - ave_ions_mEq, sulfate_mEq_cor),
      sodium_mEq_SO4 =
        ifelse(total_anions_mEq > total_cations_mEq &
                 ave_ions_mEq > 0 & ion_check_sodium == TRUE,
               sodium_mEq_cor - ave_ions_mEq, sodium_mEq_cor),
      potassium_mEq_SO4 =
        ifelse(total_anions_mEq > total_cations_mEq &
                 ave_ions_mEq > 0 & ion_check_potassium == TRUE,
               potassium_mEq_cor - ave_ions_mEq, potassium_mEq_cor),
      calcium_mEq_SO4 =
        ifelse(total_anions_mEq > total_cations_mEq &
                 ave_ions_mEq > 0 & ion_check_calcium == TRUE,
               calcium_mEq_cor - ave_ions_mEq, calcium_mEq_cor),
      magnesium_mEq_SO4 =
        ifelse(total_anions_mEq > total_cations_mEq &
                 ave_ions_mEq > 0 & ion_check_magnesium == TRUE,
               magnesium_mEq_cor - ave_ions_mEq, magnesium_mEq_cor),
      #
      total_anions_cor = chloride_mEq_SO4 + nitrate_mEq_SO4 + sulfate_mEq_SO4,
      total_cations_cor = sodium_mEq_SO4 + potassium_mEq_SO4 + calcium_mEq_SO4 + magnesium_mEq_SO4,
      abs_xs_ions_cor = abs(round(total_anions_cor - total_cations_cor, 6)),
      sulfate_mEq_SO4cor = sulfate_mEq_SO4 / (water_ml / (dry_g * (48.305))),
      sulfate_wt_cor = (sulfate_mEq_SO4cor * water_ml) / (10000 * dry_g),
      #
      chloride_mEq_corSO4 = chloride_mEq_SO4,
      nitrate_mEq_corSO4 = nitrate_mEq_SO4,
      sulfate_mEq_corSO4 = sulfate_mEq_SO4cor,
      sodium_mEq_corSO4 = sodium_mEq_SO4,
      potassium_mEq_corSO4 = potassium_mEq_SO4,
      calcium_mEq_corSO4 = calcium_mEq_SO4,
      magnesium_mEq_corSO4 = magnesium_mEq_SO4,
      #
      sulfate_diff = ifelse(calcium_mEq_SO4 > sulfate_mEq_SO4, 0, calcium_mEq_SO4 - sulfate_mEq_SO4),
      calcium_diff = ifelse(calcium_mEq_SO4 > sulfate_mEq_SO4, calcium_mEq_SO4 - sulfate_mEq_SO4, 0),
      theoretical_CaSO4 = ifelse(sulfate_mEq_SO4 == min(calcium_mEq_SO4, sulfate_mEq_SO4),
                                 sulfate_mEq_SO4 * 2, calcium_mEq_SO4 * 2),
      #
      sulfate_ppm_cor = sulfate_diff / (water_ml / (dry_g * mol_wts$sulfate)),
      calcium_ppm_cor = calcium_diff / (water_ml / (dry_g * mol_wts$calcium)),
      sulfate_mEq_corCaSO4 = sulfate_mEq_SO4 - sulfate_diff,
      calcium_mEq_corCaSO4 = calcium_mEq_SO4 - calcium_diff,
      sulfate_ppm_corCaSO4 = sulfate_mEq_corCaSO4 / (water_ml / (dry_g * mol_wts$sulfate)),
      calcium_ppm_corCaSO4 = calcium_mEq_corCaSO4 / (water_ml / (dry_g * mol_wts$calcium)),
      Gypsum_wt =
        (sulfate_ppm_corCaSO4 * water_ml) / (10000 * dry_g) +
        (calcium_ppm_corCaSO4 * water_ml) / (10000 * dry_g),
      Total_anions = round(chloride_mEq_corSO4 + nitrate_mEq_corSO4 + sulfate_diff, 6),
      Total_cations = round(sodium_mEq_corSO4 + potassium_mEq_corSO4 +
                              calcium_diff + magnesium_mEq_corSO4, 6),
      XS_ions = abs(Total_anions - Total_cations),
      #
      Chloride_ppm = chloride_mEq_corSO4 / (water_ml / (dry_g * mol_wts$chloride)),
      Nitrate_ppm = nitrate_mEq_corSO4 / (water_ml / (dry_g * mol_wts$nitrate)),
      Sulfate_ppm = sulfate_ppm_cor,
      # sulfate_mEq_corSO4 / (water_ml / (dry_g * mol_wts$sulfate)),
      Sodium_ppm = sodium_mEq_corSO4 / (water_ml / (dry_g * mol_wts$sodium)),
      Potassium_ppm = potassium_mEq_corSO4 / (water_ml / (dry_g * mol_wts$potassium)),
      Calcium_ppm = calcium_mEq_corSO4 / (water_ml / (dry_g * mol_wts$calcium)),
      Magnesium_ppm = magnesium_mEq_corSO4 / (water_ml / (dry_g * mol_wts$magnesium)),
      #
      chloride_eqkg_SO4 = chloride_mEq_SO4 / total_anions_cor * 100,
      nitrate_eqkg_SO4 = nitrate_mEq_SO4 / total_anions_cor * 100,
      sulfate_eqkg_SO4 = sulfate_mEq_SO4 / total_anions_cor * 100,
      sodium_eqkg_SO4 = sodium_mEq_SO4 / total_cations_cor * 100,
      potassium_eqkg_SO4 = potassium_mEq_SO4 / total_cations_cor * 100,
      calcium_eqkg_SO4 = calcium_mEq_SO4 / total_cations_cor * 100,
      magnesium_eqkg_SO4 = magnesium_mEq_SO4 / total_cations_cor * 100,
      #
      chloride_eqkg_CaSO4 = chloride_mEq_corSO4 / Total_anions * 100,
      nitrate_eqkg_CaSO4 = nitrate_mEq_corSO4 / Total_anions * 100,
      sulfate_eqkg_CaSO4 = sulfate_diff / Total_anions * 100, # Corrected
      sodium_eqkg_CaSO4 = sodium_mEq_corSO4 / Total_cations * 100,
      potassium_eqkg_CaSO4 = potassium_mEq_corSO4 / Total_cations * 100,
      calcium_eqkg_CaSO4 = calcium_diff / Total_cations * 100, # Corrected
      magnesium_eqkg_CaSO4 = magnesium_mEq_corSO4 / Total_cations * 100,
      #
      total_eqkg = sum(
        chloride_eqkg_CaSO4, nitrate_eqkg_CaSO4, sulfate_eqkg_CaSO4,
        sodium_eqkg_CaSO4, potassium_eqkg_CaSO4, calcium_eqkg_CaSO4,
        magnesium_eqkg_CaSO4),
      #
      Chloride_MMOLkg = chloride_mEq_corSO4,
      Nitrate_MMOLkg = nitrate_mEq_corSO4,
      Sulfate_MMOLkg = sulfate_diff / 2,
      Sodium_MMOLkg = sodium_mEq_corSO4,
      Potassium_MMOLkg = potassium_mEq_corSO4,
      Calcium_MMOLkg = calcium_diff / 2,
      Magnesium_MMOLkg = magnesium_mEq_corSO4 / 2,
      #
      total_anions_MMOLkg = Chloride_MMOLkg + Nitrate_MMOLkg + (Sulfate_MMOLkg * 2),
      total_cations_MMOLkg = Sodium_MMOLkg + Potassium_MMOLkg +
        (Calcium_MMOLkg * 2) + (Magnesium_MMOLkg * 2),
      diff_MMOLkg = round(total_anions_MMOLkg - total_cations_MMOLkg, 4),
      #
      Chloride_MOLkg =
        ifelse(total_anions_MMOLkg > total_cations_MMOLkg &
                 Chloride_MMOLkg == max(Chloride_MMOLkg, Nitrate_MMOLkg, Sulfate_MMOLkg),
               Chloride_MMOLkg - diff_MMOLkg, Chloride_MMOLkg) / 1000,
      Nitrate_MOLkg =
        ifelse(total_anions_MMOLkg > total_cations_MMOLkg &
                 Nitrate_MMOLkg == max(Chloride_MMOLkg, Nitrate_MMOLkg, Sulfate_MMOLkg),
               Nitrate_MMOLkg - diff_MMOLkg, Nitrate_MMOLkg) / 1000,
      Sulfate_MOLkg =
        ifelse(total_anions_MMOLkg > total_cations_MMOLkg &
                 Sulfate_MMOLkg == max(Chloride_MMOLkg, Nitrate_MMOLkg, Sulfate_MMOLkg),
               Sulfate_MMOLkg - diff_MMOLkg, Sulfate_MMOLkg) / 1000,
      Sodium_MOLkg =
        ifelse(total_cations_MMOLkg > total_anions_MMOLkg &
                 Sodium_MMOLkg == max(Sodium_MMOLkg, Potassium_MMOLkg,
                                      Calcium_MMOLkg, Magnesium_MMOLkg),
               Sodium_MMOLkg - diff_MMOLkg, Sodium_MMOLkg) / 1000,
      Potassium_MOLkg =
        ifelse(total_cations_MMOLkg > total_anions_MMOLkg &
                 Sodium_MMOLkg == max(Sodium_MMOLkg, Potassium_MMOLkg,
                                      Calcium_MMOLkg, Magnesium_MMOLkg),
               Potassium_MMOLkg - diff_MMOLkg, Potassium_MMOLkg) / 1000,
      Calcium_MOLkg =
        ifelse(total_cations_MMOLkg > total_anions_MMOLkg &
                 Calcium_MMOLkg == max(Sodium_MMOLkg, Potassium_MMOLkg,
                                       Calcium_MMOLkg, Magnesium_MMOLkg),
               Calcium_MMOLkg - diff_MMOLkg, Calcium_MMOLkg) / 1000,
      Magnesium_MOLkg =
        ifelse(total_cations_MMOLkg > total_anions_MMOLkg &
                 Magnesium_MMOLkg == max(Sodium_MMOLkg, Potassium_MMOLkg,
                                         Calcium_MMOLkg, Magnesium_MMOLkg),
               Magnesium_MMOLkg - diff_MMOLkg, Magnesium_MMOLkg) / 1000,
      #
      total_anions_MOLkg_total = Chloride_MOLkg + Nitrate_MOLkg + (Sulfate_MOLkg * 2),
      total_cations_MOLkg_total = Sodium_MOLkg + Potassium_MOLkg +
        (Calcium_MOLkg * 2) + (Magnesium_MOLkg * 2),
      diff_MOLkg_diff = round(total_anions_MOLkg_total - total_cations_MOLkg_total, 4),
      #
      Chloride_PPM = Chloride_ppm,
      Nitrate_PPM = Nitrate_ppm,
      Sulfate_PPM = sulfate_ppm_cor, # Corrected
      Sodium_PPM = Sodium_ppm,
      Potassium_PPM = Potassium_ppm,
      Calcium_PPM = calcium_ppm_cor, # Corrected
      Magnesium_PPM = Magnesium_ppm,
      #
      Chloride_Wt = ((Chloride_PPM * water_ml) / (10000 * dry_g)),
      Nitrate_Wt = ((Nitrate_PPM * water_ml) / (10000 * dry_g)),
      Sulfate_Wt = ((Sulfate_PPM * water_ml) / (10000 * dry_g)),
      Sodium_Wt = ((Sodium_PPM * water_ml) / (10000 * dry_g)),
      Potassium_Wt = ((Potassium_PPM * water_ml) / (10000 * dry_g)),
      Calcium_Wt = ((Calcium_PPM * water_ml) / (10000 * dry_g)),
      Magnesium_Wt = ((Magnesium_PPM * water_ml) / (10000 * dry_g)),
      #
      total_Wt = Chloride_Wt + Nitrate_Wt + Sulfate_Wt +
        Sodium_Wt + Potassium_Wt + Calcium_Wt + Magnesium_Wt,
      total_gypsum_Wt = total_salt_wt - Gypsum_wt,
      total_diff_Wt = total_salt_wt - total_gypsum_Wt,
      #
      Chloride_ECOS = Chloride_MOLkg / total_anions_MOLkg_total * 100 , # |> abs()
      Nitrate_ECOS = Nitrate_MOLkg / total_anions_MOLkg_total * 100, # |> abs()
      Sulfate_ECOS = Sulfate_MOLkg / total_anions_MOLkg_total * 100, # |> abs()
      Sodium_ECOS = Sodium_MOLkg / total_cations_MOLkg_total * 100,
      Potassium_ECOS = Potassium_MOLkg / total_cations_MOLkg_total * 100,
      Calcium_ECOS = Calcium_MOLkg / total_cations_MOLkg_total * 100,
      Magnesium_ECOS = Magnesium_MOLkg / total_cations_MOLkg_total * 100,
      #
      ECOS_total_anions = Chloride_ECOS + Nitrate_ECOS + Sulfate_ECOS * 2,
      ECOS_total_cations = Sodium_ECOS + Potassium_ECOS + Calcium_ECOS * 2 + Magnesium_ECOS * 2,
      ECOS_diff = abs(ECOS_total_anions - ECOS_total_cations),
      ECOS_sample_name = sample_name
      # ,
      # ECOS_sample_xs = ifelse()
    )
}
