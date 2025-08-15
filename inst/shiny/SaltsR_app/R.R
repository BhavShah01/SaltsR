## R helper file for SaltsRApp ----

tidyRunsalt <- function(file_path, X_col = "RH", Y_col = "mol", Temp_value = 20) {
  FILENAME = basename(file_path)

  runsalt_output <-
    read_table(file_path, col_names = FALSE) |>
    pivot_longer(-1) |>
    separate(X1, sep = "_", into = c("Salt", "Variable")) |>
    drop_na(value) |>
    pivot_wider(names_from = Variable, values_from = value) |>
    unnest(c(X, Y)) |>
    mutate(
      filename = FILENAME,
      !!X_col := X,
      !!Y_col := Y,
      Temp = Temp_value)

  return(runsalt_output)
}


fun_salt_balance <- function(
    # sample_name = "A unique sample name to link to your metadata",
  # dry_g = 0.801,
  # water_ml = 100,
  # chloride_ppm = 14.651,
  # nitrate_ppm = 17.339,
  # sulfate_ppm = 39.923,
  # sodium_ppm = 2.027,
  # potassium_ppm = 2.04,
  # calcium_ppm = 49.809,
  # magnesium_ppm = 0.581

  sample_name,
  dry_g,
  water_ml,
  chloride_ppm,
  nitrate_ppm,
  sulfate_ppm,
  sodium_ppm,
  potassium_ppm,
  calcium_ppm,
  magnesium_ppm

) {

  # # Load molecular weights and ion charges (z)
  # data("mol_wts", envir = environment())
  # data("salt_charges_z", envir = environment())

  mol_wts <- data.frame(
    chloride = 35.4527,
    nitrate = 62.0049,
    sulfate = 96.064,
    sodium = 22.989768,
    potassium = 39.0983,
    calcium = 40.078,
    magnesium = 24.305
  )

  salt_charges_z <- data.frame(
    chloride = 1,
    nitrate = 1,
    sulfate = 2,
    sodium = 1,
    potassium = 1,
    calcium = 2,
    magnesium = 2
  )


  # Eqn 1. Weight fractions (displayed as weight percents)
  # Eqn 1. Ion content in the sample (w\%)
  # ms (g) | Vw (mL) | c (mg L-1) -> wCl (kg/kg (-))
  chloride_wt = (chloride_ppm * (water_ml / 1000)) / (dry_g * 1000)
  nitrate_wt = (nitrate_ppm * (water_ml / 1000)) / (dry_g * 1000)
  sulfate_wt = (sulfate_ppm * (water_ml / 1000)) / (dry_g * 1000)
  sodium_wt = (sodium_ppm * (water_ml / 1000)) / (dry_g * 1000)
  potassium_wt = (potassium_ppm * (water_ml / 1000)) / (dry_g * 1000)
  calcium_wt = (calcium_ppm * (water_ml / 1000)) / (dry_g * 1000)
  magnesium_wt = (magnesium_ppm * (water_ml / 1000)) / (dry_g * 1000)
  total_wt = chloride_wt + nitrate_wt + sulfate_wt +
    sodium_wt + potassium_wt + calcium_wt + magnesium_wt


  # Eqn 2. Amount of substance converted to mEq
  # e (mEq/kg) | eani  (mEq/kg) | ecat  (mEq/kg)
  chloride_mEq = ((chloride_wt * salt_charges_z$chloride) / (mol_wts$chloride / 1000)) * 1000
  nitrate_mEq = ((nitrate_wt * salt_charges_z$nitrate) / (mol_wts$nitrate / 1000)) * 1000
  sulfate_mEq = ((sulfate_wt * salt_charges_z$sulfate) / (mol_wts$sulfate / 1000)) * 1000
  sodium_mEq = ((sodium_wt * salt_charges_z$sodium) / (mol_wts$sodium / 1000)) * 1000
  potassium_mEq = ((potassium_wt * salt_charges_z$potassium) / (mol_wts$potassium / 1000)) * 1000
  calcium_mEq = ((calcium_wt * salt_charges_z$calcium) / (mol_wts$calcium / 1000)) * 1000
  magnesium_mEq = ((magnesium_wt * salt_charges_z$magnesium) / (mol_wts$magnesium / 1000)) * 1000
  total_mEq_anions = chloride_mEq + nitrate_mEq + sulfate_mEq
  total_mEq_cations = sodium_mEq + potassium_mEq + calcium_mEq + magnesium_mEq


  # Eqn 3. Initial balance to decide Pathway
  # Δe  (mEq/kg) | Δecat or Δeani | pI | pII
  charge_imbalance_initial = abs(total_mEq_cations - total_mEq_anions)
  imbalance_allocation = ifelse(total_mEq_cations > total_mEq_anions,
                                "dExcess Cations", "dExcess Anions")
  Pathway1 = ifelse(
    charge_imbalance_initial <= max(total_mEq_cations, total_mEq_anions) * 0.02 |
      total_mEq_anions > total_mEq_cations, TRUE, FALSE)
  Pathway2 = ifelse(
    charge_imbalance_initial > total_mEq_cations * 0.02 &
      total_mEq_cations > total_mEq_anions, TRUE, FALSE)
  Pathway = ifelse(Pathway1 == TRUE, "Pathway 1", "Pathway 2")


  # Eqn 4. Pathway I - Adjustment of all ions equally
  # e,adj (mEq/kg) eq4
  chloride_mEq_Path1 = ifelse(
    Pathway == "Pathway 1",
    (chloride_mEq * (total_mEq_anions + total_mEq_cations) / (2 * total_mEq_anions)), NA)
  nitrate_mEq_Path1 = ifelse(
    Pathway == "Pathway 1",
    (nitrate_mEq * (total_mEq_anions + total_mEq_cations) / (2 * total_mEq_anions)), NA)
  sulfate_mEq_Path1 = ifelse(
    Pathway == "Pathway 1",
    (sulfate_mEq * (total_mEq_anions + total_mEq_cations) / (2 * total_mEq_anions)), NA)
  sodium_mEq_Path1 = ifelse(
    Pathway == "Pathway 1",
    (sodium_mEq * (total_mEq_anions + total_mEq_cations) / (2 * total_mEq_cations)), NA)
  potassium_mEq_Path1 = ifelse(
    Pathway == "Pathway 1",
    (potassium_mEq * (total_mEq_anions + total_mEq_cations) / (2 * total_mEq_cations)), NA)
  calcium_mEq_Path1 = ifelse(
    Pathway == "Pathway 1",
    (calcium_mEq * (total_mEq_anions + total_mEq_cations) / (2 * total_mEq_cations)), NA)
  magnesium_mEq_Path1 = ifelse(
    Pathway == "Pathway 1",
    (magnesium_mEq * (total_mEq_anions + total_mEq_cations) / (2 * total_mEq_cations)), NA)


  # Eqn 5a. Pathway II - Excess is assumed to relate to the least soluble salt: Calcium adjustment
  # e (mEq/kg) eq5a
  chloride_mEq_Path2Ca = ifelse(Pathway == "Pathway 2", chloride_mEq, NA)
  nitrate_mEq_Path2Ca = ifelse(Pathway == "Pathway 2", nitrate_mEq, NA)
  sulfate_mEq_Path2Ca = ifelse(Pathway == "Pathway 2", sulfate_mEq, NA)
  sodium_mEq_Path2Ca = ifelse(Pathway == "Pathway 2", sodium_mEq, NA)
  potassium_mEq_Path2Ca = ifelse(Pathway == "Pathway 2", potassium_mEq, NA)
  calcium_mEq_Path2Ca = ifelse(
    Pathway == "Pathway 2",
    (ifelse(calcium_mEq - charge_imbalance_initial >= 0,
            calcium_mEq - charge_imbalance_initial, 0)), NA)
  magnesium_mEq_Path2Ca = ifelse(Pathway == "Pathway 2", magnesium_mEq, NA)
  # Re-balance post Ca (5a)
  total_mEq_anions_Path2Ca = chloride_mEq_Path2Ca + nitrate_mEq_Path2Ca + sulfate_mEq_Path2Ca
  total_mEq_cations_Path2Ca = sodium_mEq_Path2Ca + potassium_mEq_Path2Ca + calcium_mEq_Path2Ca + magnesium_mEq_Path2Ca
  charge_imbalance_CaAdj = ifelse(
    abs(total_mEq_anions_Path2Ca - total_mEq_cations_Path2Ca) < 0.000001,
    0, total_mEq_anions_Path2Ca - total_mEq_cations_Path2Ca)


  # Eqn 5b. Pathway II - Excess is assumed to relate to the least soluble salt: Magnesium adjustment
  # e (mEq/kg) eq5b
  chloride_mEq_Path2Mg = ifelse(Pathway == "Pathway 2", chloride_mEq_Path2Ca, NA)
  nitrate_mEq_Path2Mg = ifelse(Pathway == "Pathway 2", nitrate_mEq_Path2Ca, NA)
  sulfate_mEq_Path2Mg = ifelse(Pathway == "Pathway 2", sulfate_mEq_Path2Ca, NA)
  sodium_mEq_Path2Mg = ifelse(Pathway == "Pathway 2", sodium_mEq_Path2Ca, NA)
  potassium_mEq_Path2Mg = ifelse(Pathway == "Pathway 2", potassium_mEq_Path2Ca, NA)
  calcium_mEq_Path2Mg = ifelse(Pathway == "Pathway 2", calcium_mEq_Path2Ca, NA)
  magnesium_mEq_Path2Mg = ifelse(
    Pathway == "Pathway 2",
    (ifelse(magnesium_mEq_Path2Ca - charge_imbalance_CaAdj >= 0,
            magnesium_mEq_Path2Ca - charge_imbalance_CaAdj, 0)), NA)
  # Re-balance post Mg (5b)
  total_mEq_anions_Path2Mg = chloride_mEq_Path2Mg + nitrate_mEq_Path2Mg + sulfate_mEq_Path2Mg
  total_mEq_cations_Path2Mg = sodium_mEq_Path2Mg + potassium_mEq_Path2Mg + calcium_mEq_Path2Mg + magnesium_mEq_Path2Mg
  charge_imbalance_MgAdj = ifelse(
    abs(total_mEq_anions_Path2Mg - total_mEq_cations_Path2Mg) < 0.000001,
    0, total_mEq_anions_Path2Mg - total_mEq_cations_Path2Mg)


  # Eqn 5c. Pathway II - Excess is assumed to relate to the least soluble salt: Sodium adjustment
  # e (mEq/kg) eq5c
  chloride_mEq_Path2Na = ifelse(Pathway == "Pathway 2", chloride_mEq_Path2Mg, NA)
  nitrate_mEq_Path2Na = ifelse(Pathway == "Pathway 2", nitrate_mEq_Path2Mg, NA)
  sulfate_mEq_Path2Na = ifelse(Pathway == "Pathway 2", sulfate_mEq_Path2Mg, NA)
  sodium_mEq_Path2Na = ifelse(
    Pathway == "Pathway 2",
    (ifelse(sodium_mEq_Path2Mg - charge_imbalance_MgAdj >= 0,
            sodium_mEq_Path2Mg - charge_imbalance_MgAdj, 0)), NA)
  potassium_mEq_Path2Na = ifelse(Pathway == "Pathway 2", potassium_mEq_Path2Mg, NA)
  calcium_mEq_Path2Na = ifelse(Pathway == "Pathway 2", calcium_mEq_Path2Mg, NA)
  magnesium_mEq_Path2Na = ifelse(Pathway == "Pathway 2", magnesium_mEq_Path2Mg, NA)
  # Re-balance post Na (5c)
  total_mEq_anions_Path2Na = chloride_mEq_Path2Na + nitrate_mEq_Path2Na + sulfate_mEq_Path2Na
  total_mEq_cations_Path2Na = sodium_mEq_Path2Na + potassium_mEq_Path2Na + calcium_mEq_Path2Na + magnesium_mEq_Path2Na
  charge_imbalance_NaAdj = ifelse(
    abs(total_mEq_anions_Path2Na - total_mEq_cations_Path2Na) < 0.000001,
    0, total_mEq_anions_Path2Na - total_mEq_cations_Path2Na)


  # Eqn 5d. Pathway II - Excess is assumed to relate to the least soluble salt: Potassium adjustment
  # e,adj (mEq/kg) eq5d
  chloride_mEq_Path2K = ifelse(Pathway == "Pathway 2", chloride_mEq_Path2Na, NA)
  nitrate_mEq_Path2K = ifelse(Pathway == "Pathway 2", nitrate_mEq_Path2Na, NA)
  sulfate_mEq_Path2K = ifelse(Pathway == "Pathway 2", sulfate_mEq_Path2Na, NA)
  sodium_mEq_Path2K = ifelse(Pathway == "Pathway 2", sodium_mEq_Path2Na, NA)
  potassium_mEq_Path2K = ifelse(
    Pathway == "Pathway 2",
    (ifelse(potassium_mEq_Path2Na - charge_imbalance_NaAdj >= 0,
            potassium_mEq_Path2Na - charge_imbalance_NaAdj, 0)), NA)
  calcium_mEq_Path2K = ifelse(Pathway == "Pathway 2", calcium_mEq_Path2Na, NA)
  magnesium_mEq_Path2K = ifelse(Pathway == "Pathway 2", magnesium_mEq_Path2Na, NA)
  # Re-balance post K (5d)
  total_mEq_anions_Path2K = chloride_mEq_Path2K + nitrate_mEq_Path2K + sulfate_mEq_Path2K
  total_mEq_cations_Path2K = sodium_mEq_Path2K + potassium_mEq_Path2K + calcium_mEq_Path2K + magnesium_mEq_Path2K
  charge_imbalance_KAdj = ifelse(
    abs(total_mEq_anions_Path2K - total_mEq_cations_Path2K) < 0.000001,
    0, total_mEq_anions_Path2K - total_mEq_cations_Path2K)


  # Adjusted Values (after either Pathway I or Pathway II) for gypsum removal
  # e,adj (mEq/kg) pI or pII
  chloride_mEq_adj = ifelse(Pathway == "Pathway 1", chloride_mEq_Path1, chloride_mEq_Path2K)
  nitrate_mEq_adj = ifelse(Pathway == "Pathway 1", nitrate_mEq_Path1, nitrate_mEq_Path2K)
  sulfate_mEq_adj = ifelse(Pathway == "Pathway 1", sulfate_mEq_Path1, sulfate_mEq_Path2K)
  sodium_mEq_adj = ifelse(Pathway == "Pathway 1", sodium_mEq_Path1, sodium_mEq_Path2K)
  potassium_mEq_adj = ifelse(Pathway == "Pathway 1", potassium_mEq_Path1, potassium_mEq_Path2K)
  calcium_mEq_adj = ifelse(Pathway == "Pathway 1", calcium_mEq_Path1, calcium_mEq_Path2K)
  magnesium_mEq_adj = ifelse(Pathway == "Pathway 1", magnesium_mEq_Path1, magnesium_mEq_Path2K)


  # Eqn 6. Amount of Calcium or Sulfate that limits CaSO4 production
  # Determination of gypsum content
  # elim,CaSO4 (mEq/kg) eq6
  gypsum_content_limit = min(sulfate_mEq_adj, calcium_mEq_adj)


  # Eqn 7. Removal of gypsum ECOS/Runsalt model specific
  # e,adj (mEq/kg) eq7
  chloride_mEq_adj_SO4 = chloride_mEq_adj
  nitrate_mEq_adj_SO4 = nitrate_mEq_adj
  sulfate_mEq_adj_SO4 = sulfate_mEq_adj - gypsum_content_limit
  sodium_mEq_adj_SO4 = sodium_mEq_adj
  potassium_mEq_adj_SO4 = potassium_mEq_adj
  calcium_mEq_adj_SO4 = calcium_mEq_adj - gypsum_content_limit
  magnesium_mEq_adj_SO4 = magnesium_mEq_adj

  # Final Balance check
  # Δef (mEq/kg)
  charge_imbalance_final = ifelse(
    abs((chloride_mEq_adj_SO4 + nitrate_mEq_adj_SO4 + sulfate_mEq_adj_SO4) -
          (sodium_mEq_adj_SO4 + potassium_mEq_adj_SO4 + calcium_mEq_adj_SO4 + magnesium_mEq_adj_SO4))
    < 0.000001, TRUE, FALSE)


  # Eqn 8 part 1. Balanced Molar Concentrations excluding gypsum
  # c,adj (mol/kg)
  chloride_molkg = chloride_mEq_adj_SO4 / salt_charges_z$chloride / 1000
  nitrate_molkg = nitrate_mEq_adj_SO4 / salt_charges_z$nitrate / 1000
  sulfate_molkg = sulfate_mEq_adj_SO4 / salt_charges_z$sulfate / 1000
  sodium_molkg = sodium_mEq_adj_SO4 / salt_charges_z$sodium / 1000
  potassium_molkg = potassium_mEq_adj_SO4 / salt_charges_z$potassium / 1000
  calcium_molkg = calcium_mEq_adj_SO4 / salt_charges_z$calcium / 1000
  magnesium_mmolkg = magnesium_mEq_adj_SO4 / salt_charges_z$magnesium / 1000


  # Eqn 8 part 2. ECOS INPUTS: Adjusted amounts excluding gypsum as mole fraction (ion values as mole fraction)
  # x,adj (-) eq8
  chloride_x = chloride_molkg / (chloride_molkg + nitrate_molkg + sulfate_molkg + sodium_molkg + potassium_molkg + calcium_molkg + magnesium_mmolkg)
  nitrate_x = nitrate_molkg / (chloride_molkg + nitrate_molkg + sulfate_molkg + sodium_molkg + potassium_molkg + calcium_molkg + magnesium_mmolkg)
  sulfate_x = sulfate_molkg / (chloride_molkg + nitrate_molkg + sulfate_molkg + sodium_molkg + potassium_molkg + calcium_molkg + magnesium_mmolkg)
  sodium_x = sodium_molkg / (chloride_molkg + nitrate_molkg + sulfate_molkg + sodium_molkg + potassium_molkg + calcium_molkg + magnesium_mmolkg)
  potassium_x = potassium_molkg / (chloride_molkg + nitrate_molkg + sulfate_molkg + sodium_molkg + potassium_molkg + calcium_molkg + magnesium_mmolkg)
  calcium_x = calcium_molkg / (chloride_molkg + nitrate_molkg + sulfate_molkg + sodium_molkg + potassium_molkg + calcium_molkg + magnesium_mmolkg)
  magnesium_x = magnesium_mmolkg / (chloride_molkg + nitrate_molkg + sulfate_molkg + sodium_molkg + potassium_molkg + calcium_molkg + magnesium_mmolkg)


  # Eqn 9. Assessment of Correction Degree as a Fraction of Initial Cation Sum, displayed as percent
  # Degree of adjustments as a fraction
  # f Δe  amount of substance in excess as a fraction
  # fΔe (-) eq9
  calcium_fraction = ifelse(
    Pathway == "Pathway 1", 0,
    (calcium_mEq - calcium_mEq_adj) / (sodium_mEq + potassium_mEq + calcium_mEq + magnesium_mEq))
  magnesium_fraction =ifelse(
    Pathway == "Pathway 1", 0,
    (magnesium_mEq - magnesium_mEq_adj) / (sodium_mEq + potassium_mEq + calcium_mEq + magnesium_mEq))
  sodium_fraction =ifelse(
    Pathway == "Pathway 1", 0,
    (sodium_mEq - sodium_mEq_adj) / (sodium_mEq + potassium_mEq + calcium_mEq + magnesium_mEq))
  potassium_fraction =ifelse(
    Pathway == "Pathway 1", 0,
    (potassium_mEq - potassium_mEq_adj) / (sodium_mEq + potassium_mEq + calcium_mEq + magnesium_mEq))


  # Eqn 10. Adjusted contents as Weight Fraction Relative to the Dry Sample Mass, displayed as a weight percent
  # final corrected amount of substance as weight fraction per individual ion in the dry sample mass, wi,f
  # w,f (-) eq10
  chloride_wt_adj = ((chloride_mEq_adj_SO4 * (mol_wts$chloride / 1000)) / salt_charges_z$chloride) * 0.001
  nitrate_wt_adj = ((nitrate_mEq_adj_SO4 * (mol_wts$nitrate / 1000)) / salt_charges_z$nitrate) * 0.001
  sulfate_wt_adj = ((sulfate_mEq_adj_SO4 * (mol_wts$sulfate / 1000)) / salt_charges_z$sulfate) * 0.001
  sodium_wt_adj = ((sodium_mEq_adj_SO4 * (mol_wts$sodium / 1000)) / salt_charges_z$sodium) * 0.001
  potassium_wt_adj = ((potassium_mEq_adj_SO4 * (mol_wts$potassium / 1000)) / salt_charges_z$potassium) * 0.001
  calcium_wt_adj = ((calcium_mEq_adj_SO4 * (mol_wts$calcium / 1000)) / salt_charges_z$calcium) * 0.001
  magnesium_wt_adj = ((magnesium_mEq_adj_SO4 * (mol_wts$magnesium / 1000)) / salt_charges_z$magnesium) * 0.001
  total_wt_adj = chloride_wt_adj + nitrate_wt_adj + sulfate_wt_adj +
    sodium_wt_adj + potassium_wt_adj + calcium_wt_adj + magnesium_wt_adj


  # Eqn 11. Evaluation of the corrected Ion and Gypsum Content displayed as a weight percent
  # total ion content adjusted as a fraction compared to the dry sample mass,
  # wtot,adj
  # wtot,adj + wCaSO4(-) eq11
  total_wt_adj_gypsum = (total_wt - total_wt_adj)
  # Gypsum content
  # wCaSO4 (-)
  gypsum_content = ((gypsum_content_limit) * (0.5 * (mol_wts$sulfate + mol_wts$calcium)) * 0.000001)

  # saturation degree of determined  gypsum content in given sample/water ratio (considering 2.14g/L 20C)
  # SCaSO4 (%)
  saturation_gypsum_content = gypsum_content / ((0.214 * water_ml / 10000) / dry_g * 100)

  # Total amount of the adjusted ion content (excluding gypsum)
  # wtot,adj (-) eq11
  total_ion_content = total_wt_adj_gypsum - gypsum_content

  # adjusted content sum Na+, K+
  # wMg,Na,K,adj (-)
  sodium_potassium_content_adj = ifelse(
    Pathway == "Pathway 2", abs((sodium_wt_adj + potassium_wt_adj) - (sodium_wt + potassium_wt)), NA)

  # adjusted content Mg2+
  # wMg,adj (-)
  magnessium_content_adj = ifelse(
    Pathway == "Pathway 2", (((abs(magnesium_mEq_adj - magnesium_mEq)) * 0.024305) / 2) * 0.001, NA)

  # adjusted content Ca2+
  # wCa,adj (-)2
  calcium_content_adj =ifelse(
    Pathway == "Pathway 2", (((abs(calcium_mEq_adj - calcium_mEq)) * 0.040078) / 2) * 0.001, NA)

  # hypothetical CO32- content related to Na+, K+
  # wCO3,h (-)
  hypothetical_CO3 = ifelse(
    Pathway == "Pathway 2", ((charge_imbalance_MgAdj + charge_imbalance_NaAdj + charge_imbalance_KAdj) / 1000000) * (60.01 / 2),
    NA)

  # ECOS outputs
  # Eqn 8b. Mols
  chloride_ECOS_mol = chloride_x
  nitrate_ECOS_mol = nitrate_x
  sulfate_ECOS_mol = sulfate_x
  sodium_ECOS_mol = sodium_x
  potassium_ECOS_mol = potassium_x
  calcium_ECOS_mol = calcium_x
  magnesium_ECOS_mol = magnesium_x

  # Eqn 10. Weights
  chloride_ECOS_weight = chloride_wt_adj
  nitrate_ECOS_weight = nitrate_wt_adj
  sulfate_ECOS_weight = sulfate_wt_adj
  sodium_ECOS_weight = sodium_wt_adj
  potassium_ECOS_weight = potassium_wt_adj
  calcium_ECOS_weight = calcium_wt_adj
  magnesium_ECOS_weight = magnesium_wt_adj

  ECOS_pathway = Pathway
  ECOS_warnings = ifelse(
    saturation_gypsum_content > 1,
    "The true gypsum content is likely to be higher and dilution is needed.",
    "No warnings")



  # RESULTS Output dataframe with results of all calculations
  salt_balance <- tibble::tibble(
    # salt_balance <- c(
    sample_name,
    dry_g,
    water_ml,
    chloride_ppm,
    nitrate_ppm,
    sulfate_ppm,
    sodium_ppm,
    potassium_ppm,
    calcium_ppm,
    magnesium_ppm,


    # Eqn 1
    chloride_wt,
    nitrate_wt,
    sulfate_wt,
    sodium_wt,
    potassium_wt,
    calcium_wt,
    magnesium_wt,
    total_wt,

    # Eqn 2
    chloride_mEq,
    nitrate_mEq,
    sulfate_mEq,
    sodium_mEq,
    potassium_mEq,
    calcium_mEq,
    magnesium_mEq,
    total_mEq_anions,
    total_mEq_cations,

    # Eqn 3
    charge_imbalance_initial,
    imbalance_allocation,
    Pathway1,
    Pathway2,
    Pathway,

    # Eqn 4
    chloride_mEq_Path1,
    nitrate_mEq_Path1,
    sulfate_mEq_Path1,
    sodium_mEq_Path1,
    potassium_mEq_Path1,
    calcium_mEq_Path1,
    magnesium_mEq_Path1,

    # Eqn 5a
    chloride_mEq_Path2Ca,
    nitrate_mEq_Path2Ca,
    sulfate_mEq_Path2Ca,
    sodium_mEq_Path2Ca,
    potassium_mEq_Path2Ca,
    calcium_mEq_Path2Ca,
    magnesium_mEq_Path2Ca,
    total_mEq_anions_Path2Ca,
    total_mEq_cations_Path2Ca,
    charge_imbalance_CaAdj,

    # Eqn 5b
    chloride_mEq_Path2Mg,
    nitrate_mEq_Path2Mg,
    sulfate_mEq_Path2Mg,
    sodium_mEq_Path2Mg,
    potassium_mEq_Path2Mg,
    calcium_mEq_Path2Mg,
    magnesium_mEq_Path2Mg,
    total_mEq_anions_Path2Mg,
    total_mEq_cations_Path2Mg,
    charge_imbalance_MgAdj,

    # Eqn 5c
    chloride_mEq_Path2Na,
    nitrate_mEq_Path2Na,
    sulfate_mEq_Path2Na,
    sodium_mEq_Path2Na,
    potassium_mEq_Path2Na,
    calcium_mEq_Path2Na,
    magnesium_mEq_Path2Na,
    total_mEq_anions_Path2Na,
    total_mEq_cations_Path2Na,
    charge_imbalance_NaAdj,

    # Eqn 5d
    chloride_mEq_Path2K,
    nitrate_mEq_Path2K,
    sulfate_mEq_Path2K,
    sodium_mEq_Path2K,
    potassium_mEq_Path2K,
    calcium_mEq_Path2K,
    magnesium_mEq_Path2K,
    total_mEq_anions_Path2K,
    total_mEq_cations_Path2K,
    charge_imbalance_KAdj,

    # Adjusted for gypsum removal
    chloride_mEq_adj,
    nitrate_mEq_adj,
    sulfate_mEq_adj,
    sodium_mEq_adj,
    potassium_mEq_adj,
    calcium_mEq_adj,
    magnesium_mEq_adj,

    # Eqn 6
    gypsum_content_limit,

    # Eqn 7
    chloride_mEq_adj_SO4,
    nitrate_mEq_adj_SO4,
    sulfate_mEq_adj_SO4,
    sodium_mEq_adj_SO4,
    potassium_mEq_adj_SO4,
    calcium_mEq_adj_SO4,
    magnesium_mEq_adj_SO4,
    charge_imbalance_final,

    # Eqn 8 pt1
    chloride_molkg,
    nitrate_molkg,
    sulfate_molkg,
    sodium_molkg,
    potassium_molkg,
    calcium_molkg,
    magnesium_mmolkg,

    # Eqn 8 pt2
    ## ECOS INPUTS
    chloride_x,
    nitrate_x,
    sulfate_x,
    sodium_x,
    potassium_x,
    calcium_x,
    magnesium_x,

    # Eqn 9
    calcium_fraction,
    magnesium_fraction,
    sodium_fraction,
    potassium_fraction,

    # Eqn 10
    chloride_wt_adj,
    nitrate_wt_adj,
    sulfate_wt_adj,
    sodium_wt_adj,
    potassium_wt_adj,
    calcium_wt_adj,
    magnesium_wt_adj,
    total_wt_adj,

    # Eqn 11
    total_wt_adj_gypsum,
    gypsum_content,
    saturation_gypsum_content,
    total_ion_content,
    sodium_potassium_content_adj,
    magnessium_content_adj,
    calcium_content_adj,
    hypothetical_CO3,

    # ECOS outputs
    # Mols
    sodium_ECOS_mol,
    potassium_ECOS_mol,
    magnesium_ECOS_mol,
    calcium_ECOS_mol,
    chloride_ECOS_mol,
    nitrate_ECOS_mol,
    sulfate_ECOS_mol,

    # Weight
    sodium_ECOS_weight,
    potassium_ECOS_weight,
    magnesium_ECOS_weight,
    calcium_ECOS_weight,
    chloride_ECOS_weight,
    nitrate_ECOS_weight,
    sulfate_ECOS_weight,

    # Description and warnings
    ECOS_pathway,
    ECOS_warnings,


  )

  return(salt_balance)
}
