% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_salt_balance.R
\name{fun_salt_balance}
\alias{fun_salt_balance}
\title{Calculating salt balance from ion chromatography data}
\source{
https://predict.kikirpa.be/index.php/tools/moisture-and-salt-sample-data-analysis-tool/`
}
\usage{
fun_salt_balance(
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
)
}
\arguments{
\item{sample_name}{Sample name}

\item{dry_g}{Dry mass of sample, g}

\item{water_ml}{Water added for ion chromatography, ml}

\item{chloride_ppm}{Chloride concentration, ppm}

\item{nitrate_ppm}{Nitrate concentration, ppm}

\item{sulfate_ppm}{Sulfate concentration, ppm}

\item{sodium_ppm}{Sodium concentration, ppm}

\item{potassium_ppm}{Potassium concentration, ppm}

\item{calcium_ppm}{Calcium concentration, ppm}

\item{magnesium_ppm}{Magnesium concentration, ppm}
}
\value{
Dataframe of balanced ions for ECOS Runsalt software
}
\description{
Performs a salt balance on ion chromatography data and returns outputs for ECOS Runsalt software.
}
\examples{
\dontrun{
fun_salt_balance(sample_name = "Pathway 2",
                 dry_g = 0.801,
                 water_ml = 100,
                 chloride_ppm = 14.651,
                 nitrate_ppm = 17.339,
                 sulfate_ppm = 39.923,
                 sodium_ppm = 2.027,
                 potassium_ppm = 2.04,
                 calcium_ppm = 49.809,
                 magnesium_ppm = 0.581) |>
dplyr::glimpse()
}


}
\references{
Godts, S., Steiger, M., Orr, S.A. et al. Charge balance calculations for mixed salt systems applied to a large dataset from the built environment. Sci Data 9, 324 (2022). https://doi.org/10.1038/s41597-022-01445-9
}
