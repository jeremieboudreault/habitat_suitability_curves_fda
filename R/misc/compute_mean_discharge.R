# compute_mean_discharge.R


# Compute mean discharge on both rivers.


# Project : habitat_suitability_curves_fda
# Author  : Jeremie Boudreault
# Email   : Jeremie.Boudreault [at] ete.inrs [dot] ca
# Depends : R (v3.6.3)
# License : CC BY-NC-ND 4.0



# Library ----------------------------------------------------------------------


library(data.table)


# Inputs -----------------------------------------------------------------------


# PCR.
discharge_pcr <- data.table::fread(
    input  = file.path("data", "hydro", "010902_Q_MOY.txt"),
    skip   = 16L,
    header = TRUE,
    fill   = TRUE
)

# SMR.
discharge_smr <- data.table::fread(
    input  = file.path("data", "hydro", "062803_Q_MOY.txt"),
    skip   = 16L,
    header = TRUE,
    fill   = TRUE
)


# Compute mean discharge -------------------------------------------------------


# PCR.
discharge_pcr[!is.na(Annuel), mean(Annuel)]
discharge_pcr[!is.na(Annuel), 1L] # Années.

# SMR.
discharge_smr[!is.na(Annuel), mean(Annuel)]
discharge_smr[!is.na(Annuel), 1L] # Années.

