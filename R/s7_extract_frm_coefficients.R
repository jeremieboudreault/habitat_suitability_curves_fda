# s7_extract_frm_coefficients.R


# Extract FRM coefficients and compute bootstrap 95% CI.


# Project : habitat_suitability_curves_fda
# Author  : Jeremie Boudreault
# Email   : Jeremie.Boudreault [at] ete.inrs [dot] ca
# Depends : R (v3.6.3)
# License : CC BY-NC-ND 4.0



# Library ----------------------------------------------------------------------


library(data.table)
library(ggplot2)
library(FDboost)


# Functions --------------------------------------------------------------------


source(file.path("R", "functions", "internals.R"))
source(file.path("R", "functions", "fdboost_helpers.R"))


# Imports ----------------------------------------------------------------------


# Fitted functional models.
frm_results <- qs::qread(
    file = file.path("out", "tmp", "s6_frm_results.qs")
)


# Extract coefficients ---------------------------------------------------------


# To be continued...

