# s8_compute_coef_bootstrap.R


# Compute regression coefficient bootstrap to calculate the standard error.


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

# Functional observations.
fd_curves_list <- qs::qread(
    file = file.path("out", "tmp", "s5_fd_curves_list.qs")
)

