# s5_fit_functional_model.R


# Fit functional regression model to curves.


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


# Imports ----------------------------------------------------------------------


# Functional observations.
fd_curves <- qs::qread(
    file.path("out", "final", "fd_curves.qs")
)


