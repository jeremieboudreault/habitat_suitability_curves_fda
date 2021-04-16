# s6_fit_functional_model.R


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
source(file.path("R", "functions", "fdboost_helpers.R"))


# Imports ----------------------------------------------------------------------


# Functional observations.
fd_curves_list <- qs::qread(
    file = file.path("out", "tmp", "s5_fd_curves_list.qs")
)


# FDboost options --------------------------------------------------------------


fdboost_opts <- list(
    mstop_max     = 1000L,
    learning_rate = 0.1,
    metric        = "frmse"
)


# FDboost fit ------------------------------------------------------------------


# Depth model.
res_depth <- FDboost_kfold(
    data    = fd_curves_list$DEPTH,
    n_folds = nrow(fd_curves_list$DEPTH$Y),
    fdboost_opts
)

# D50 model.
res_d50 <- FDboost_kfold(
    data    = fd_curves_list$D50,
    n_folds = nrow(fd_curves_list$D50$Y),
    fdboost_opts
)

# Velocity model.
res_velocity <- FDboost_kfold(
    data    = fd_curves_list$VELOCITY,
    n_folds = nrow(fd_curves_list$D50$VELOCITY),
    fdboost_opts
)

