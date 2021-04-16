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
    mstop_max     = 2000L,
    learning_rate = 0.1,
    metric        = "frmse",
    n_folds       = "loo"
)


# Depth model ------------------------------------------------------------------


# Fit FDboost with k-fold cross validation using the options above.
res_depth <- FDboost_kfold(
    data         = fd_curves_list$DEPTH,
    fdboost_opts = fdboost_opts
)

# Save results for traceability.
qs::qsave(res_depth, file.path("out", "tmp", "s6_depth_frm_results.qs"))


# D50 model --------------------------------------------------------------------


# Fit FDboost with k-fold cross validation using the options above.
res_d50 <- FDboost_kfold(
    data    = fd_curves_list$D50,
    fdboost_opts
)

# Save results for traceability.
qs::qsave(res_d50, file.path("out", "tmp", "s6_d50_frm_results.qs"))


# Velocity model ---------------------------------------------------------------


# Fit FDboost with k-fold cross validation using the options above.
res_velocity <- FDboost_kfold(
    data    = fd_curves_list$VELOCITY,
    fdboost_opts
)

# Save results.
qs::qsave(res_depth, file.path("out", "tmp", "s6_velocity_frm_results.qs"))


# Plot results -----------------------------------------------------------------


# To be completed...

