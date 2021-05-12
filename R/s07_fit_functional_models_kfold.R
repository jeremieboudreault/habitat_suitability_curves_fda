# s07_fit_functional_models_kfold.R


# Step 07 : Fit functional regression model with k-fold cross-validation.


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
source(file.path("R", "functions", "globals.R"))
source(file.path("R", "functions", "fdboost_helpers.R"))


# Imports ----------------------------------------------------------------------


# Functional observations.
fd_curves_list <- qs::qread(
    file = file.path("out", "tmp", "s06_fd_curves_list.qs")
)


# Depth model ------------------------------------------------------------------


# Options for the depth model.
depth_opts <- .fdboost_opts(learning_rate = 0.2)

# Fit FDboost with k-fold cross validation using the options above.
res_depth <- FDboost_kfold(
    data         = fd_curves_list$DEPTH,
    fdboost_opts = depth_opts
)


# D50 model --------------------------------------------------------------------


# Options for the D50 model.
d50_opts <- .fdboost_opts(learning_rate = 2L, mstop_max = 1000L)

# Fit FDboost with k-fold cross validation using the options above.
res_d50 <- FDboost_kfold(
    data         = fd_curves_list$D50,
    fdboost_opts = d50_opts
)


# Velocity model ---------------------------------------------------------------


# Options for the velocity model.
velocity_opts <- .fdboost_opts(learning_rate = 0.1)

# Fit FDboost with k-fold cross validation using the options above.
res_velocity <- FDboost_kfold(
    data         = fd_curves_list$VELOCITY,
    fdboost_opts = velocity_opts
)


# Exports ----------------------------------------------------------------------


qs::qsave(
    x   = list(
        DEPTH    = res_depth,
        D50      = res_d50,
        VELOCITY = res_velocity
    ),
    file = file.path("out", "tmp", "s07_frm_results.qs")
)

