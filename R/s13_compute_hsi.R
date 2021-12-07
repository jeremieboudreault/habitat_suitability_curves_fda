# s13_compute_hsi.R


# Step 13 : Compute Habitat Suitability Index (HSI) on each parcel.


# Project : habitat_suitability_curves_fda
# Author  : Jeremie Boudreault
# Email   : Jeremie.Boudreault [at] ete.inrs [dot] ca
# Depends : R (v3.6.3)
# License : CC BY-NC-ND 4.0



# Library ----------------------------------------------------------------------


library(data.table)
library(ggplot2)


# Functions --------------------------------------------------------------------


source(file.path("R", "functions", "globals.R"))
source(file.path("R", "functions", "internals.R"))
source(file.path("R", "functions", "plot_helpers.R"))


# Imports ----------------------------------------------------------------------


# Habitat variable.
hab <- qs::qread(
    file = file.path("out", "tmp", "s05_hab_var.qs")
)

# Local HSC model SMR.
local_models_smr <- qs::qread(
    file = file.path("out", "tmp", "s11_local_models_smr.qs")
)

# Local HSC model PCR.
local_models_pcr <- qs::qread(
    file = file.path("out", "tmp", "s11_local_models_pcr.qs")
)

# Regional HSC model.
regional_models <- qs::qread(
    file = file.path("out", "tmp", "s11_regional_models.qs")
)

# FRM models.
frm_models <- qs::qread(
    file = file.path("out", "tmp", "s08_frm_results_full.qs")
)


# Generate tiny outputs for FRM ------------------------------------------------


frm_tiny_models <- lapply(frm_models, function(model) {

    # Extract x values.
    x <- model$full$x

    # Extract observations.
    y_obs <- model$full$y_obs

    # Extract predictions.
    y_hat <- model$full$y_hat

    # Extract cross-validation predictions.
    y_hat_cv <- model$y_hat_kfold[, , which(model$mstops == model$mstop_best)]

    # Rivers.
    river <- model$full$river

    # Return list.
    return(
        list(
            x         = x,
            y_obs     = y_obs,
            y_hat     = y_hat,
            y_hat_cv  = y_hat_cv,
            river     = river
        )
    )

})



# Create a table for habitat variable ------------------------------------------


# Dcast data.table to have one column per habitat variable.
hab_dcast <- data.table::dcast.data.table(
    data      = hab[TYPE == "AVAILABLE", ],
    formula   = SITE_INTERNAL + RIVER + PARCEL + Y ~ VARIABLE,
    value.var = "VALUE"
)

