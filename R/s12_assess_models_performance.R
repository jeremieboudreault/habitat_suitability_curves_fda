# s12_assess_models_performance.R


# Step 12 : Assess models performance.


# Project : habitat_suitability_curves_fda
# Author  : Jeremie Boudreault
# Email   : Jeremie.Boudreault [at] ete.inrs [dot] ca
# Depends : R (v3.6.3)
# License : CC BY-NC-ND 4.0



# Library ----------------------------------------------------------------------


library(data.table)
library(ggplot2)


# Functions --------------------------------------------------------------------


source(file.path("R", "functions", "internals.R"))
source(file.path("R", "functions", "globals.R"))
source(file.path("R", "functions", "fdboost_helpers.R"))


# Imports ----------------------------------------------------------------------


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


# Calculate R2 on the SMR ------------------------------------------------------


# Calculate performance of SMR on SMR - Training
smr_on_smr_R2 <- round(sapply(names(var_names), function(w) {
    y_obs <- local_models_smr[[w]]$y_obs
    y_hat <- local_models_smr[[w]]$y_hat
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "R2"
    )
}), 3L)

# Calculate performance of SMR on SMR - CV
smr_on_smr_R2_cv <- round(sapply(names(var_names), function(w) {
    y_obs <- local_models_smr[[w]]$y_obs
    y_hat <- local_models_smr[[w]]$y_hat_cv
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "R2"
    )
}), 3L)

# Calculate performance of PCR on SMR
pcr_on_smr_R2 <- round(sapply(names(var_names), function(w) {
    y_obs <- local_models_smr[[w]]$y_obs
    y_hat  = matrix(
        data  = local_models_pcr[[w]]$y_hat[1L, ],
        nrow  = nrow(y_obs),
        ncol  = ncol(y_obs),
        byrow = TRUE
    )
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "R2"
    )
}), 3L)

# Calculate performance of regional model on SMR - Training.
reg_on_smr_R2 <- round(sapply(names(var_names), function(w) {
    riv_index <- which(regional_models[[w]]$river == "SMR")
    y_obs <- regional_models[[w]]$y_obs[riv_index, ]
    y_hat <- regional_models[[w]]$y_hat[riv_index, ]
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "R2"
    )
}), 3L)

# Calculate performance of regional model on SMR - CV.
reg_on_smr_R2_cv <- round(sapply(names(var_names), function(w) {
    riv_index <- which(regional_models[[w]]$river == "SMR")
    y_obs <- regional_models[[w]]$y_obs[riv_index, ]
    y_hat <- regional_models[[w]]$y_hat_cv[riv_index, ]
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "R2"
    )
}), 3L)

# Calculate performance of FRM model on SMR - Training.
frm_on_smr_R2 <- round(sapply(names(var_names), function(w) {
    riv_index <- which(frm_tiny_models[[w]]$river == "SMR")
    y_obs <- frm_tiny_models[[w]]$y_obs[riv_index, ]
    y_hat <- frm_tiny_models[[w]]$y_hat[riv_index, ]
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "R2"
    )
}), 3L)

# Calculate performance of FRM model on SMR - CV.
frm_on_smr_R2_cv <- round(sapply(names(var_names), function(w) {
    riv_index <- which(frm_tiny_models[[w]]$river == "SMR")
    y_obs <- frm_tiny_models[[w]]$y_obs[riv_index, ]
    y_hat <- frm_tiny_models[[w]]$y_hat_cv[riv_index, ]
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "R2"
    )
}), 3L)


# Calculate R2 on the PCR ------------------------------------------------------


# Calculate performance of PCR on PCR - Training
pcr_on_pcr_R2 <- round(sapply(names(var_names), function(w) {
    y_obs <- local_models_pcr[[w]]$y_obs
    y_hat <- local_models_pcr[[w]]$y_hat
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "R2"
    )
}), 3L)

# Calculate performance of PCR on PCR - CV
pcr_on_pcr_R2_cv <- round(sapply(names(var_names), function(w) {
    y_obs <- local_models_pcr[[w]]$y_obs
    y_hat <- local_models_pcr[[w]]$y_hat_cv
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "R2"
    )
}), 3L)

# Calculate performance of SMR on PCR
smr_on_pcr_R2 <- round(sapply(names(var_names), function(w) {
    y_obs <- local_models_pcr[[w]]$y_obs
    y_hat  = matrix(
        data  = local_models_smr[[w]]$y_hat[1L, ],
        nrow  = nrow(y_obs),
        ncol  = ncol(y_obs),
        byrow = TRUE
    )
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "R2"
    )
}), 3L)

# Calculate performance of regional model on PCR - Training.
reg_on_pcr_R2 <- round(sapply(names(var_names), function(w) {
    riv_index <- which(regional_models[[w]]$river == "PCR")
    y_obs <- regional_models[[w]]$y_obs[riv_index, ]
    y_hat <- regional_models[[w]]$y_hat[riv_index, ]
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "R2"
    )
}), 3L)

# Calculate performance of regional model on PCR - CV.
reg_on_pcr_R2_cv <- round(sapply(names(var_names), function(w) {
    riv_index <- which(regional_models[[w]]$river == "PCR")
    y_obs <- regional_models[[w]]$y_obs[riv_index, ]
    y_hat <- regional_models[[w]]$y_hat_cv[riv_index, ]
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "R2"
    )
}), 3L)

# Calculate performance of FRM model on PCR - Training.
frm_on_pcr_R2 <- round(sapply(names(var_names), function(w) {
    riv_index <- which(frm_tiny_models[[w]]$river == "PCR")
    y_obs <- frm_tiny_models[[w]]$y_obs[riv_index, ]
    y_hat <- frm_tiny_models[[w]]$y_hat[riv_index, ]
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "R2"
    )
}), 3L)

# Calculate performance of FRM model on PCR - CV.
frm_on_pcr_R2_cv <- round(sapply(names(var_names), function(w) {
    riv_index <- which(frm_tiny_models[[w]]$river == "PCR")
    y_obs <- frm_tiny_models[[w]]$y_obs[riv_index, ]
    y_hat <- frm_tiny_models[[w]]$y_hat_cv[riv_index, ]
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "R2"
    )
}), 3L)



# Calculate R2 on SMR + PCR ----------------------------------------------------


# Calculate performance of REG on SMR + PCR - Training
reg_on_reg_R2 <- round(sapply(names(var_names), function(w) {
    y_obs <- regional_models[[w]]$y_obs
    y_hat <- regional_models[[w]]$y_hat
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "R2"
    )
}), 3L)

# Calculate performance of REG on SMR + PCR - CV
reg_on_reg_R2_cv <- round(sapply(names(var_names), function(w) {
    y_obs <- regional_models[[w]]$y_obs
    y_hat <- regional_models[[w]]$y_hat_cv
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "R2"
    )
}), 3L)

# Calculate performance of FRM on SMR + PCR- Training
frm_on_reg_R2 <- round(sapply(names(var_names), function(w) {
    y_obs <- frm_tiny_models[[w]]$y_obs
    y_hat <- frm_tiny_models[[w]]$y_hat
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "R2"
    )
}), 3L)

# Calculate performance of FRM on SMR + PCR - CV
frm_on_reg_R2_cv <- round(sapply(names(var_names), function(w) {
    y_obs <- frm_tiny_models[[w]]$y_obs
    y_hat <- frm_tiny_models[[w]]$y_hat_cv
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "R2"
    )
}), 3L)


# Merge all R2 results in a table ----------------------------------------------


# Create a vector of three NAs to fill empty cells in table.
nas <- rep(NA, 3L)

# Create a matrix with the results.
table_R2 <- matrix(
    data = c(
        smr_on_smr_R2, smr_on_smr_R2_cv,
        pcr_on_smr_R2, nas,
        reg_on_smr_R2, reg_on_pcr_R2_cv,
        frm_on_smr_R2, frm_on_smr_R2_cv,
        pcr_on_pcr_R2, pcr_on_pcr_R2_cv,
        smr_on_pcr_R2, nas,
        reg_on_pcr_R2, reg_on_pcr_R2_cv,
        frm_on_pcr_R2, frm_on_pcr_R2_cv,
        reg_on_reg_R2, reg_on_reg_R2_cv,
        frm_on_reg_R2, frm_on_reg_R2_cv
    ),
    ncol  = 6L,
    byrow = TRUE
)

# Add columns names.
colnames(table_R2) <- c(
    "Depth-Train", "D50-Train", "Velocity-Train",
    "Depth-CV",    "D50-CV",    "Velocity-CV"
)

# Add row names.
rownames(table_R2) <- c(
    "SMR_on_SMR",
    "PCR_on_SMR",
    "REG_on_SMR",
    "FRM_on_SMR",
    "PCR_on_PCR",
    "SMR_on_PCR",
    "REG_on_PCR",
    "FRM_on_PCR",
    "REG_on_REG",
    "FRM_on_REG"
)


# Export R2 table --------------------------------------------------------------


data.table::fwrite(
    x    = as.data.table(table_R2, keep.rownames = TRUE),
    file = file.path("out", "tables", "table_2_performance_R2.csv"),
    sep  = ";",
    dec  = "."
)


# Calculate funR2 on the SMR ---------------------------------------------------


# Calculate funR2 performance of SMR on SMR - Training
smr_on_smr_funR2 <- round(sapply(names(var_names), function(w) {
    y_obs <- local_models_smr[[w]]$y_obs
    y_hat <- local_models_smr[[w]]$y_hat
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "fR2"
    )
}), 3L)

# Calculate funR2 performance of SMR on SMR - CV
smr_on_smr_funR2_cv <- round(sapply(names(var_names), function(w) {
    y_obs <- local_models_smr[[w]]$y_obs
    y_hat <- local_models_smr[[w]]$y_hat_cv
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "fR2"
    )
}), 3L)

# Calculate funR2 performance of PCR on SMR
pcr_on_smr_funR2 <- round(sapply(names(var_names), function(w) {
    y_obs <- local_models_smr[[w]]$y_obs
    y_hat  = matrix(
        data  = local_models_pcr[[w]]$y_hat[1L, ],
        nrow  = nrow(y_obs),
        ncol  = ncol(y_obs),
        byrow = TRUE
    )
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "fR2"
    )
}), 3L)

# Calculate funR2 performance of regional model on SMR - Training.
reg_on_smr_funR2 <- round(sapply(names(var_names), function(w) {
    riv_index <- which(regional_models[[w]]$river == "SMR")
    y_obs <- regional_models[[w]]$y_obs[riv_index, ]
    y_hat <- regional_models[[w]]$y_hat[riv_index, ]
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "fR2"
    )
}), 3L)

# Calculate funR2 performance of regional model on SMR - CV.
reg_on_smr_funR2_cv <- round(sapply(names(var_names), function(w) {
    riv_index <- which(regional_models[[w]]$river == "SMR")
    y_obs <- regional_models[[w]]$y_obs[riv_index, ]
    y_hat <- regional_models[[w]]$y_hat_cv[riv_index, ]
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "fR2"
    )
}), 3L)

# Calculate funR2 performance of FRM model on SMR - Training.
frm_on_smr_funR2 <- round(sapply(names(var_names), function(w) {
    riv_index <- which(frm_tiny_models[[w]]$river == "SMR")
    y_obs <- frm_tiny_models[[w]]$y_obs[riv_index, ]
    y_hat <- frm_tiny_models[[w]]$y_hat[riv_index, ]
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "fR2"
    )
}), 3L)

# Calculate funR2 performance of FRM model on SMR - CV.
frm_on_smr_funR2_cv <- round(sapply(names(var_names), function(w) {
    riv_index <- which(frm_tiny_models[[w]]$river == "SMR")
    y_obs <- frm_tiny_models[[w]]$y_obs[riv_index, ]
    y_hat <- frm_tiny_models[[w]]$y_hat_cv[riv_index, ]
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "fR2"
    )
}), 3L)


# Calculate funR2 on the PCR ---------------------------------------------------


# Calculate funR2 performance of PCR on PCR - Training
pcr_on_pcr_funR2 <- round(sapply(names(var_names), function(w) {
    y_obs <- local_models_pcr[[w]]$y_obs
    y_hat <- local_models_pcr[[w]]$y_hat
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "fR2"
    )
}), 3L)

# Calculate funR2 performance of PCR on PCR - CV
pcr_on_pcr_funR2_cv <- round(sapply(names(var_names), function(w) {
    y_obs <- local_models_pcr[[w]]$y_obs
    y_hat <- local_models_pcr[[w]]$y_hat_cv
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "fR2"
    )
}), 3L)

# Calculate funR2 performance of SMR on PCR
smr_on_pcr_funR2 <- round(sapply(names(var_names), function(w) {
    y_obs <- local_models_pcr[[w]]$y_obs
    y_hat  = matrix(
        data  = local_models_smr[[w]]$y_hat[1L, ],
        nrow  = nrow(y_obs),
        ncol  = ncol(y_obs),
        byrow = TRUE
    )
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "fR2"
    )
}), 3L)

# Calculate funR2 performance of regional model on PCR - Training.
reg_on_pcr_funR2 <- round(sapply(names(var_names), function(w) {
    riv_index <- which(regional_models[[w]]$river == "PCR")
    y_obs <- regional_models[[w]]$y_obs[riv_index, ]
    y_hat <- regional_models[[w]]$y_hat[riv_index, ]
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "fR2"
    )
}), 3L)

# Calculate funR2 performance of regional model on PCR - CV.
reg_on_pcr_funR2_cv <- round(sapply(names(var_names), function(w) {
    riv_index <- which(regional_models[[w]]$river == "PCR")
    y_obs <- regional_models[[w]]$y_obs[riv_index, ]
    y_hat <- regional_models[[w]]$y_hat_cv[riv_index, ]
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "fR2"
    )
}), 3L)

# Calculate funR2 performance of FRM model on PCR - Training.
frm_on_pcr_funR2 <- round(sapply(names(var_names), function(w) {
    riv_index <- which(frm_tiny_models[[w]]$river == "PCR")
    y_obs <- frm_tiny_models[[w]]$y_obs[riv_index, ]
    y_hat <- frm_tiny_models[[w]]$y_hat[riv_index, ]
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "fR2"
    )
}), 3L)

# Calculate performance of FRM model on PCR - CV.
frm_on_pcr_funR2_cv <- round(sapply(names(var_names), function(w) {
    riv_index <- which(frm_tiny_models[[w]]$river == "PCR")
    y_obs <- frm_tiny_models[[w]]$y_obs[riv_index, ]
    y_hat <- frm_tiny_models[[w]]$y_hat_cv[riv_index, ]
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "fR2"
    )
}), 3L)



# Calculate funR2 on SMR + PCR -------------------------------------------------


# Calculate funR2 performance of REG on SMR + PCR - Training
reg_on_reg_funR2 <- round(sapply(names(var_names), function(w) {
    y_obs <- regional_models[[w]]$y_obs
    y_hat <- regional_models[[w]]$y_hat
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "fR2"
    )
}), 3L)

# Calculate funR2 performance of REG on SMR + PCR - CV
reg_on_reg_funR2_cv <- round(sapply(names(var_names), function(w) {
    y_obs <- regional_models[[w]]$y_obs
    y_hat <- regional_models[[w]]$y_hat_cv
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "fR2"
    )
}), 3L)

# Calculate funR2 performance of FRM on SMR + PCR- Training
frm_on_reg_funR2 <- round(sapply(names(var_names), function(w) {
    y_obs <- frm_tiny_models[[w]]$y_obs
    y_hat <- frm_tiny_models[[w]]$y_hat
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "fR2"
    )
}), 3L)

# Calculate funR2 performance of FRM on SMR + PCR - CV
frm_on_reg_funR2_cv <- round(sapply(names(var_names), function(w) {
    y_obs <- frm_tiny_models[[w]]$y_obs
    y_hat <- frm_tiny_models[[w]]$y_hat_cv
    calc_fun_metric(
        y_obs  = y_obs,
        y_hat  = y_hat,
        metric = "fR2"
    )
}), 3L)


# Merge all funR2 results in a table -------------------------------------------


# Create a vector of three NAs to fill empty cells in table.
nas <- rep(NA, 3L)

# Create a matrix with the results.
table_funR2 <- matrix(
    data = c(
        smr_on_smr_funR2, smr_on_smr_funR2_cv,
        pcr_on_smr_funR2, nas,
        reg_on_smr_funR2, reg_on_pcr_funR2_cv,
        frm_on_smr_funR2, frm_on_smr_funR2_cv,
        pcr_on_pcr_funR2, pcr_on_pcr_funR2_cv,
        smr_on_pcr_funR2, nas,
        reg_on_pcr_funR2, reg_on_pcr_funR2_cv,
        frm_on_pcr_funR2, frm_on_pcr_funR2_cv,
        reg_on_reg_funR2, reg_on_reg_funR2_cv,
        frm_on_reg_funR2, frm_on_reg_funR2_cv
    ),
    ncol  = 6L,
    byrow = TRUE
)

# Add columns names.
colnames(table_funR2) <- c(
    "Depth-Train", "D50-Train", "Velocity-Train",
    "Depth-CV",    "D50-CV",    "Velocity-CV"
)

# Add row names.
rownames(table_funR2) <- c(
    "SMR_on_SMR",
    "PCR_on_SMR",
    "REG_on_SMR",
    "FRM_on_SMR",
    "PCR_on_PCR",
    "SMR_on_PCR",
    "REG_on_PCR",
    "FRM_on_PCR",
    "REG_on_REG",
    "FRM_on_REG"
)


# Export funR2 table -----------------------------------------------------------


data.table::fwrite(
    x    = as.data.table(table_funR2, keep.rownames = TRUE),
    file = file.path("out", "tables", "table_3_performance_funR2.csv"),
    sep  = ";",
    dec  = "."
)

