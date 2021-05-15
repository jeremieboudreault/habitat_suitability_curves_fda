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

    # Return list.
    return(
        list(
            x         = x,
            y_obs     = y_obs,
            y_hat     = y_hat,
            y_hat_cv  = y_hat_cv
        )
    )

})


# Calculate metrics for all models ---------------------------------------------


calc_performance <- function(models, var, metric, cv) {

        # Extract individual model.
        model <- get(models)[[var]]

        # Extract y_hat if cv is TRUE or not.
        y_hat <- if (cv) model$y_hat_cv else model$y_hat

        # Return metric.
        return(calc_fun_metric(
            y_hat  = y_hat,
            y_obs  = model$y_obs,
            metric = metric
        ))

}

# Expand a grid for all metrics.
performance = as.data.table(expand.grid(
    models = c("local_models_smr", "local_models_pcr", "regional_models", "frm_tiny_models"),
    var    = names(var_names),
    metric = c("fR2", "R2"),
    cv     = c("TRUE", "FALSE"),
    stringsAsFactors = FALSE
))

# Calculate performance for each combinaison.
performance[, PERF := sapply(seq_len(nrow(performance)), function(w) do.call(calc_performance, performance[w, ]))]

# Melt.
perf_dt <- data.table::dcast.data.table(performance,
    formula   = models ~ cv + metric + var,
    value.var = "PERF"
)

# Rearrange the row order.
perf_dt <- perf_dt[c(3L, 2L, 4L, 1L), ]

# Rearrange the columns.
perf_dt <- perf_dt[, c(1L, 3L, 2L, 4L, 6L, 5L, 7L, 9L, 8L, 10L, 12L, 11L, 13L)]

# Round values prior to export.
perf_dt[, 2:13] <- round(perf_dt[, 2:13])


# Export table to .csv ---------------------------------------------------------


data.table::fwrite(
    x    = perf_dt,
    file = file.path("out", "tables", "table_2_performance.csv"),
    sep  = ";",
    dec  = "."
)

