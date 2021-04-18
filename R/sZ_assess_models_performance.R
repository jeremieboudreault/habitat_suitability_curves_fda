# sZ_assess_models_performance.R


# Assess models performance.


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


# Calculate metrics for all models ---------------------------------------------


# Apply for each models.
performance <- lapply(
    X   = names(var_names),
    FUN = function(w) {

        # Extract observations and results.
        res <- frm_results[[w]]

        # Full model.
        full_perf <- calc_fun_metrics(
            y_hat  = res$full$y_hat,
            y_obs  = res$full$y_obs
        )

        # K-fold metrics.
        kfold_perf <- calc_fun_metrics(
            y_hat  = res$y_hat_kfold[, , which(res$mstops == res$mstop_best)],
            y_obs  = res$full$y_obs
        )

        # Return results.
        return(rbind(full_perf, kfold_perf))

    }
)

# Add names to performance.
names(performance) <- names(var_names)


# Export results ---------------------------------------------------------------


qs::qsave(
    x    = performance,
    file = file.path("out", "tmp", "sZ_models_performance.qs")
)

