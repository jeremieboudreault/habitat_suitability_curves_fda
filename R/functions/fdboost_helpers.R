# fdboost_helpers.R


# Helpers function to fit FDboost models.


# Project : habitat_suitability_curves_fda
# Author  : Jeremie Boudreault
# Email   : Jeremie.Boudreault [at] ete.inrs [dot] ca
# Depends : R (v3.6.3)
# License : CC BY-NC-ND 4.0



# Generate folds for cross-validation ------------------------------------------


generate_folds <- function(n_obs, n_folds) {

    # Set seed before generating folds so that results are always identical.
    set.seed(2912L)

    # Generate ordered folds.
    folds <- rep_len(
        x          = seq_len(n_folds),
        length.out = n_obs
    )

    # Shuffle the folds except in the leave-one-out case.
    if (n_folds != n_obs) {
        folds <- sample(
            x       = folds,
            size    = n_obs,
            replace = FALSE
        )
    }

    # Return the folds.
    return(folds)

}


# Calculate metrics on functional predictions ----------------------------------


calc_fun_metric <- function(y_hat, y_obs, metric = "frmse") {

    # Tradiditonal R square (R2).
    if (metric == "R2") {
        return(
            1 - sum((y_obs - y_hat)^2) / sum((y_obs - mean(y_obs))^2)
        )
    }

    # Functional R square (fR2).
    if (metric == "fR2") {
        return(
            1 - sum((y_obs - y_hat)^2) / sum((y_obs - colMeans(y_obs))^2)
        )
    }

    # Functional Root Mean Square Error (fRMSE).
    if (metric == "frmse") {
        return(
            sqrt(mean((y_obs - y_hat)^2))
        )
    }

    # Functional Mean Absoluation Error (fMAE).
    if (metric == "fmae") {
        return(
            mean(abs(y_obs - y_hat))
        )
    }

}

