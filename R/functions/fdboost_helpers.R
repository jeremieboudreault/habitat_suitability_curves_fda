# fdboost_helpers.R


# Helpers function to fit FDboost models.


# Project : habitat_suitability_curves_fda
# Author  : Jeremie Boudreault
# Email   : Jeremie.Boudreault [at] ete.inrs [dot] ca
# Depends : R (v3.6.3)
# License : CC BY-NC-ND 4.0


# Generate folds for cross-validaiton ------------------------------------------


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