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


# Fit a FDboost model with k-fold cross-validation -----------------------------


# Function-on-function regression model to fit Y(s) ~ B(s, r) X(r) while
# performing a k-fold cross-validation to find optimal mstop based on a given
# performance criterion.

FDboost_kfold <- function(
    data,
    n_folds,
    fdboost_opt
) {

    # Checks on dimension of supplied data.
    if (nrow(data$Y) =! nrow(data$X)   |
        ncol(data$Y) =! length(data$s) |
        ncol(data$X) =! length(data$r)) {
        stop("Dimension of supplied data do not match.", call. = FALSE)
    }

    # Number of observations.
    n_obs <- nrow(data$Y)

    # Number of points for Y.
    n_s <- length(data$s)

    # Check on the number of folds.
    if (!is.integer(n_folds) | n_folds < 1L | n_folds > n_obs) {
        stop("Invalid number of folds.", call. FALSE)
    }

    # Generate folds.
    folds <- generate_fold(n_obs, n_folds)

    # Set cluster for parralel computation.
    cl <- parallel::makeCluster(parallel::detectCores() - 1L)

    # Apply in parallel the .FDboost_kfold_k function.
    res <- parallel::parLapplyLB(
        X            = seq.int(1L, n_folds),
        fun          = .FDboost_kfold_k,
        data         = data,
        folds        = folds,
        fdboost_opts = fdboost_opts,
        cl           = cl
    )

    # Close cluster.
    parallel::stopCluster(cl)

    # Create an array to rearrange results in a nicer format.
    results <- array(NA, dim = c(n_obs, n_s, fdboost_opts$mstop_max))

    # Rearrange results to the current position in the array.
    for (i in seq_len(length(res))) {
        results[i, , ] <- t(res[[i]])
    }

    # Calculate the chosen metric.
    error <- apply(
        X      = results,
        MARGIN = 3L,
        FUN    = calc_fun_metric,
        y_obs  = data$Y,
        err    = fdboost_opts$metric
    )

    # Find optimal mstop based on the value of the metric.
    mstop_best <- which.min(error)

    # Return results.
    return(structure(results,
        mstop_best = mstop_best
    ))

}
