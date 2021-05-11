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


calc_fun_metric <- function(y_hat, y_obs, metric = "fRMSE") {

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
    if (metric == "fRMSE") {
        return(
            sqrt(mean((y_obs - y_hat)^2))
        )
    }

    # Functional Mean Absoluation Error (fMAE).
    if (metric == "fMAE") {
        return(
            mean(abs(y_obs - y_hat))
        )
    }

}

# Wrapper function the the above one returning a list of various metrics.
calc_fun_metrics <- function(y_obs, y_hat, metrics = names(metric_names)) {

    return(
        sapply(
            X     = metrics,
            FUN   = calc_fun_metric,
            y_obs = y_obs,
            y_hat = y_hat
        )
    )

}


# Generate bootstrap sample ----------------------------------------------------


generate_bs_sample <- function(l) {
    n_obs <- nrow(l$Y)
    boot_i <- sample(seq_len(n_obs), size = n_obs, replace = TRUE)
    l$Y <- l$Y[boot_i, ]
    l$X <- l$X[boot_i, ]
    return(l)
}


# Create options to fit a FDboost ----------------------------------------------


.fdboost_opts <- function(
        mstop_max     = 500L,
        mstop_step    = 1L,
        learning_rate = 0.1,
        metric        = "fRMSE",
        n_folds       = "loo",
        knots         = 10L,
        degree        = 3L,
        differences   = 2L
) {
    return(
        list(
            mstop_max     = mstop_max,
            mstop_step    = mstop_step,
            learning_rate = learning_rate,
            metric        = metric,
            n_folds       = n_folds,
            knots         = knots,
            degree        = degree,
            differences   = differences
        )
    )
}


# Fit a FDboost model with k-fold cross-validation -----------------------------


# Function-on-function regression model to fit Y(s) ~ B(s, r) X(r) while
# performing a k-fold cross-validation to find optimal mstop based on a given
# performance criterion.

FDboost_kfold <- function(
    data,
    fdboost_opts
) {

    # Checks on dimension of supplied data.
    if (nrow(data$Y) != nrow(data$X)   |
        ncol(data$Y) != length(data$s) |
        ncol(data$X) != length(data$r)) {
        stop("Dimension of supplied data do not match.", call. = FALSE)
    }

    # Number of observations.
    n_obs <- nrow(data$Y)

    # Number of points for Y.
    n_s <- length(data$s)

    # Extract number of folds from fdboost_opts.
    n_folds <- fdboost_opts$n_folds

    # Check on the number of folds.
    if (n_folds == "loo") {
        n_folds <- n_obs
    } else if (!is.integer(n_folds) | n_folds < 1L | n_folds > n_obs) {
        stop("Invalid number of folds.\n",
             "> Specify the number of folds or use 'loo' for leave-one-out ",
             "cross-validation.",
        call. = FALSE)
    }

    # Generate folds.
    folds <- generate_folds(n_obs, n_folds)

    # Set values of mstop.
    mstops <- seq.int(
        from  = fdboost_opts$mstop_step,
        to    = fdboost_opts$mstop_max,
        by    = fdboost_opts$mstop_step
    )

    # Set cluster for parralel computation.
    cl <- parallel::makeCluster(parallel::detectCores() - 1L)

    # Apply in parallel the .FDboost_kfold_k function.
    res <- parallel::parLapplyLB(
        X            = seq_len(n_folds),
        fun          = .FDboost_kfold_k,
        data         = data,
        folds        = folds,
        fdboost_opts = fdboost_opts,
        mstops       = mstops,
        cl           = cl
    )

    # Close cluster.
    parallel::stopCluster(cl)

    # Create an array to rearrange results in a nicer format.
    results <- array(NA, dim = c(n_obs, n_s, length(mstops)))

    # Rearrange results to the current position in the array.
    for (mstop_i in seq_along(mstops)) {
        for (fold_k in seq_len(n_folds)) {
            results[which(fold_k == folds), , mstop_i] <- res[[fold_k]][[mstop_i]]
        }
    }

    # Calculate the chosen metric.
    metric <- apply(
        X      = results,
        MARGIN = 3L,
        FUN    = calc_fun_metric,
        y_obs  = data$Y,
        metric = fdboost_opts$metric
    )

    # Find optimal mstop based on the value of the metric.
    mstop_best <- mstops[which.min(metric)]

    # Return results.
    return(
        list(
            y_hat_kfold   = results,
            fdboost_opts  = fdboost_opts,
            mstops        = mstops,
            metric        = metric,
            mstop_best    = mstop_best
        )
    )

}


# Internal function to fit a FDboost on kth fold -------------------------------


.FDboost_kfold_k <- function(
    fold_k,
    data,
    folds,
    fdboost_opts,
    mstops
) {

    # Create train dataset.
    data_train <- list()
    data_train$Y <- data$Y[which(folds != fold_k), ]
    data_train$X <- data$X[which(folds != fold_k), ]
    data_train$s <- data$s
    data_train$r <- data$r

    # Create valid dataset.
    data_valid <- list()
    data_valid$X <- rbind(data$X[which(folds == fold_k), ])
    data_valid$s <- data$s
    data_valid$r <- data$r

    # Fit model.
    fit <- FDboost::FDboost(
        formula     = Y ~ 1L + bsignal(
            x           = X,
            s           = r,
            inS         = "smooth",
            degree      = fdboost_opts[["degree"]],
            knots       = fdboost_opts[["knots"]],
            differences = fdboost_opts[["differences"]],
            cyclic      = FALSE
        ),
        timeformula = ~ bbs(s),
        data        = data_train,
        control     = mboost::boost_control(
            mstop = max(mstops),
            nu    = fdboost_opts$learning_rate
        )
    )

    # Calculate prediction error for all mstop in a list.
    results <- lapply(
        X   = rev(mstops),
        FUN = function(mstop) {

            # Update the model.
            fit_mstop <- fit[mstop]

            # Predict on "valid" dataset.
            y_hat <- predict(
                object  = fit_mstop,
                newdata = data_valid,
                type    = "response"
            )

            # Delete attribute "offset".
            attr(y_hat, "offset") <- NULL

            # Return as a matrix.
            return(y_hat)

        }
    )

    # Reverse the results because we want them from min to max_stop.
    results <- rev(results)

    # Output file when completed.
    write.table("", file.path("cache", sprintf("Fold_%s_completed.txt", fold_k)))

    # Return results.
    return(results)

}


# Full model -------------------------------------------------------------------


.FDboost <- function(
    data,
    fdboost_opts,
    mstop_best
) {

    # Fit model.
    fit <- FDboost::FDboost(
        formula     = Y ~ 1L + bsignal(
            x           = X,
            s           = r,
            inS         = "smooth",
            degree      = fdboost_opts[["degree"]],
            knots       = fdboost_opts[["knots"]],
            differences = fdboost_opts[["differences"]],
            cyclic      = FALSE
        ),
        timeformula = ~ bbs(s),
        data        = data,
        control     = mboost::boost_control(
            mstop = mstop_best,
            nu    = fdboost_opts$learning_rate
        )
    )

    # Calculate prediction error for all mstop in a list.
    y_hat <-  predict(
        object  = fit,
        newdata = data,
        type    = "response"
    )

    # Delete attribute "offset".
    attr(y_hat, "offset") <- NULL

    # Return as a list.
    return(
        list(
            fit   = fit,
            y_hat = y_hat,
            y_obs = data$Y
        )
    )

}


# Extract coefficents ----------------------------------------------------------


.extract_coef <- function(fit_coef) {

    # Extract x, y, z values.
    z <- fit_coef$smterms$`bsignal(X) %O% bbs(s)`$value
    rownames(z) <- fit_coef$smterms$`bsignal(X) %O% bbs(s)`$y
    colnames(z) <- fit_coef$smterms$`bsignal(X) %O% bbs(s)`$x

    # Add proper names to the matrix.
    names(dimnames(z)) <- c("Y", "X")

    # Generate a data.frame for plotting.
    z_df <- as.data.table(reshape2::melt(z, value.name = "Z"))

    # Return "z_df".
    return(z_df)

}

