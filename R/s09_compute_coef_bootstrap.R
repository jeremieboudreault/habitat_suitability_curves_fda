# s09_compute_coef_bootstrap.R


# Step 09 : Compute standard effort of regression coefficient with bootstrap.


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


source(file.path("R", "functions", "globals.R"))
source(file.path("R", "functions", "internals.R"))
source(file.path("R", "functions", "plot_helpers.R"))
source(file.path("R", "functions", "fdboost_helpers.R"))


# Imports ----------------------------------------------------------------------


# Fitted functional models.
frm_results <- qs::qread(
    file = file.path("out", "tmp", "s08_frm_results_full.qs")
)

# Functional observations.
fd_curves_list <- qs::qread(
    file = file.path("out", "tmp", "s06_fd_curves_list.qs")
)


# Compute bootstrap of coefficients --------------------------------------------


# Merge two lists together.
fd_objs <- lapply(seq_along(frm_results), function(w) {
    list(frm_results[[w]], fd_curves_list[[w]])
})

# Set cluster for parallel computation.
cl <- parallel::makeCluster(parallel::detectCores() - 1L)

# Apply over all variables.
bs_results <- parallel::parLapply(
    cl  = cl,
    X   = fd_objs,
    fun = function(fd_obj) {

        # Load functions.
        source(file.path("R", "functions", "internals.R"))
        source(file.path("R", "functions", "fdboost_helpers.R"))
        source(file.path("R", "functions", "globals.R"))

        # Apply over all bootstrap (n_bs).
        do.call(what = abind, args = lapply(
            X    = seq_len(n_bs),
            FUN  = function(i) {

                # Fit functional model.
                obj <- .FDboost(
                    data         = generate_bs_sample(fd_obj[[2L]]),
                    fdboost_opts = fd_obj[[1L]]$fdboost_opts,
                    mstop_best   = fd_obj[[1L]]$mstop_best
                )

                # Fitted coefficients.
                coef_fit <- coef(obj$fit, n1 = n_val_coef, n2 = n_val_coef)

                # Extract matrix.
                coef_mat <- coef_fit$smterms$`bsignal(X) %O% bbs(s)`$value

                # Add row and column names.
                rownames(coef_mat) <- coef_fit$smterms$`bsignal(X) %O% bbs(s)`$y
                colnames(coef_mat) <- coef_fit$smterms$`bsignal(X) %O% bbs(s)`$x

                # Extract intercept.
                intercept <- coef_fit$offset$value

                # Cbind with the coef_mat.
                coef_mat <- cbind(intercept, coef_mat)

                # Return coef_mat.
                return(coef_mat)

            }
        ))
    }
)

# Close cluster.
parallel::stopCluster(cl)

# Add names to the results.
names(bs_results) <- names(var_names)


# Export results ---------------------------------------------------------------


qs::qsave(
    x    = bs_results,
    file = file.path("out", "tmp", "s09_frm_coef_bootstrap.qs")
)


