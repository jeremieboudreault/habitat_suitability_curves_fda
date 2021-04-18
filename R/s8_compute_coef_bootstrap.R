# s8_compute_coef_bootstrap.R


# Compute regression coefficient bootstrap to calculate the standard error.


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


# Parameters -------------------------------------------------------------------


# Number of bootstrap.
n_bs <- 1000L


# Imports ----------------------------------------------------------------------


# Fitted functional models.
frm_results <- qs::qread(
    file = file.path("out", "tmp", "s6_frm_results.qs")
)

# Functional observations.
fd_curves_list <- qs::qread(
    file = file.path("out", "tmp", "s5_fd_curves_list.qs")
)


# Generate bootstrap sample ----------------------------------------------------


generate_bs_sample <- function(l) {
    n_obs <- nrow(l$Y)
    boot_i <- sample(seq_len(n_obs), size = n_obs, replace = TRUE)
    l$Y <- l$Y[boot_i, ]
    l$X <- l$X[boot_i, ]
    return(l)
}


# Compute bootstrap of coefficients --------------------------------------------


# Apply over all variables.
bs_res <- lapply(
    X = names(var_names),
    FUN = function(var) {

        # Apply over all bootstrap (n_bs).
        do.call(what = abind, args = lapply(
            X   = seq_len(n_bs),
            FUN = function(i) {

                # Fit functional model.
                obj <- .FDboost(
                    data         = generate_bs_sample(fd_curves_list[[var]]),
                    fdboost_opts = frm_results[[var]]$fdboost_opts,
                    mstop_best   = frm_results[[var]]$mstop_best
                )

                # Fitted coefficients.
                coef_fit <- coef(obj$fit)

                # Extract matrix.
                coef_mat <- coef_fit$smterms$`bsignal(X) %O% bbs(s)`$value

                # Add row and column names.
                rownames(coef_mat) <- coef_fit$smterms$`bsignal(X) %O% bbs(s)`$y
                colnames(coef_mat) <- coef_fit$smterms$`bsignal(X) %O% bbs(s)`$x

                # Return coef_mat.
                return(coef_mat)

            }
        ))
    }
)

# Add names to the results
names(bs_res) <- names(var_names)


