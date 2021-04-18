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


# Parameters (Number of bootstrap) ---------------------------------------------


n_bs <- 1000L



