# s7_extract_frm_coefficients.R


# Extract FRM coefficients and compute bootstrap 95% CI.


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


# Extract coefficients ---------------------------------------------------------


.extract_coef <- function(var_name) {

    # Extract results from the list of the 3 models.
    res <- frm_results[[var_name]]

    # Extract full fitted model from res.
    fit <- res$full$fit

    # Extract coefficient from fit.
    fit_coef <- coef(fit)

    # Extract x, y, z values.
    z <- fit_coef$smterms$`bsignal(X) %O% bbs(s)`$value
    rownames(z) <- fit_coef$smterms$`bsignal(X) %O% bbs(s)`$y
    colnames(z) <- fit_coef$smterms$`bsignal(X) %O% bbs(s)`$x

    # Add proper names to the matrix.
    names(dimnames(z)) <- c("Y", "X")

    # Generate a data.frame for plotting.
    z_df <- reshape2::melt(z, value.name = "Z")

    # Add the name of variable.
    z_df$MODEL <- var_names_u[[var_name]]

    # Return z_df as a data.table.
    return(data.table::setDT(z_df))

}


# Extract coefficients for all three models ------------------------------------


coef_list <- lapply(
    X   = names(var_names),
    FUN = .extract_coef
)




