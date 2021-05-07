# internals.R


# Internal functions.


# Project : habitat_suitability_curves_fda
# Author  : Jeremie Boudreault
# Email   : Jeremie.Boudreault [at] ete.inrs [dot] ca
# Depends : R (v3.6.3)
# License : CC BY-NC-ND 4.0



# Data functions ---------------------------------------------------------------


# dtlapply : apply a function and return the data.table.
dtlapply <- function(X, FUN, ...) {
    return(do.call(rbind, lapply(X = X, FUN = FUN, ...)))
}


# get_range : Get the range of a given variable.
get_range <- function(x, liminf = TRUE, factor = 1L) {
    if (liminf == TRUE) liminf <- min(x, na.rm = TRUE) / factor
    return(c(liminf, max(x, na.rm = TRUE) * factor))
}


# abind : Bind matrices into a array.
abind <- function(...) {
    DescTools::Abind(..., along = 3L)
}


# fit_kde : Fit a KDE and scale it to 0 - 1.
fit_kde <- function(x, range, npoints = 2^7, adjust = 4L, scale = FALSE) {

    # Calculate fit.
    fit <- stats::density(
        x      = unlist(x),
        bw     = "nrd0",
        adjust = adjust,
        kernel = "gaussian",
        n      = npoints,
        from   = unlist(range)[1],
        to     = unlist(range)[2],
    )

    # Scale y variable if asked.
    if (scale) fit$y <- fit$y / max(fit$y)

    # Return result as a data.table.
    return(data.table::data.table(X = fit$x, Y = fit$y))

}

