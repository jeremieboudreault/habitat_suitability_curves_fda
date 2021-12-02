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


# ul : a small and compact version of unlist().
ul <- function(x) {
    unlist(x, recursive = FALSE, use.names = FALSE)
}

# fit_kde : Fit a KDE to the data.
fit_kde <- function(x, range, npoints = 2^7, adjust = 1L, scale = FALSE) {

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


# fit_pref() : Fit a preference from 0 to 1 based on selec/avail.
fit_pref <- function(avail, selec, var) {

    # First we convert habitat values to categories with very narrow range.
    class <- class_list[[var]]
    avail_class <- round(avail/class) * class
    selec_class <- round(selec/class) * class

    # Create class of available and selected.
    avail_tbl <- as.data.table(table(avail_class))
    select_tbl <- as.data.table(table(selec_class))

    # Update columsn names.
    names(avail_tbl) <- c("VALUE", "N_AVAIL")
    names(select_tbl) <- c("VALUE", "N_SELEC")

    # Merge both table.
    pref_tbl <- select_tbl[avail_tbl, on = "VALUE"]
    pref_tbl[is.na(N_SELEC), N_SELEC := 0]

    # Compute frequency.
    pref_tbl[, F_AVAIL := N_AVAIL / sum(N_AVAIL)]
    pref_tbl[, N_SELEC := N_SELEC / sum(N_SELEC)]
    pref_tbl[, PREF    := N_SELEC / F_AVAIL]

    # We want to transfer the preference values to fit a KDE.
    mult <- find_lcm(pref_tbl$PREF)

    # Convert to values.
    pref_tbl[, N_PSEUDO_PREF := round(PREF * mult, 0)]

    # Convert to values.
    pref_val <- as.numeric(unlist(sapply(
        X   = 1:nrow(pref_tbl),
        FUN = function(w) rep(pref_tbl$VALUE[w], length.out = pref_tbl$N_PSEUDO_PREF[w])
    )))

    # Fit KDE (pref.)
    pref <- fit_kde(
        x       = pref_val,
        range   = hab_range[[var]],
        adjust  = adjust_list[[var]] + adjust_pref
    )

    # Convert to 0 to 1.
    pref[, Y := Y/max(Y)]

    # Return the preference curve.
    return(pref)

}


# Find LCM ---------------------------------------------------------------------


find_lcm <- function(x) {
    test_val <- 1:100
    for (test in test_val) {
        x_test <- x * test
        if (all(x_test - as.integer(x_test) == 0)) {
            return(test)
        }
    }
    warning("No LCM found in 1:10. Returning 30.")
    return(30L)
}
