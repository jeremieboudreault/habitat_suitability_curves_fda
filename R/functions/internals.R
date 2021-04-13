# internals.R


# Internal functions and global variables.


# Project : habitat_suitability_curves_fsa
# Author  : Jeremie Boudreault
# Email   : JeremieBoudreault11@gmail.com
# Depends : R (v3.6.3)
# License : CC BY-NC-ND 4.0



# Globals ----------------------------------------------------------------------


# List of variables for this project.
var_names <- list(
    VELOCITY = "Velocity (m/s)",
    DEPTH    = "Depth (cm)",
    D50      = "D50 (mm)"
)


# List of the names for available / selected.
hab_names <- list(
    AVAILABLE = "Available",
    SELECTED  = "Selected"
)


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


# ggplot helpers ---------------------------------------------------------------


# rotate_x_axis : Rotate the ticks of the x axis on a ggplot2 graph.
rotate_x_axis <- ggplot2::theme(
    axis.text.x = element_text(
        angle = 90L,
        vjust = 0.5,
        hjust = 1L
    )
)


# legend_bottom : Bring legend to the bottom of the plot.
legend_bottom <- ggplot2::theme(
    legend.position = "bottom"
)

