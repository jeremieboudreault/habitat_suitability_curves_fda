# internals.R


# Internal functions and global variables.


# Project : habitat_suitability_curves_fsa
# Author  : Jeremie Boudreault
# Email   : JeremieBoudreault11@gmail.com
# Depends : R (v3.6.3)
# License : CC BY-NC-ND 4.0



# Globals ----------------------------------------------------------------------


# List of variables for this projecté
var_list <- list(
    VELOCITY = "Velocity (m/s)",
    DEPTH    = "Depth (cm)",
    D50      = "D50 (mm)"
)


# Functions --------------------------------------------------------------------


# dtlapply : apply a function and return the data.table.
dtlapply <- function(X, FUN, ...) {
    return(do.call(rbind, lapply(X = X, FUN = FUN, ...)))
}


# rotate_x_axis : Rotate the ticks of the x axis on a ggplot2 graph.
rotate_x_axis <- ggplot2::theme(
    axis.text.x = element_text(
        angle = 90L,
        vjust = 0.5,
        hjust = 1L
    )
)

# get_range : Get the range of a given variable.
get_range <- function(x, liminf = TRUE, factor = 1L) {
    if (liminf == TRUE) liminf <- min(x, na.rm = TRUE) / factor
    return(c(liminf, max(x, na.rm = TRUE) * factor))
}

