# internals.R


# Internal functions and global variables.


# Project : habitat_suitability_curves_fsa
# Author  : Jeremie Boudreault
# Email   : JeremieBoudreault11@gmail.com
# Depends : R (v3.6.3)
# License : CC BY-NC-ND 4.0



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
