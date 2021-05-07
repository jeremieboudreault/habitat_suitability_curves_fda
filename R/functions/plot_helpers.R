# plot_helpers.R


# Internal helpers function for plots.


# Project : habitat_suitability_curves_fda
# Author  : Jeremie Boudreault
# Email   : Jeremie.Boudreault [at] ete.inrs [dot] ca
# Depends : R (v3.6.3)
# License : CC BY-NC-ND 4.0



# ggplot helpers ---------------------------------------------------------------


# rotate_x_axis : Rotate the ticks of the x axis on a ggplot2 graph.
rotate_x_axis <- ggplot2::theme(
    axis.text.x = element_text(
        angle = 90L,
        vjust = 0.5,
        hjust = 1L
    )
)


# Custom theme -----------------------------------------------------------------


# custom_theme() : A customised theme for ggplot2.
custom_theme <- function(x) {
    theme_light() +
    theme(
        plot.title   = element_text(
            face  = "bold",
            hjust = 0.5,
            size  = 12L
        ),
        plot.subtitle = element_text(
            hjust = 0.5,
            size  = 11L
        ),
        strip.text = element_text(
            colour = "black"
        ),
        strip.background = element_rect(
            fill = "#c1c1c1"
        ),
        legend.position = "bottom"
    )
}
