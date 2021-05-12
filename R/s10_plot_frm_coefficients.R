# s10_plot_frm_coefficients.R


# Step 10 : Plot FRM coefficients with bootstrap error.


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

# Bootstrap estimates of the coefficients.
bs_results <- qs::qread(
    file = file.path("out", "tmp", "s09_frm_coef_bootstrap.qs")
)


# Plot regression coefficients with standard error -----------------------------


plot_coef  <- function(var) {

    # Extract both intercept and surface.
    coef_fit <- coef(frm_results[[var]]$full$fit,
        n1 = n_val_coef,
        n2 = n_val_coef
    )

    # Intercept.
    intercept_fit <- data.table::data.table(
        X     = round(coef_fit$offset$x, 3L),
        Y     = coef_fit$offset$value
    )

    # Bootstrap standard error.
    std <- apply(
        X      = bs_results[[var]],
        FUN    = sd,
        MARGIN = c(1L, 2L)
    )

    # Melt into a data.table.
    std_dt <- data.table::setDT(reshape2::melt(std))

    # Update names.
    names(std_dt) <- c("X", "Y", "Z")

    # Extract intercept std.
    intercept_std <- std_dt[Y == "intercept", ]
    intercept_std <- intercept_std[, .(X = X, SD = Z)]
    intercept_std[, X := round(as.numeric(as.character(X)), 3L)]

    # Merge with intercept.
    intercept <- data.table::merge.data.table(
        x = intercept_fit,
        y = intercept_std,
        by = "X"
    )

    # Plot intercept.
    plot_intercept <- ggplot(
        data    = intercept,
        mapping = aes(
            x = X,
            y = Y,
        )
    ) +
    geom_line(
        lwd = 0.8
    ) +
    geom_ribbon(
        mapping = aes(
            ymin = Y - SD,
            ymax = Y + SD
        ),
        alpha = 0.15
    ) +
    labs(
        title    = paste0(
            letters[which(var == names(var_names))], ") ", var_names[[var]]
        ),
        y        = "",
        x        = var_names_u[[var]]
    ) +
    scale_color_manual(values = "black") +
    scale_fill_manual(values = "black") +
    facet_wrap(
        facets = ~ "Intercept",
        nrow   = 1L,
        ncol   = 1L,
        scales = "free",
        labeller = labeller(MODEL = unlist(var_names_u))
    ) +
    custom_theme() +
    theme(
        plot.title = element_text(hjust = 0)
    )

    # Add PDF to ylab if the second plot.
    if (var == "D50") {
        plot_intercept <- plot_intercept +
            ylab("Probability density function (PDF)")
    }

    # Extract surface coefficients.
    surface_fit <- .extract_coef(coef_fit)
    surface_fit[, TYPE := "Regression coefficient"]

    # Extract surface std.
    surface_std <- std_dt[Y != "intercept", ]
    surface_std <- surface_std[, Y := as.numeric(as.character(Y)) ]
    surface_std[, TYPE := "Standard error"]

    # Combine both table.
    surface <- rbind(surface_fit, surface_std)

    # Generate colors for the plot.
    colfun <- grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(9L, "RdBu")))

    # Plot surface.
    plot_surface <-   ggplot(
        data    = surface,
        mapping = aes(
            x    = X,
            y    = Y,
            z    = Z,
            fill = Z
        )
    ) +
    geom_raster(
        alpha = 1L
    ) +
    geom_abline(
        slope     = 1L,
        intercept = 0L,
        lty       = 2L,
        lwd       = 0.1
    ) +
    scale_fill_gradientn(
        colors  = colfun(100L),
        limits  = c(-max(abs(surface$Z)), max(abs(surface$Z))),
        guide   = guide_colourbar(barheight = 10L),

    ) +
    scale_x_continuous(
        expand = c(0.001, 0.001)
    ) +
    scale_y_continuous(
        expand = c(0.001, 0.001)
    ) +
    labs(
        title    = "",
        y        = paste0(var_names_u[[var]], " (selected)"),
        x        = paste0(var_names_u[[var]], " (available)"),
        fill      = ""
    ) +
    facet_wrap(
        facets = ~ TYPE,
        nrow   = 1L,
        ncol   = 2L,
        scales = "free"
    ) +
    custom_theme() +
    theme(
        panel.border     = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line        = element_line(colour = "white"),
        legend.position  = "right"
    )

    # Combine both plots.
    p <- ggpubr::ggarrange(
        plotlist = list(plot_intercept, plot_surface),
        ncol     = 2L,
        widths   = c(1L, 2.2)
    )

    # Return plot.
    return(p)

}


# Merge all plots --------------------------------------------------------------


# Generate all plots.
ps <- lapply(names(var_names), plot_coef)

# Plot.
ggpubr::ggarrange(plotlist = ps, nrow = 3L)

# Save plot.
ggsave(
    file   = file.path("out", "plots", "fig_8_frm_coef_bs.pdf"),
    width  = 9L,
    height = 8L
)

