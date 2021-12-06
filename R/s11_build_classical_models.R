# s11_build_classical_models.R


# Step 11 : Build classical models at local and regional level for comparison.


# Project : habitat_suitability_curves_fda
# Author  : Jeremie Boudreault
# Email   : Jeremie.Boudreault [at] ete.inrs [dot] ca
# Depends : R (v3.6.3)
# License : CC BY-NC-ND 4.0



# Library ----------------------------------------------------------------------


library(data.table)
library(ggplot2)
library(ggpubr)


# Functions --------------------------------------------------------------------


source(file.path("R", "functions", "globals.R"))
source(file.path("R", "functions", "internals.R"))
source(file.path("R", "functions", "plot_helpers.R"))


# Imports ----------------------------------------------------------------------


# Habitat variables.
hab <- qs::qread(
    file = file.path("out", "tmp", "s05_hab_var.qs")
)

# Range of the variables.
hab_range <- qs::qread(
    file = file.path("out", "tmp", "s05_hab_var_range.qs")
)

# Fitted HSC curves.
fd_curves <- qs::qread(
    file = file.path("out", "tmp", "s06_fd_curves_list.qs")
)


# Fit classical HSC model ------------------------------------------------------


.fit_classical_model <- function(var, rivers) {

    # Extract habitat variables.
    hab_sub <- hab[VARIABLE == var & RIVER %in% rivers, ]

    # Number of sites.
    sites <- sort(unique(hab_sub$SITE_INTERNAL))
    n_sites <- length(sites)

    # Fit KDE (avail.)
    kde_avail <- fit_kde(
        x       = hab_sub[TYPE == "AVAILABLE", VALUE],
        range   = hab_range[[var]],
        adjust  = adjust_list[[var]],
    )

    # Fit KDE (select.)
    kde_selec <- fit_kde(
        x       = hab_sub[TYPE == "SELECTED", VALUE],
        range   = hab_range[[var]],
        adjust  = adjust_list[[var]],
    )

    # Fit Pref function.
    pref <- fit_pref(
        avail = hab_sub[TYPE == "AVAILABLE", VALUE],
        selec = hab_sub[TYPE == "SELECTED", VALUE],
        var   = var
    )

    # Set y_hat as the HSC curves.
    y_hat <- Reduce(
        f     = function(x1, x2) rbind(x1, pref$Y),
        x     = seq_len(n_sites - 1L),
        init  = rbind(pref$Y)
    )

    # Extract y_obs from the fitted curves.
    y_obs <- fd_curves[[var]]$Y[which(fd_curves[[var]]$RIVER %in% rivers), ]

    # Set leave-one-out predictions.
    y_hat_cv <- do.call(rbind, lapply(sites, function(site) {

        # Fit Preference.
        pref <- fit_pref(
            avail    = hab_sub[SITE_INTERNAL != site & TYPE == "AVAILABLE", VALUE],
            selec    = hab_sub[SITE_INTERNAL != site & TYPE == "SELECTED", VALUE],
            var      = var
        )

        # Save Preference function as the estimate for this site.
        return(pref$Y)

    }))

    # Extract rivers.
    river <- fd_curves[[var]]$RIVER[which(fd_curves[[var]]$RIVER %in% rivers)]

    # Return results for the model.
    return(
        list(
            x         = pref$X,
            y_obs     = y_obs,
            y_hat     = y_hat,
            y_hat_cv  = y_hat_cv,
            river     = river,
            avail     = kde_avail$Y,
            selec     = kde_selec$Y
        )
    )

}


# Fit classical models ---------------------------------------------------------


# Local models - SMR.
local_models_smr <- lapply(
    X      = names(var_names),
    FUN    = .fit_classical_model,
    rivers = c("SMR")
)

# Add names.
names(local_models_smr) <- names(var_names)

# Local model - PCR.
local_models_pcr <- lapply(
    X   = names(var_names),
    FUN = .fit_classical_model,
    rivers = c("PCR")
)

# Add names.
names(local_models_pcr) <- names(var_names)

# Regional models.
regional_models <- lapply(
    X      = names(var_names),
    FUN    = .fit_classical_model,
    rivers = c("SMR", "PCR")
)

# Add names.
names(regional_models) <- names(var_names)


# Plot classical models --------------------------------------------------------


plot_classical_model <- function(models) {

    # Loop on each of the supplied models.
    p_all <- lapply(models, function(model) {

    # Loop on each habitat variables.
    p <- lapply(names(var_names), FUN = function(var) {

        # Subset the model
        model_sub <- model[[var]]

        # Extract rivers.
        rivers <- unique(model_sub$river)

        # Create a data.table with the relevant data.
        fd_curves_sub <- data.table::data.table(
            X    = c(model_sub$x, model_sub$x, model_sub$x),
            Y    = c(model_sub$avail, model_sub$selec, model_sub$y_hat[1L, ]),
            TYPE = c(
                rep("AVAILABLE", length(model_sub$x)),
                rep("SELECTED", length(model_sub$x)),
                rep("PREFERENCE", length(model_sub$x))
            ),
            VARIABLE  = var
        )

        # Subset data for the plot.
        hab_sub <- hab[
            RIVER %in% rivers &
            VARIABLE == var,
        ]

        # Extract range.
        rng <- unlist(hab_range[, var, with = FALSE])

        # Plot models.
        p <- ggplot(
            data    = fd_curves_sub,
            mapping = aes(x = X)
        ) +
        geom_histogram(
            data    = hab_sub,
            mapping = aes(
                x    = VALUE,
                y    = ..density..,
                fill = TYPE
            ),
            color       = "grey90",
            lwd         = 0.2,
            position    = "dodge",
            bins        = 8L,
            show.legend = FALSE
        ) +
        scale_x_continuous(
            limits = rng
        )

        # Extract maximum value of Y.
        scale_fac <- max(
            fd_curves_sub[TYPE %in% c("AVAILABLE", "SELECTED"), max(Y)],
            layer_scales(p)$y$range$range[2L]
        )

        # Update plot with lines and everything.
        p <- p + geom_line(
            data    = fd_curves_sub[TYPE %in% c("AVAILABLE", "SELECTED")],
            mapping = aes(
                x        = X,
                y        = Y,
                color    = TYPE,
                linetype = TYPE
            )
        ) +
        geom_line(
            data    = fd_curves_sub[TYPE %in% c("PREFERENCE")],
            mapping = aes(
                x        = X,
                y        = Y * scale_fac,
                color    = TYPE,
                linetype = TYPE
            )
        ) +
        facet_wrap(
            facets   = "VARIABLE",
            scales   = "free",
            labeller = labeller(VARIABLE = unlist(var_names))
        ) +
        labs(
            color    = "",
            linetype = "",
            x     = NULL,
            y     = NULL
        ) +
        scale_color_manual(
            values = hab_colors,
            breaks = names(hab_names),
            label  = ul(hab_names)
        ) +
        scale_fill_manual(
            values = hab_colors[1:2],
            breaks = names(hab_names)[1:2],
        ) +
        scale_y_continuous(
            sec.axis = sec_axis(
                trans  = ~.*1/scale_fac,
                breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))
        ) +
        scale_linetype_manual(
            values = c(1L, 1L, 3L),
            breaks = names(hab_names),
            label  = ul(hab_names)
        ) +
        custom_theme() +
        theme(
            axis.ticks.y.right = element_line(color = hab_colors[3L]),
            axis.text.y.right  = element_text(color = hab_colors[3L]),
            axis.title.y.right = element_text(color = hab_colors[3L]),
            axis.ticks.y.left  = element_line(color = hab_colors[2L]),
            axis.text.y.left   = element_text(color = hab_colors[2L]),
            axis.title.y.left  = element_text(color = hab_colors[2L]),
            plot.title        = element_text(hjust = 0)
        )

        # Add title.
        if (length(rivers) == 1L) {
            p <- p + xlab("")
        } else {
            p <- p + xlab(var_names_u[[var]])
        }

        # Add x labels.
        if (var == "DEPTH" & rivers[1L] == "SMR" & length(rivers) == 1L) {
            p <- p + ggtitle(
                "a) HSC-SM"
            )
        } else if (var == "DEPTH" & rivers[1L] == "PCR" & length(rivers) == 1L) {
            p <- p + ggtitle(
                "b) HSC-PC"
            )
        } else if (var == "DEPTH" & length(rivers) == 2L) {
            p <- p + ggtitle(
                "c) HSC-REG"
            )
        } else {
            p <- p + ggtitle("")
        }

        # Return plot.
        return(p)

    # Close the first loop.
    })

    # Return p.
    return(p)

    # Close the second loop
    })

    # Combine all plots.
    p_full <- ggpubr::ggarrange(
        plotlist      = unlist(p_all, recursive = FALSE),
        ncol          = length(var_names),
        nrow          = length(models),
        legend        = "bottom",
        common.legend = TRUE
    )

    # Add y_axis.
    p_full <- ggpubr::annotate_figure(
        p    = p_full,
        left  = text_grob(
            "Probability density function (PDF)",
            color = hab_colors[2L],
            rot   = 90,
        ),
        right = text_grob(
            "Habitat suitability curve (HSC)",
            color = hab_colors[3L],
            rot = 270
        )
    )

    # Print plot.
    print(p_full)

    # Return null.
    return(invisible(NULL))


}


# Plot all models --------------------------------------------------------------


# Combine all plots.
plot_classical_model(
    models = list(
        local_models_smr,
        local_models_pcr,
        regional_models
    )
)

# Save plot.
ggsave(
    file   = file.path("out", "plots", "fig_8_classical_models.pdf"),
    width  = 9L,
    height = 8L
)


# Exports results --------------------------------------------------------------


# Local HSC model SMR.
qs::qsave(
    x = local_models_smr,
    file = file.path("out", "tmp", "s11_local_models_smr.qs")
)

# Local HSC model PCR.
qs::qsave(
    x = local_models_pcr,
    file = file.path("out", "tmp", "s11_local_models_pcr.qs")
)

# Regional HSC model.
qs::qsave(
    x = regional_models,
    file = file.path("out", "tmp", "s11_regional_models.qs")
)

