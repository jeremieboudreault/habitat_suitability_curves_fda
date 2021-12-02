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
    X   = names(var_names),
    FUN = .fit_classical_model,
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


plot_classical_model <- function(models, rivers) {

    # Create a data.table with the relevant data.
    model_dt <- data.table::data.table(
        X   = ul(lapply(models, function(w) w$x)),
        Y   = ul(lapply(models, function(w) w$y_hat[1L, ])),
        VAR = factor(
            x = rep(var_names, sapply(models, function(w) length(w$x))),
            levels = ul(var_names)
        )
    )

    # Update <VAR> of hab.
    hab[, VAR := factor(var_names[VARIABLE],  levels = ul(var_names))]

    # Plot models.
    ggplot(
        data    = model_dt,
        mapping = aes(
            x = X,
            y = Y
        )
    ) +
    geom_histogram(
        data    = hab[RIVER %in% rivers, ],
        mapping = aes(
            x    = VALUE,
            y    = ..density..,
            fill = "Distribution of selected habitat characteristics"
        ),
        color    = "grey90",
        lwd      = 0.3,
        position = "dodge",
        boundary = 0L,
        bins     = 12L,
        alpha    = 0.9
    ) +
    geom_line(
        mapping = aes(
            color = "Habitat suitability curve"
        ),
        lwd   = 1.2
    ) +
    labs(
        title = "",
        y     = NULL,
        x     = "",
        fill  = "",
        color = ""
    ) +
    facet_wrap(
        facets   = "VAR",
        scales   = "free"
    ) +
    scale_fill_manual(
        values = "#63B0CD"

    ) +
    scale_color_manual(
        values = "#257B97"
    ) +
    custom_theme()

}


# Plot all models --------------------------------------------------------------


# Generate plots.
ps <- list(
    plot_classical_model(local_models_smr, rivers = "SMR"),
    plot_classical_model(local_models_pcr, rivers = "PCR"),
    plot_classical_model(regional_models, rivers = c("SMR", "PCR"))
)

# Add xlab to last plot.
ps[[3L]] <- ps[[3L]] + xlab("Depth (cm)                                                  D50 (mm)                                                Velocity (m/s)")

# Combine all plots.
p <- ggpubr::ggarrange(
    plotlist      = ps,
    nrow          = 3L,
    labels        = c(
        "a) Local HSC model for SMR",
        "b) Local HSC model for PCR",
        "c) Regional HSC model"
    ),
    hjust         = -0.2,
    legend        = "bottom",
    common.legend = TRUE
)

# Annotate figure.
ggpubr::annotate_figure(p,
    left   = "Probability density function (PDF)",
)

# Save plot.
ggsave(
    file   = file.path("out", "plots", "fig_9_classical_models.pdf"),
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

