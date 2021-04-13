# s4_generate_fd_curves.R


# Generate functional curves for the further analysis.


# Project : habitat_suitability_curves_fda
# Author  : Jeremie Boudreault
# Email   : Jeremie.Boudreault [at] ete.inrs [dot] ca
# Depends : R (v3.6.3)
# License : CC BY-NC-ND 4.0



# Library ----------------------------------------------------------------------


library(data.table)
library(ggplot2)
library(ggforce)


# Functions --------------------------------------------------------------------


source(file.path("R", "functions", "internals.R"))


# Imports ----------------------------------------------------------------------


# Subsetted data for the analysis.
data <- qs::qread(
    file = file.path("out", "final", "smr_pcr_subset_to_model.qs")
)


# Generate availablity habitat data --------------------------------------------


# Melt "data" for all variables.
hab_avail <- data.table::melt.data.table(
    data          = data,
    measure.var   = names(var_names),
    variable.name = "VARIABLE",
    value.name    = "VALUE"
)

# Add the <TYPE>.
hab_avail[, TYPE := "Available"]


# Generate selected habitat data -----------------------------------------------


# Melt "data" when Y > 0 for all variables.
hab_select <- data.table::melt.data.table(
    data          = data[Y > 0],
    measure.var   = names(var_names),
    variable.name = "VARIABLE",
    value.name    = "VALUE"
)

# Duplicate values when multiple fishes were observed.
irows <- unlist(lapply(
    X   = 1:nrow(hab_select),
    FUN = function(w) rep(w, times = hab_select$Y[w])
))
hab_select <- hab_select[irows, ]

# Add the <TYPE>.
hab_select[, TYPE := "Selected"]


# Merge two tables -------------------------------------------------------------


hab <- data.table::rbindlist(list(hab_avail, hab_select))


# Plot global histograms of availability / selection ---------------------------


# Save as a pdf for future use.
pdf(
    file   = file.path("out", "plots", "fig_3_histograms_global.pdf"),
    width  = 9L,
    height = 4L
)

# Plot.
ggplot(
    data    = hab,
    mapping = aes(
        x = VALUE
    )
) +
geom_histogram(
    mapping = aes(
        fill = TYPE,
        y    = ..density..
    ),
    color="grey90",
    lwd=0.5,
    position = "dodge",
    bins=8
) +
facet_wrap(
    facets = ~ VARIABLE,
    nrow   = 2L,
    ncol   = 3L,
    scales = "free"
) +
labs(
    title = "",
    y     = "Probability density function (PDF)",
    x     = "Velocity (m/s)                                                   Depth (cm)                                                    D50 (mm)",
    fill  = ""
) +
theme(
    legend.position = "bottom"
)  +
scale_fill_manual(
    values = c("#9B9B93", "#63B0CD")
)

# Save plot.
dev.off()


# Range of the habitat variables  ----------------------------------------------


# Get the range of the three habitat characteristics.
hab_var_range <- data.table::data.table(
    LIMITTYPE = c("lower", "upper"),
    DEPTH     = get_range(data$DEPTH,    liminf = 0L, factor = 1L),
    VELOCITY  = get_range(data$VELOCITY, liminf = 0L, factor = 1L),
    D50       = get_range(data$D50,      liminf = 0L, factor = 1L)
)

# Check results.
hab_var_range

# Manual adjustment for this specific dataset
hab_var_range[2L, 2L] <- 100L
hab_var_range[2L, 3L] <- 2L
hab_var_range[2L, 4L] <- 300L

# Check final result.
hab_var_range

# Save for traceability.
qs::qsave(
    x    = hab_var_range,
    file = file.path("out", "tmp", "hab_var_range.qs")
)


# Generate functional curves ---------------------------------------------------


# Create a canvas table.
tbl <- data.table::setDT(expand.grid(
    SITENEW  = unique(hab$SITENEW),
    VARIABLE = names(var_names),
    TYPE     = unlist(hab_names, use.names = FALSE)
))

# Loop over all possible values.
fd_curves <- dtlapply(
    X   = seq.int(1L, nrow(tbl)),
    FUN = function(w) {

        # Extract information from the canvas tbl.
        site <- tbl$SITENEW[w]
        var  <- tbl$VARIABLE[w]
        type <- tbl$TYPE[w]

        # Fit curves
        fit <- fit_kde(
            x     = hab[SITENEW  == site &
                        VARIABLE == var  &
                        TYPE     == type, VALUE],
            range = hab_var_range[, var, with = FALSE]
        )

        # Add relevant information.
        fit[, `:=`(SITENEW = site, VARIABLE = var, TYPE = type)]

        # Return fitted function.
        return(fit)

    }
)


# Plot curves ------------------------------------------------------------------


# Save as a pdf for future use.
pdf(
    file   = file.path("out", "plots", "fig_4_habitat_fd_curves.pdf"),
    width  = 8L,
    height = 8L
)

# Loop over all variables.
for (var in names(var_names)) {

    # Loop over two pages.
    for (page.i in c(1L, 2L)) {

        # Generate plot.
        p <- ggplot(
            data    = fd_curves[VARIABLE == var, ],
            mapping = aes(x = X)) +
        geom_line(
            mapping = aes(
                y     = Y,
                color = TYPE),
            lwd     = 1L
        ) +
        facet_wrap_paginate(
            facets = ~ SITENEW,
            nrow   = 5L,
            ncol   = 4L,
            scales = "free_y",
            page   = page.i
        ) +
        labs(
            title = paste0(
                "Habitat availability / selection for ", tolower(var)
            ),
            color = "",
            x     = var_names[var],
            y     = "Probability density function (PDF)"
        ) +
        scale_color_manual(
            values = c("#9B9B93", "#63B0CD")
        ) +
        legend_bottom

        # Print plot.
        print(p)
    }
}

# Saving plot.
dev.off()


# Exports ----------------------------------------------------------------------


qs::qsave(
    x    = fd_curves,
    file = file.path("out", "final", "fd_curves.qs")
)

