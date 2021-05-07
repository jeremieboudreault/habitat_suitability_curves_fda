# s05_generate_fd_curves.R


# Step 05 : Generate functional curves for the further analysis.


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


source(file.path("R", "functions", "globals.R"))
source(file.path("R", "functions", "internals.R"))
source(file.path("R", "functions", "plot_helpers.R"))


# Imports ----------------------------------------------------------------------


# Subsetted data for the analysis.
data <- qs::qread(
    file = file.path("out", "tmp", "s03_smr_pcr_subset_to_model.qs")
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
hab_avail[, TYPE := "AVAILABLE"]


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
hab_select[, TYPE := "SELECTED"]


# Merge two tables -------------------------------------------------------------


hab <- data.table::rbindlist(list(hab_avail, hab_select))


# Plot global histograms of availability / selection ---------------------------


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
    color    = "grey90",
    lwd      = 0.5,
    position = "dodge",
    bins     = 8L
) +
facet_wrap(
    facets = ~ VARIABLE,
    nrow   = 1L,
    ncol   = 3L,
    scales = "free",
    labeller = labeller(VARIABLE = unlist(var_names))
) +
labs(
    title = NULL,
    y     = "Probability density function (PDF)",
    x     = "Depth (cm)                                                   D50 (mm)                                                    Velocity (m/s)",
    fill  = ""
) +
theme(
    legend.position = "bottom"
)  +
scale_fill_manual(
    values = c("#9B9B93", "#63B0CD"),
    breaks = names(hab_names),
    labels = ul(hab_names)
) +
custom_theme()

# Save plot.
ggsave(
    file   = file.path("out", "plots", "fig_4_histograms_global.pdf"),
    width  = 9L,
    height = 4L
)


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
hab_var_range[2L, 3L] <- 1.75
hab_var_range[2L, 4L] <- 300L

# Check final result.
hab_var_range

# Save for traceability.
qs::qsave(
    x    = hab_var_range,
    file = file.path("out", "tmp", "s05_hab_var_range.qs")
)


# Generate functional curves ---------------------------------------------------


# Create a canvas table.
tbl <- data.table::setDT(expand.grid(
    SITE_INTERNAL = unique(hab$SITE_INTERNAL),
    VARIABLE      = names(var_names),
    TYPE          = names(hab_names)
))

# Adjust for each variables.
adjust_list <- list(
    DEPTH    = 1.5,
    D50      = 1.7,
    VELOCITY = 1.5
)

# Loop over all possible values.
fd_curves <- dtlapply(
    X   = seq.int(1L, nrow(tbl)),
    FUN = function(w) {

        # Extract information from the canvas tbl.
        site <- tbl$SITE_INTERNAL[w]
        var  <- tbl$VARIABLE[w]
        type <- tbl$TYPE[w]

        # Fit curves
        fit <- fit_kde(
            x       = hab[SITE_INTERNAL == site &
                        VARIABLE       == var  &
                        TYPE           == type, VALUE],
            range   = hab_var_range[, var, with = FALSE],
            adjust  = adjust_list[[var]],
            npoints = 2^7,
            scale   = FALSE
        )

        # Add relevant information.
        fit[, `:=`(SITE_INTERNAL = site, VARIABLE = var, TYPE = type)]

        # Return fitted function.
        return(fit)

    }
)


# Labels for plotting ----------------------------------------------------------


# Extract labels
labels <- unique(data[, .(LABEL = paste0(RIVER, " - ", SITE_NEW), SITE_INTERNAL)])

# Convert labels to a list.
x_labels <- as.list(labels$LABEL)

# Merge labels with "fd_curves" and "hab".
fd_curves[, LABEL := factor(ul(x_labels[SITE_INTERNAL]), ul(x_labels))]
hab[,       LABEL := factor(ul(x_labels[SITE_INTERNAL]), ul(x_labels))]


# Plotting function for curves and histograms ----------------------------------


plot_kde_hist <- function(sites) {

    # Generate the tree plots for all habitat habitat variables.
    p <- lapply(names(var_names), function(var) {
        ggplot(
            data    = fd_curves[
                SITE_INTERNAL %in% sites &
                    VARIABLE == var,
                ],
            mapping = aes(x = X)
        ) +
        geom_histogram(
            data = hab[SITE_INTERNAL %in% sites & VARIABLE == var, ],
            mapping = aes(
                x    = VALUE,
                y    = ..density..,
                fill = TYPE
            ),
            color    = "grey90",
            lwd      = 0.1,
            position = "dodge",
            bins     = 10L
        ) +
        geom_line(
            mapping = aes(
                y     = Y,
                color = TYPE
            ),
            lwd         = 1L,
            show.legend = FALSE
        ) +
        facet_wrap(
            facets   = ~ LABEL,
            ncol     = 4L,
            nrow     = 1L,
            scales   = "free"
        ) +
        labs(
            title = paste0(letters[which(var == names(var_names))], ") ",
                           var_names[[var]]),
            fill  = "",
            x     = var_names_u[[var]],
            y     = NULL
        ) +
        scale_color_manual(
            values = c("#282822", "#257B97"),
            breaks = names(hab_names),
            label  = ul(hab_names)
        ) +
        scale_fill_manual(
            values = c("#9B9B93", "#63B0CD"),
            breaks = names(hab_names),
            label  = ul(hab_names)
        ) +
        custom_theme() +
        theme(plot.title = element_text(hjust = 0))
    })

    # Align the third graph with the other.
    p[[3L]] <- p[[3L]] + ylab("")

    # Generate the plot.
    p_all <- ggpubr::ggarrange(
        plotlist      = p,
        nrow          = 3L,
        legend        = "bottom",
        common.legend = TRUE
    )

    # Add y_axis.
    p_all <- ggpubr::annotate_figure(
        p    = p_all,
        left = "Probability density function (PDF)"
    )

    # Print plot.
    print(p_all)

    # Return null.
    return(invisible(NULL))

}


# Plot a subset of the curves --------------------------------------------------


# Select sites.
sites <- c(5L, 14L, 22L, 33L)

# Plot curves.
plot_kde_hist(sites)

# Save plot for further use.
ggsave(
    file   = file.path("out", "plots", "fig_5_kde_curves_overview.pdf"),
    width  = 8L,
    height = 7L
)

# Plot all curves --------------------------------------------------------------


# Create subsets of 4 sites for plotting.
sites <- unique(data$SITE_INTERNAL)
sites_subset <- lapply(
    X   = seq_len(ceiling(length(sites)/4L)),
    FUN = function(w) { seq.int(w * 4 - 3L, w * 4L) }
)

# Create pdf to save all the graphs.
pdf(
    file   = file.path("out", "plots", "fig_6_kde_curves_all.pdf"),
    width  = 8L,
    height = 7L
)

# Plot for all sites.
for (sites in sites_subset) {
    plot_kde_hist(sites)
}

# Close plot.
dev.off()


# Exports ----------------------------------------------------------------------


qs::qsave(
    x    = fd_curves,
    file = file.path("out", "tmp", "s05_fd_curves_dt.qs")
)

