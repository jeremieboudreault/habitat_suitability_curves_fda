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
library(ggpubr)


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
    data          = data[Y > 0, ],
    measure.var   = names(var_names),
    variable.name = "VARIABLE",
    value.name    = "VALUE"
)

# Add the <TYPE>.
hab_select[, TYPE := "SELECTED"]


# Merge two tables -------------------------------------------------------------


hab <- data.table::rbindlist(list(hab_avail, hab_select))


# Create a table with characteristics ------------------------------------------


# Calculate number of sites.
hab[, .N, by = c("RIVER", "SITE_INTERNAL")][, .N, by = "RIVER"]

# Calculate mean physical measurements for SMR and PCR.
phy_mean_river <- data.table::dcast.data.table(
    data      = hab[, mean(VALUE), by = c("RIVER", "VARIABLE", "TYPE")],
    formula   = RIVER ~ TYPE + VARIABLE
)

# Calculate mean physical measurements for both rivers.
phy_mean_reg <- data.table::dcast.data.table(
    data      = hab[, mean(VALUE), by = c("VARIABLE", "TYPE")],
    formula   = . ~ TYPE + VARIABLE
)[, RIVER := "REG"]

# Combine tables of mean.
phy_mean <- data.table::rbindlist(
    l         = list(phy_mean_river[c(2L, 1L)], phy_mean_reg),
    use.names = TRUE,
    fill      = TRUE
)

# Calculate std of physical measurements for SMR and PCR.
phy_sd_river <- data.table::dcast.data.table(
    data      = hab[, sd(VALUE), by = c("RIVER", "VARIABLE", "TYPE")],
    formula   = RIVER ~ TYPE + VARIABLE
)

# Calculate mean physical measurements for both rivers.
phy_sd_reg <- data.table::dcast.data.table(
    data      = hab[, sd(VALUE), by = c("VARIABLE", "TYPE")],
    formula   = . ~ TYPE + VARIABLE
)[, RIVER := "REG"]

# Combine tables of std.
phy_sd <- data.table::rbindlist(
    l         = list(phy_sd_river[c(2L, 1L)], phy_sd_reg),
    use.names = TRUE,
    fill      = TRUE
)

# Calculate biological measurement physical measurements for SMR and PCR.
bio_sub <- hab[TYPE == "AVAILABLE", .(Y = sum(Y)), by = c("SITE_INTERNAL", "RIVER")]

# Calculate mean across all sites.
bio_mean_river <- bio_sub[, .(MEAN = mean(Y), SD = sd(Y)), by = "RIVER"]
bio_mean_reg <- bio_sub[, .(MEAN = mean(Y), SD = sd(Y))][, RIVER := "REG"]

# Combine both table.
bio_mean <- data.table::rbindlist(
    l         = list(bio_mean_river[c(2L, 1L), ], bio_mean_reg),
    use.names = TRUE,
    fill      = TRUE
)

# Calculate sum across all sites.
bio_sum_river <- bio_sub[, .(SUM = sum(Y)), by = "RIVER"]
bio_sum_reg <- bio_sub[, .(SUM = sum(Y))][, RIVER := "REG"]

# Combine both tables.
bio_sum <- data.table::rbindlist(
    l         = list(bio_sum_river[c(2L, 1L), ], bio_sum_reg),
    use.names = TRUE,
    fill      = TRUE
)


# Limit the values of the upper tail prior to fit KDE --------------------------


# Note : We want to avoid wigly upper tails of the KDE, so we limit the maximum
# value of the distribution to the 99% quantile of the habitat distribution,
# with little or no effect on the overall results.

# Depth.
hab[VARIABLE == "DEPTH", quantile(VALUE, 0.99)]  # 84
hab[VARIABLE == "DEPTH" & VALUE > 84, VALUE := 84]

# D50.
hab[VARIABLE == "D50", quantile(VALUE, 0.99)] # 190
hab[VARIABLE == "D50" & VALUE > 190, VALUE := 190]

# Velocity.
hab[VARIABLE == "VELOCITY", quantile(VALUE, 0.99)]  # 1.42
hab[VARIABLE == "VELOCITY" & VALUE > 1.42, VALUE := 1.42]


# Range of the habitat variables  ----------------------------------------------


# Get the range of the three habitat characteristics.
hab_var_range <- data.table::data.table(
    LIMITTYPE = c("lower", "upper"),
    DEPTH     = get_range(hab[VARIABLE == "DEPTH", VALUE],    liminf = 0L, factor = 1L),
    VELOCITY  = get_range(hab[VARIABLE == "VELOCITY", VALUE], liminf = 0L, factor = 1L),
    D50       = get_range(hab[VARIABLE == "D50", VALUE],      liminf = 0L, factor = 1L)
)

# Check results.
hab_var_range

# Manual adjustment for this specific dataset
hab_var_range[2L, 2L] <- 85
hab_var_range[2L, 3L] <- 1.50
hab_var_range[2L, 4L] <- 200L

# Check final result.
hab_var_range


# Compute preference values ----------------------------------------------------


# Note : Because small differences in site-specific KDE for availability and
#        selection can generate huge differences in the resulting preference
#        curve, we rather developed smooth continuous of preference curve using
#        another method than the direct division. The results are, however,
#        similar to those obtained by dividing the selected KDE with the
#        available KDE, but the results are much smoother and realistic.

# Extract all sites to compute preference.
site_frame <- unique(hab[, .(RIVER, SITE_ORIGINAL, SITE_NEW, SITE_INTERNAL, VARIABLE)])

# Loop over all site and return a data.table.
pref_table <- dtlapply(
    X   = 1:nrow(site_frame),
    FUN = function(w) {

        # Extract site w.
        site_frame_sub <- site_frame[w, ]

        # Extract internal site and variable.
        site_sub <- site_frame_sub$SITE_INTERNAL
        var_sub <- site_frame_sub$VARIABLE

        # Extract habitat information.
        hab_sub <- hab[SITE_INTERNAL ==  site_sub & VARIABLE == var_sub, ]

        # First we convert habitat values to categories with very narrow range.
        class <- class_list[[var_sub]]
        hab_sub[, VALUE_CLASS := round(VALUE/class) * class]

        # Create class of available and selected.
        avail_tbl <- as.data.table(table(hab_sub[TYPE == "AVAILABLE", VALUE_CLASS]))
        select_tbl <- as.data.table(table(hab_sub[TYPE == "SELECTED", VALUE_CLASS]))

        # Update columsn names.
        names(avail_tbl) <- c("VALUE", "N_AVAIL")
        names(select_tbl) <- c("VALUE", "N_SELE")

        # Merge both table.
        pref_tbl <- select_tbl[avail_tbl, on = "VALUE"]
        pref_tbl[is.na(N_SELE), N_SELE := 0]

        # Compute frequency.
        pref_tbl[, F_AVAIL := N_AVAIL / sum(N_AVAIL)]
        pref_tbl[, F_SELE  := N_SELE  / sum(N_SELE)]
        pref_tbl[, PREF    := F_SELE  / F_AVAIL]

        # We want to transfer the preference values to fit a KDE.
        mult <- find_lcm(pref_tbl$PREF)

        # Convert to values.
        pref_tbl[, N_PSEUDO_PREF := round(PREF * mult, 0)]

        # Convert to values.
        pref_val <- as.numeric(unlist(sapply(
            X   = 1:nrow(pref_tbl),
            FUN = function(w) rep(pref_tbl$VALUE[w], length.out = pref_tbl$N_PSEUDO_PREF[w])
        )))

        # Return a data.table.
        data.table(
            site_frame_sub,
            VALUE  = pref_val,
            TYPE   = "PREFERENCE",
            PARCEL = 0,
            Y      = 1L
        )

    }
)

# Combine pref and habitat table.
hab <- data.table::rbindlist(
    l = list(hab, pref_table), use.names = TRUE
)


# Generate functional curves ---------------------------------------------------


# Create a canvas table.
tbl <- data.table::setDT(expand.grid(
    SITE_INTERNAL = unique(hab$SITE_INTERNAL),
    VARIABLE      = names(var_names),
    TYPE          = names(hab_names)
))

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
            adjust  = adjust_list[[var]] + (adjust_pref * (type == "PREFERENCE")),
            npoints = 2^7,
            scale   = FALSE
        )

        # Adjust between 0 and 1.
        if (type == "PREFERENCE") {
            fit[, Y := Y / max(Y)]
        }

        # Add relevant information.
        fit[, `:=`(SITE_INTERNAL = site, VARIABLE = var, TYPE = type)]

        # Return fitted function.
        return(fit)

    }
)

# Drop preference "pseudo" habitat.
hab <- hab[TYPE != "PREFERENCE", ]


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
    p_all <- lapply(names(var_names), function(var) {

        p <- lapply(sites, function(site) {

        # Extract sub data to plot.
        fd_curves_sub <- fd_curves[
            SITE_INTERNAL == site &
            VARIABLE      == var
        ]
        hab_sub <- hab[
            SITE_INTERNAL == site &
            VARIABLE      == var,
        ]

        # Extract range.
        rng <- unlist(hab_var_range[, var, with = FALSE])

        # Create the plot.
        p <- ggplot(
            data    = fd_curves_sub,
            mapping = aes(x = X)
        ) +
        geom_histogram(
            data = hab_sub,
            mapping = aes(
                x    = VALUE,
                y    = ..density..,
                fill = TYPE
            ),
            color       = "grey90",
            lwd         = 0.2,
            position    = "dodge",
            bins        = 10L,
            show.legend = FALSE
        ) +
        scale_x_continuous(
            limits = rng
        )

        # Extract maximum values of
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
            facets   = ~ LABEL,
            ncol     = 4L,
            nrow     = 1L,
            scales   = "free"
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

        # Add title and labels.
        if (site == sites[1]) {
            p <- p + ggtitle(
                paste0(
                    letters[which(var == names(var_names))], ") ",
                   var_names[[var]])
            ) + xlab("")
        } else if (site == sites[2L]) {
            p <- p + ggtitle("") + xlab(var_names_u[[var]])
        } else {
            p <- p + ggtitle("") + xlab("")
        }

        # Return p.
        return(p)

    })

    return(p)

    })

    # Combine all plots.
    p_full <- ggpubr::ggarrange(
        plotlist      = unlist(p_all, recursive = FALSE),
        ncol          = length(sites),
        nrow          = 3L,
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


# Plot a subset of the curves --------------------------------------------------


# Select sites.
sites <- c(5L, 14L, 22L)

# Plot curves.
plot_kde_hist(sites)

# Save plot for further use.
ggsave(
    file   = file.path("out", "plots", "fig_4_kde_curves_overview.pdf"),
    width  = 8L,
    height = 7L
)


# Plot all curves --------------------------------------------------------------


# Create subsets of 4 sites for plotting.
sites <- unique(data$SITE_INTERNAL)
sites_subset <- lapply(
    X   = seq_len(ceiling(length(sites)/3L)),
    FUN = function(w) { seq.int(w * 3 -2L, w * 3L) }
)

# Create pdf to save all the graphs.
pdf(
    file   = file.path("out", "plots", "fig_5_kde_curves_all.pdf"),
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


# Habitat variables.
qs::qsave(
    x    = hab,
    file = file.path("out", "tmp", "s05_hab_var.qs")
)

# Habitat variables range.
qs::qsave(
    x    = hab_var_range,
    file = file.path("out", "tmp", "s05_hab_var_range.qs")
)

# Functional curves.
qs::qsave(
    x    = fd_curves,
    file = file.path("out", "tmp", "s05_fd_curves_dt.qs")
)

