# s4_generate_fd_curves.R


# Generate functional curves for the further analysis.


# Project : habitat_suitability_curves_fsa
# Author  : Jeremie Boudreault
# Email   : JeremieBoudreault11@gmail.com
# Depends : R (v3.6.3)
# License : CC BY-NC-ND 4.0



# Library ----------------------------------------------------------------------


library(data.table)
library(ggplot2)
library(ggforce)


# Imports ----------------------------------------------------------------------


# Subsetted data for the analysis.
data <- qs::qread(
    file = file.path("out", "final", "smr_pcr_subset_to_model.qs")
)


# Generate availablity habitat data --------------------------------------------


# Melt "data" for all variables.
hab_avail <- data.table::melt.data.table(
    data          = data,
    measure.var   = names(var_list),
    variable.name = "VARIABLE",
    value.name    = "VALUE"
)

# Add the <TYPE>.
hab_avail[, TYPE := "Available"]


# Generate selected habitat data -----------------------------------------------


# Melt "data" when Y > 0 for all variables.
hab_select <- data.table::melt.data.table(
    data          = data[Y > 0],
    measure.var   = names(var_list),
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
    width  = 8L,
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
    x     = "Velocity (m/s)                                            Depth (cm)                                           D50 (mm)",
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
hab_var_range[2L, 2L] <- 100
hab_var_range[2L, 3L] <- 2
hab_var_range[2L, 4L] <- 300

# Check final result.
hab_var_range

# Save for traceability.
qs::qsave(
    x    = hab_var_range,
    file = file.path("out", "tmp", "hab_var_range.qs")
)


# Generate functional curves ---------------------------------------------------


# Generate availability curves at each site
SITES <- unique(data$NewSite)
NSITES <- length(SITES)

# Function to fit a curve, scale it to 0 - 1 and adjust kernel estimate
curve01 <- function(data, range, npoints = 2^8, adjust=4) {
    a <- density(x = unlist(data),
                 bw = 'nrd0',
                 adjust = adjust,
                 kernel = "gaussian",
                 n = npoints,
                 from = unlist(range)[1],
                 to = unlist(range)[2],)
    a$y <- a$y/max(a$y)
    data.table(x=a$x, y=a$y)
}

# Generate curve01 of availability for all sites
CalcCurves <- function(w) {

    # Generate an initial data.table to get the s values of the x(s) curve
    x <- curve01(hab_avail[NewSite == 1 & variable == w, value], RANGE_TBL[, get(w)])
    y <- curve01(hab_avail[NewSite == 1 & variable == w, value], RANGE_TBL[, get(w)])

    # Run on the NSITES availability curves x(s)
    x <- data.table(cbind(x, sapply(2:NSITES, function(z) curve01(hab_avail[NewSite == z & variable == w, value], RANGE_TBL[, get(w)])$y)))

    # Run on the NSITES selection curves y(s)
    y <- data.table(cbind(y, sapply(2:NSITES, function(z) curve01(hab_select[NewSite == z & variable == w, value], RANGE_TBL[, get(w)])$y)))

    # Convert to data.table
    x$TYPE <- "Availability"
    y$TYPE <- "Selection"

    # Return
    colnames(x) <- c("s", paste0("Site ", 1:NSITES), "TYPE")
    colnames(y) <- c("s", paste0("Site ", 1:NSITES), "TYPE")
    rbind(x, y)
}


# Generate curves
CURVES_LIST <- lapply(var_list, CalcCurves)
names(CURVES_LIST) <- var_list

# Melting the element of the list for graphing purpose
CURVES_MELT <- lapply(CURVES_LIST, melt, id.var_list=c("s", "TYPE"))

# Plot the resulting curves
# Plotting availability versus selection at each site
pdf("out/data visualisation/Availability_selection_curves_per_site.pdf")
for (var in var_list) {
for (page.i in 1:2) {
        print(
            ggplot(CURVES_MELT[[var]], aes(x = s)) +
                geom_line(aes(y=value, color=TYPE), lwd=1, alpha=1) +
                facet_wrap_paginate(~variable, nrow=5, ncol=4, scales="free_y", page=page.i) +
                labs(color="") +
                theme(legend.position="bottom")  +
                ylab("Probability density function (PDF)") +
                xlab("Value") +
                ggtitle(paste0("Habitat availability / selection for ", var)) +
                scale_color_manual(values = c("#9B9B93", "#63B0CD"))
        )
    }
}
dev.off()


