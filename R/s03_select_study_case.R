# s03_select_study_case.R


# Step 03 : Select optimal study case to be modelled (0+, 1+, 2+).


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


# Cleaned data of SMR and PCR with salmon numbers.
data <- qs::qread(
    file.path("out", "tmp", "s02_smr_pcr_salmon_number.qs")
)


# Calculate summary per site ---------------------------------------------------


data_per_site <- data[, list(
    N_FRY_T        = sum(N_FRY_T),
    N_OBS_FRY_T    = sum(N_FRY_T > 0L),
    N_FRY_M        = sum(N_FRY_M),
    N_OBS_FRY_M    = sum(N_FRY_M > 0L),
    N_PARR1_M      = sum(N_PARR1_M),
    N_OBS_PARR1_M  = sum(N_PARR1_M > 0L),
    N_PARR_2_M     = sum(N_PARR2_M),
    N_OBS_PARR2_M  = sum(N_PARR2_M > 0L),
    N_PARR_M       = sum(N_PARR_M),
    N_OBS_PARR_M   = sum(N_PARR_M > 0L),
    N_PARR_T       = sum(N_PARR_T),
    N_OBS_PARR_T   = sum(N_PARR_T > 0L)
), by = c("RIVER", "SITE")]


# Calculate number of observations ---------------------------------------------


# Extract columns names starting with N_OBS.
cnames <- names(data_per_site)
cnames <- cnames[
    substr(cnames, 1L, 5L) == "N_OBS" &
    substr(cnames, nchar(cnames), nchar(cnames)) == "M"
]

# Summary of the number of observation
res <- dtlapply(
    X    = seq.int(3L, 7L),
    FUN  = function(n_obs_threshold) {
        data_per_site[, list(
            VAR       = cnames,
            N_SITES   = lapply(.SD, function(w) sum(w >= n_obs_threshold)),
            THRESHOLD = n_obs_threshold
            ),
        .SDcols = cnames]
    }
)


# Plot results -----------------------------------------------------------------


# Plot.
ggplot(
    data    = res,
    mapping = aes(
        x     = THRESHOLD,
        y     = VAR,
        fill  = as.numeric(N_SITES),
        label = N_SITES
    )
) +
geom_raster(
    show.legend = FALSE
) +
geom_text(
    col     = "white",
) +
scale_fill_gradientn(
    colors = RColorBrewer::brewer.pal(11L, "RdYlGn")[-c(1, 5, 6, 7, 11)]
) +
labs(
    title = "Number of sites for our study",
    y     = "Studied variable",
    x     = "Minimum number of observations / site required"
) +
custom_theme() +
theme(
    panel.grid   = element_blank(),
    panel.border =element_blank()
)

# Save the plot.
ggsave(
    file   = file.path("out", "plots", "fig_2_study_case_selection.pdf"),
    width  = 6L,
    height = 5L
)


# Optimal selected study case --------------------------------------------------


# Minimum number of observations.
threshold <- 4L

# Selected variable.
var_selected <- "N_PARR_M"


# Subset the study case --------------------------------------------------------


# Extract the selected sites that fit the criteria.
sites_selected <- data_per_site[get(var_selected) >= threshold, .(RIVER, SITE)]

# Add a new definition of the site.
sites_selected[, SITENEW := 1:.N]

# Add an indicator.
sites_selected[, SELECTED := TRUE]

# Extract selected data.
data_selected <- data.table::merge.data.table(
    x     = data,
    y     = sites_selected,
    by    = c("RIVER", "SITE"),
    all.x =  TRUE
)[SELECTED == TRUE, ]

# Rearrange the table.
data_clean <- data_selected[, .(
    RIVER,
    SITE,
    SITENEW,
    PARCEL,
    Y = get(var_selected),
    VELOCITY,
    DEPTH,
    D50)
]


# Export -----------------------------------------------------------------------


qs::qsave(
    x    = data_clean,
    file = file.path("out", "tmp", "s03_smr_pcr_subset_to_model.qs")
)

