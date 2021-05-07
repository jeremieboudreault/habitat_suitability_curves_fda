# s02_compute_salmon_numbers.R


# Step 02 :  Compute salmon numbers based on the measured fork-lengths.


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


data <- qs::qread(
    file.path("out", "tmp", "s01_smr_pcr_cleaned.qs")
)


# Extract all measured fork-lengths --------------------------------------------


# Define cnames of the columns for <LENGTH_X>.
cnames <- paste0("LENGTH_", seq.int(1L, 11L))

# Extract lengths for SMR.
lengths_smr <- as.vector(na.omit(unlist(
    x         = data[RIVER == "SMR", cnames, with = FALSE],
    use.names = FALSE
)))

# Extract lengths for PCR
lengths_pcr <- as.vector(na.omit(unlist(
    x         = data[RIVER == "PCR", cnames, with = FALSE],
    use.names = FALSE
)))

# Combine all lengths in a data.table.
lengths <- data.table::data.table(
    RIVER  = c(
        rep("SMR", length(lengths_smr)),
        rep("PCR", length(lengths_pcr))
    ),
    LENGTH = c(
        lengths_smr,
        lengths_pcr
    )
)

# Check resulting table.
lengths


# Check distributions ----------------------------------------------------------


# Plot distribution of lengths.
ggplot(
    data    = lengths,
    mapping = aes(x = LENGTH)
) +
geom_histogram(
    boundary = 0L,
    binwidth = 2.5,
    fill     = "#c1c1c1",
    color    = "#636363",
    lwd      = 0.25
) +
labs(
    title = "Juvenile salmons lengths distribution",
    x     = "Juvenile salmons lengths (mm)",
    y     = "Count"
) +
scale_x_continuous(
    breaks = seq.int(2L, 14L) * 10L
) +
facet_wrap(
    ~ RIVER,
    nrow   = 2L,
    scales = "free_y"
) +
custom_theme()


# Define threshold limits ------------------------------------------------------


# Extract minimumand maximum.
lengths[, list(MIN = min(LENGTH), MAX = max(LENGTH)), by = RIVER]

# Fork-lengths limits for the SMR and PCR.
lengths_lim <- data.table::data.table(
    RIVER    = rep(c("SMR", "PCR"), each = 4L),
    LIM      = rep(c("MIN_0", "MIN_1", "MIN_2", "MAX_2"), length.out = 8L),
    VAL      = c(30, 55, 87.5, 125, 42.5, 62.5, 92.5, 132.5)
)

# Check result.
lengths_lim


# Final plot -------------------------------------------------------------------


# Plot.
ggplot(
    data    = lengths,
    mapping = aes(x = LENGTH)
) +
geom_histogram(
    boundary = 0,
    binwidth = 2.5,
    fill     = "#c1c1c1",
    color    = "#636363",
    lwd      = 0.25
) +
labs(
    title = NULL,
    x     = "Fork length (mm)",
    y     = "Number of juvenile salmon"
) +
scale_x_continuous(
    breaks = seq.int(3L, 14L) * 10L
) +
geom_vline(
    data    = lengths_lim,
    mapping = aes(xintercept = VAL),
    color   = "red",
    lwd     = 0.3,
    lty     = 2L
) +
geom_text(
    data    = data.frame(
        X     = c(42.5, 53, 71.5, 78, 106.5, 112.5),
        Y     = c(82L, 32L, 38L, 38L, 18L, 18L),
        TEXT  = rep(c("Fry (0+)", "Parr (1+)", "Parr (2+)"), each = 2L),
        RIVER = rep(c("SMR", "PCR"), length.out = 6L)
    ),
    mapping = aes(
        x     = X,
        y     = Y,
        label = TEXT
    ),
    size    = 3L,
    nudge_y = -3L
) +
facet_wrap(
    facets   = ~ RIVER,
    nrow     = 2L,
    scales   = "free_y",
    as.table = FALSE,
    labeller = labeller(RIVER = unlist(riv_names))
) +
custom_theme()

# Save to pdf.
ggsave(
    file   = file.path("out", "plots", "fig_1_salmon_lengths.pdf"),
    width  = 7L,
    height = 6L
)


# Convert lengths to number ----------------------------------------------------


# First dcast the above lengths_lim.
lengths_lim_dcast <- data.table::dcast.data.table(
    data      = lengths_lim,
    formula    = RIVER ~ LIM,
    value.var = "VAL",
)

# Check result.
lengths_lim_dcast

# Merge the lengths_limits and the data.
data <- data[lengths_lim_dcast, on = "RIVER"]

# Calculate number of Fry, Parr 1+ and Parr 2+ from lengths at SMR
data$N_FRY_M   <- sapply(1:nrow(data), function(w) sum(data[w, ..cnames] <  data$MIN_1[w], na.rm = TRUE))
data$N_PARR1_M <- sapply(1:nrow(data), function(w) sum(data[w, ..cnames] >= data$MIN_1[w] & data[w, ..cnames] < data$MIN_2[w], na.rm = TRUE))
data$N_PARR2_M <- sapply(1:nrow(data), function(w) sum(data[w, ..cnames] >= data$MIN_2[w] & data[w, ..cnames] < data$MAX_2[w], na.rm = TRUE))
data$N_PARR3_M <- sapply(1:nrow(data), function(w) sum(data[w, ..cnames] >= data$MAX_2[w], na.rm=T))

# Validation.
data[, list(
    N_FRY_M   = sum(N_FRY_M),
    N_PARR1_M = sum(N_PARR1_M),
    N_PARR2_M = sum(N_PARR2_M),
    N_PARR3_M = sum(N_PARR3_M)
), by = list(RIVER, SITE)]


# Final number of fish calculation ---------------------------------------------


data[ , N_FRY_T   := as.integer(N_FRY + N_FRY_M) ]
data[ , N_PARR_M  := as.integer(N_PARR1_M + N_PARR2_M + N_PARR3_M) ]
data[ , N_PARR1_T := as.integer(N_PARR1_M) ]
data[ , N_PARR2_T := as.integer(N_PARR2_M) ]
data[ , N_PARR_T  := as.integer(N_PARR + N_PARR_M) ]
data[ , N_SASA_M  := as.integer(N_FRY_M + N_PARR_M) ]
data[ , N_SASA_T  := as.integer(N_FRY_T + N_PARR_T + N_SASA) ]


# Final arrangement of the table -----------------------------------------------


# Check final names of the dataset.
names(data)

# Clean the rows.
data <- data[, list(
    RIVER,
    SITE,
    PARCEL,
    N_FRY_M,
    N_FRY_T,
    N_PARR1_M,
    N_PARR1_T,
    N_PARR2_M,
    N_PARR2_T,
    N_PARR_M,
    N_PARR_T,
    N_SASA_M,
    N_SASA_T,
    VELOCITY,
    DEPTH,
    TEMP,
    D50,
    D84)
]

# Check for NAs.
which(is.na(data), arr.ind = TRUE)

# Check the class of the variables.
sapply(data, class)


# Export -----------------------------------------------------------------------


qs::qsave(
    x    = data,
    file = file.path("out", "tmp", "s02_smr_pcr_salmon_number.qs")
)

