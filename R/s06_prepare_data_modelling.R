# s06_prepare_data_modelling.R


# Step 06 : Prepare data prior to modelling.


# Project : habitat_suitability_curves_fda
# Author  : Jeremie Boudreault
# Email   : Jeremie.Boudreault [at] ete.inrs [dot] ca
# Depends : R (v3.6.3)
# License : CC BY-NC-ND 4.0



# Library ----------------------------------------------------------------------


library(data.table)


# Functions --------------------------------------------------------------------


source(file.path("R", "functions", "internals.R"))
source(file.path("R", "functions", "globals.R"))


# Imports ----------------------------------------------------------------------


# Functional observations.
fd_curves <- qs::qread(
    file.path("out", "tmp", "s05_fd_curves_dt.qs")
)

# Load data per site.
data <- qs::qread(
    file.path("out", "tmp", "s03_smr_pcr_subset_to_model.qs")
)


# Extract functional observations ----------------------------------------------


# Y(s).
fd_curves_y <- data.table::dcast.data.table(
    data      = fd_curves[TYPE == "SELECTED", ],
    formula   = X + VARIABLE ~ SITE_INTERNAL,
    value.var = "Y"
)

# X(r).
fd_curves_x <- data.table::dcast.data.table(
    data      = fd_curves[TYPE == "AVAILABLE", ],
    formula   = X + VARIABLE ~ SITE_INTERNAL,
    value.var = "Y"
)


# Extract information about the rivers -----------------------------------------


site_info <- unique(data[, .(
    SITE_INTERNAL,
    SITE_LABEL = paste0(RIVER, " - ", SITE_NEW),
    RIVER
)])[order(SITE_INTERNAL), ]


# Generate dataset for all three habitat variable ------------------------------


# Create a fd_curves_list object.
fd_curves_list <- lapply(
    X   = names(var_names),
    FUN = function(var) {
        return(list(
            Y     = t(as.matrix(fd_curves_y[VARIABLE == var, -c("X", "VARIABLE")])),
            X     = t(as.matrix(fd_curves_x[VARIABLE == var, -c("X", "VARIABLE")])),
            s     = unlist(fd_curves_y[VARIABLE == var, "X"], use.names = FALSE),
            r     = unlist(fd_curves_x[VARIABLE == var, "X"], use.names = FALSE),
            SITE_INTERNAL = site_info$SITE_INTERNAL,
            SITE_LABEL    = site_info$SITE_LABEL,
            RIVER         = site_info$RIVER
        ))
    }
)

# Add names.
names(fd_curves_list) <- names(var_names)


# Exports ----------------------------------------------------------------------


qs::qsave(
    x = fd_curves_list,
    file = file.path("out", "tmp", "s06_fd_curves_list.qs")
)

