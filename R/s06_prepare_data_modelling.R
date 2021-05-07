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
    file.path("out", "tmp", "s4_fd_curves_dt.qs")
)


# Extract functional observations ----------------------------------------------


# Y(s).
fd_curves_y <- data.table::dcast.data.table(
    data      = fd_curves[TYPE == "Selected", ],
    formula   = X + VARIABLE ~ SITENEW,
    value.var = "Y"
)

# X(r).
fd_curves_x <- data.table::dcast.data.table(
    data      = fd_curves[TYPE == "Available", ],
    formula   = X + VARIABLE ~ SITENEW,
    value.var = "Y"
)


# Generate dataset for all three habitat variable ------------------------------


# Create a fd_curves_list object.
fd_curves_list <- lapply(
    X   = names(var_names),
    FUN = function(var) {
        return(list(
            Y = t(as.matrix(fd_curves_y[VARIABLE == var, -c("X", "VARIABLE")])),
            X = t(as.matrix(fd_curves_x[VARIABLE == var, -c("X", "VARIABLE")])),
            s = unlist(fd_curves_y[VARIABLE == var, "X"], use.names = FALSE),
            r = unlist(fd_curves_x[VARIABLE == var, "X"], use.names = FALSE)
        ))
    }
)

# Add names.
names(fd_curves_list) <- names(var_names)


# Exports ----------------------------------------------------------------------


qs::qsave(
    x = fd_curves_list,
    file = file.path("out", "tmp", "s5_fd_curves_list.qs")
)

