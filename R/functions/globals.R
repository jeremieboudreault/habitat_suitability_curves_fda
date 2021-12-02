# globals.R


# Globals variables used within that project.


# Project : habitat_suitability_curves_fda
# Author  : Jeremie Boudreault
# Email   : Jeremie.Boudreault [at] ete.inrs [dot] ca
# Depends : R (v3.6.3)
# License : CC BY-NC-ND 4.0



# Names ------------------------------------------------------------------------


# River names.
riv_names <- list(
    SMR  = "Sainte-Marguerite River",
    PCR  = "Petite-Cascapedia River"
)

# Variables names.
var_names <- list(
    DEPTH    = "Depth",
    D50      = "D50",
    VELOCITY = "Velocity"
)

# Variables names with units.
var_names_u <- list(
    DEPTH    = "Depth (cm)",
    D50      = "D50 (mm)",
    VELOCITY = "Velocity (m/s)"
)

# Habitat type names.
hab_names <- list(
    AVAILABLE  = "Available",
    SELECTED   = "Selected",
    PREFERENCE = "Preference"
)

# Colors for available / selected.
hab_colors <- c(
    blues9[c(4L, 8L)], "#000000"
)

# Metric names.
metric_names <- list(
    "fRMSE" = "Functional Root Mean Square Error (fRMSE)",
    "fMAE"  = "Functional Mean Abosule Error (fMAE)",
    "fR2"   = "Functional R2 (fR2)",
    "R2"    = "R square (R2)"
)


# Parameters -------------------------------------------------------------------


# Values of the adjust parameters for the KDE.
adjust_list <- list(
    DEPTH    = 1.5,
    D50      = 1.7,
    VELOCITY = 1.7
)
adjust_pref <- 0.3

# Class interval for preference curves.
class_list <- list(
    DEPTH    = 5,
    D50      = 10,
    VELOCITY = 0.05
)

# Number of values for coefficients.
n_val_coef <- 100L

# Number of bootstrap.
n_bs <- 1000L

