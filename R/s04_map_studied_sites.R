# s04_map_studied_sites.R


# Step 04 : Generate a map of the studied sites.


# Project : habitat_suitability_curves_fda
# Author  : Jeremie Boudreault
# Email   : Jeremie.Boudreault [at] ete.inrs [dot] ca
# Depends : R (v3.6.3)
# License : CC BY-NC-ND 4.0



# Library ----------------------------------------------------------------------


library(readxl)
library(data.table)
library(ggplot2)
library(rgdal)
library(sp)


# Functions --------------------------------------------------------------------


source(file.path("R", "functions", "globals.R"))
source(file.path("R", "functions", "internals.R"))
source(file.path("R", "functions", "plot_helpers.R"))


# Imports ----------------------------------------------------------------------


# Subsetted data for the analysis.
data <- qs::qread(
    file = file.path("out", "tmp", "s03_smr_pcr_subset_to_model.qs")
)

# Extract unique sites.
data_site <- unique(data[, .(RIVER, SITE)])

