# s04_map_studied_sites.R


# Step 04 : Generate a subset of the studied site for mapping purpose.


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


# GPS points for the SMR -------------------------------------------------------


# Load GPS points with coordinates.
gps_points_smr <- rgdal::readOGR(
    dsn              = file.path("data", "gps", "gps_sites_smr.kml"),
    layer            = "Sites Faits (Ordre rivière-25)",
    stringsAsFactors = FALSE
)

# Convert to data.frame.
gps_points_smr <- data.table::setDT(data.frame(gps_points_smr[, c("Name")]))

# Rename columns.
names(gps_points_smr) <- c("SITE", "LON", "LAT", "Z", "C")

# Keep the three first columns.
gps_points_smr[, `:=`(Z = NULL, C = NULL)]

# Update <SITE>.
gps_points_smr[, SITE := as.integer(substr(SITE, 6L, nchar(SITE)))]

# Select the subset of points retained for the studied site.
gps_smr_sub <- gps_points_smr[data_site[RIVER == "SMR", ], , on = "SITE"]

# Order by sitte.
gps_smr_sub <- gps_smr_sub[order(SITE), ]

# Create a variable <NEWSITE>
gps_smr_sub[, NEWSITE := 1:.N]

# Convert to spdf.
gps_smr_sub_spdf <- sp::SpatialPointsDataFrame(
    coords      = as.matrix(gps_smr_sub[, .(LON, LAT)]),
    data        = gps_smr_sub[, .(SITE, NEWSITE, RIVER)],
    proj4string = sp::CRS("+init=EPSG:4326")
)


# GPS points for the PCR -------------------------------------------------------


# Load GPS points with coordinates.
gps_points_pcr <- rgdal::readOGR(
    dsn              = file.path("data", "gps", "gps_sites_pcr.kml"),
    layer            = "Sites Faits (Ordre rivière)",
    stringsAsFactors = FALSE
)

# Convert to data.frame.
gps_points_pcr <- data.table::setDT(data.frame(gps_points_pcr[, c("Name")]))

# Rename columns.
names(gps_points_pcr) <- c("SITE", "LON", "LAT", "Z", "C")

# Keep the three first columns.
gps_points_pcr[, `:=`(Z = NULL, C = NULL)]

# Update <SITE>.
gps_points_pcr[, SITE := as.integer(substr(SITE, 6L, nchar(SITE)))]

# Select the subset of points retained for the studied site.
gps_pcr_sub <- gps_points_pcr[data_site[RIVER == "PCR", ], , on = "SITE"]

# Order by sitte.
gps_pcr_sub <- gps_pcr_sub[order(SITE), ]

# Create a variable <NEWSITE>
gps_pcr_sub[, NEWSITE := 1:.N]

# Convert to spdf.
gps_pcr_sub_spdf <- sp::SpatialPointsDataFrame(
    coords      = as.matrix(gps_pcr_sub[, .(LON, LAT)]),
    data        = gps_pcr_sub[, .(SITE, NEWSITE, RIVER)],
    proj4string = sp::CRS("+init=EPSG:4326")
)


# Plot sites -------------------------------------------------------------------


# Plot sites of the SMR.
ggplot(
    data = sf::st_as_sf(gps_smr_sub_spdf)
) +
geom_sf(
) +
geom_sf_text(
    nudge_y = .01,
    mapping = aes(label = NEWSITE)
)

# Plot sites of the PCR.
ggplot(
    data = sf::st_as_sf(gps_pcr_sub_spdf)
) +
geom_sf(
) +
geom_sf_text(
    nudge_y = .01,
    mapping = aes(label = NEWSITE)
)


# Exports ----------------------------------------------------------------------


# SMR.
rgdal::writeOGR(
    obj             = gps_smr_sub_spdf,
    dsn             = file.path("QGIS", "data"),
    layer           = "studied_sites_smr",
    driver          = "ESRI Shapefile",
    overwrite_layer = TRUE
)

# PCR.
rgdal::writeOGR(
    obj             = gps_pcr_sub_spdf,
    dsn             = file.path("QGIS", "data"),
    layer           = "studied_sites_pcr",
    driver          = "ESRI Shapefile",
    overwrite_layer = TRUE
)


# Map creation -----------------------------------------------------------------


# The other steps are performed in QGIS. See QGIS/ for more info.

