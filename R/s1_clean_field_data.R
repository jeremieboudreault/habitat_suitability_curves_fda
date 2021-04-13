# s1_clean_field_data.R


# Import and clean the field data and produce nice ready-to-use files.


# Project : habitat_suitability_curves_fsa
# Author  : Jeremie Boudreault
# Email   : JeremieBoudreault11@gmail.com
# Depends : R (v3.6.3)
# License : CC BY-NC-ND 4.0



# Library ----------------------------------------------------------------------


library(readxl)
library(data.table)


# Functions --------------------------------------------------------------------


source(file.path("R", "functions", "internals.R"))


# Import Sainte-Marguerite data ------------------------------------------------


# Importation of field data.
smr_data <- readxl::read_xlsx(
    path      =  file.path("data", "field_data",
                           "field_data_summer_2017_SMR.xlsx"),
    sheet     =  1L,
    skip      =  2L,
    col_names =  TRUE
)

# Convert to a data.table.
data.table::setDT(smr_data)

# Check the dim of the data.
dim(smr_data)

# Check the class of the variables
sapply(smr_data, class)

# Remove bad sites (i.e. where field work was impractible).
smr_data <- smr_data[SiteNew != 999, ]

# Rename, update and delete some columns.
smr_data <- smr_data[, .(
    RIVER       = "SMR",
    SITE        = as.integer(SiteNew),
    TRANSECT    = as.integer(Transect),
    PARCEL      = as.integer(Parcelle),
    GPS         = as.integer(GPS),
    DEPTH       = as.numeric(Depth),
    VELOCITY    = as.numeric(Velocity),
    TEMP        = as.numeric(Temp),
    D50         = as.numeric(D50),
    D84         = as.numeric(D84),
    ARE_TB      = as.logical(areTD),
    ARE_MB      = as.logical(areMB),
    BR_PERCENT  = as.numeric(BRPercent),
    N_FRY       = as.integer(nFry),
    N_PARR      = as.integer(nParr),
    N_SASA      = as.integer(nSalmon),
    N_RHCA      = as.integer(nRHCA),
    N_SAFO      = as.integer(nSAFO),
    N_UNKNOWN   = as.integer(nUnknown),
    FISH_OTHER  = as.character(FishOthers),
    LENGTH_1    = as.numeric(Length1),
    LENGTH_2    = as.numeric(Length2),
    LENGTH_3    = as.numeric(Length3),
    LENGTH_4    = as.numeric(Length4),
    LENGTH_5    = as.numeric(Length5),
    LENGTH_6    = as.numeric(Length6),
    LENGTH_7    = as.numeric(Length7),
    LENGTH_8    = as.numeric(Length8),
    LENGTH_9    = as.numeric(Length9),
    LENGTH_10   = as.numeric(Length10),
    LENGTH_11   = as.numeric(Length11)
)]

# Reorder the table by <SITE> and <PARCEL>.
data.table::setorderv(smr_data, c("SITE", "PARCEL"))

# Replaces NAs by 0 in number of fishes.
smr_data[is.na(N_FRY),  N_FRY  := 0L]
smr_data[is.na(N_PARR), N_PARR := 0L]
smr_data[is.na(N_SASA), N_SASA := 0L]


# Import Petite-Cascapédia data ------------------------------------------------


# Importation of field data.
pcr_data <- readxl::read_xlsx(
    path      =  file.path("data", "field_data",
                           "field_data_summer_2017_pcr.xlsx"),
    sheet     =  1L,
    skip      =  2L,
    col_names =  TRUE
)

# Convert to a data.table.
data.table::setDT(pcr_data)

# Check the dim of the data.
dim(pcr_data)

# Check the class of the variables
sapply(pcr_data, class)

# Remove parcels where D50 was bedrock (D50=10000), not fish found anyway
pcr_data <- pcr_data[D50 != 10000, ]

# Rename, update and delete some columns.
pcr_data <- pcr_data[, .(
    RIVER        = "PCR",
    SITE         = as.integer(SiteNew),
    TRANSECT     = as.integer(Transect),
    PARCEL       = as.integer(Parcelle),
    GPS          = as.integer(GPS),
    DEPTH        = as.numeric(Depth),
    VELOCITY     = as.numeric(Velocity),
    TEMP         = as.numeric(Temp),
    D50          = as.numeric(D50),
    D84          = as.numeric(D84),
    ARE_TB       = as.logical(areTD),
    ARE_MB       = as.logical(areMB),
    BR_PERCENT   = as.numeric(BRPercent),
    N_FRY        = as.integer(nFry),
    N_PARR       = as.integer(nParr),
    N_SASA       = as.integer(nSalmon),
    N_PCDPCN_YOY = as.integer(nPCDPCNMini),
    N_PCD        = as.integer(nPCD),
    N_PCN        = as.integer(nPCN),
    N_SAFO       = as.integer(nSAFO),
    N_UNKNOWN    = as.integer(nUnknown),
    FISH_OTHER   = as.character(FishOthers),
    LENGTH_1     = as.numeric(Length1),
    LENGTH_2     = as.numeric(Length2),
    LENGTH_3     = as.numeric(Length3),
    LENGTH_4     = as.numeric(Length4),
    LENGTH_5     = as.numeric(Length5),
    LENGTH_6     = as.numeric(Length6),
    LENGTH_7     = as.numeric(Length7),
    LENGTH_8     = as.numeric(Length8),
    LENGTH_9     = as.numeric(Length9),
    LENGTH_10    = as.numeric(Length10),
    LENGTH_11    = as.numeric(Length11)
)]

# Reorder the table by <SITE> and <PARCEL.
data.table::setorderv(pcr_data, c("SITE", "PARCEL"))

# Replaces NAs by 0 in number of fishes.
pcr_data[is.na(N_FRY),  N_FRY  := 0L]
pcr_data[is.na(N_PARR), N_PARR := 0L]
pcr_data[is.na(N_SASA), N_SASA := 0L]


# Combine tables ---------------------------------------------------------------


data <- data.table::rbindlist(
    l         = list(smr_data, pcr_data),
    fill      = TRUE,
    use.names = TRUE
)


# Exports ----------------------------------------------------------------------


qs::qsave(
    x    = data,
    file = file.path("out", "tmp", "smr_pcr_cleaned_s1.qs")
)

