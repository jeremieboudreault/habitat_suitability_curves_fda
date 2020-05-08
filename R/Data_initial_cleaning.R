################################################################################
#    Predicting habitat suitability curves through FDA 🏞                      #
#    Created by :     Jeremie Boudreault (Jeremie.Boudreault@ete.inrs.ca)      #
#    Current file :   Data_initial_cleaning.R                                  #
#    Aim :            Clean the field data and produce nice files to use       #
################################################################################

##### Part 0 : Libraries, functions and variable definition

# Import library
library("readxl")        # Importing Excel files (Field data)
library("data.table")    # Working with data.table

##### Part 1 : Importation of field data for the Sainte-Marguerite River

# Importation of SMR data
SMR.raw <- read_xlsx(path      =  "data/field data/Field_data_summer_2017_SMR.xlsx",
                     sheet     =  1,
                     skip      =  2,
                     col_names =  T)

# Convert to a data.table
setDT(SMR.raw)

# Check the class of the variables
sapply(SMR.raw, class)

# Remove a site where field work was imprecise
SMR.raw <- SMR.raw[SiteNew != 999, ]

# Change some class of the variables and delete some
SMR.raw[, `:=`(Site      = as.integer(SiteNew),
               Transect  = as.integer(Transect),
               Parcelle  = as.integer(Parcelle),
               GPS       = as.integer(GPS),
               areTD     = as.logical(areTD),
               areMB     = as.logical(areMB),
               nFry      = as.integer(nFry),
               nParr     = as.integer(nParr),
               nSalmon   = as.integer(nSalmon),
               nRHCA     = as.integer(nRHCA),
               nSAFO     = as.integer(nSAFO),
               nUnknown  = as.integer(nUnknown),
               SiteNew   = NULL,                   # Remove SiteNew
               BRPercent = NULL                    # Remove BRPercent
)]

# Reordering the data.table
SMR.raw <- SMR.raw[order(Site, Parcelle), ]

# Replaces NAs by 0 in number of fishes
SMR.raw[is.na(nFry), nFry := 0]
SMR.raw[is.na(nParr), nParr := 0]
SMR.raw[is.na(nSalmon), nSalmon := 0]

# Save the file in the data folder
saveRDS(SMR.raw, 'data/SMR_2017_field_data_clean.Rds')
