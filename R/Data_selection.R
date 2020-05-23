#  --------------------------------------------------------------------------- #
#    Predicting habitat suitability curves through FDA 🏞                      #
#    Created by :     Jeremie Boudreault (Jeremie.Boudreault@ete.inrs.ca)      #
#    Current file :   Data_selection.R                                         #
#    Aim :            Select the variable and sites we will be working on      #
#  --------------------------------------------------------------------------- #

# Part 0 : Libraries, functions and variable definition ------------------------

# Import library
library("data.table")    # Working with data.table
library("ggplot2")        # Plotting in ggplot

# Import clean data of SMR and PCR
RIVERDATA <- readRDS("data/River_data_combined_with_fish_numbers.Rds")


# Part 1 - Generate some very low level statistics -----------------------------

# Calculate some basic statistics about per site data
RIVERDATA_SITE <- RIVERDATA[,
                            list(nFry = sum(nFry_tot),
                                 nObs_Fry = sum(nFry_tot > 0),
                                 nParr = sum(nParr_tot),
                                 nObs_Parr = sum(nParr_tot > 0)),
                            by=c('River', 'Site')]

