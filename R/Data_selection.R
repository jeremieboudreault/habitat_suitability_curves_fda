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

# Minimum number of observed fish at each site to be used
nObs.min <- 4

# Summary of the usable sites
RIVERDATA_SITE[, list(nSites_Fry = sum(nObs_Fry >= eval(nObs.min)),
                      nSites_Parr = sum(nObs_Parr >= eval(nObs.min)))]

# Seeing the above results, the maximim will be to have at least 4 observations
# of used of  parr habitat per site, resulting in n=36 sites

# Part 2 - Generate the file of the retained data ------------------------------

# Chosen parameter
nObs.min <- 4
Fish.var <- 'nObs_Parr'

# Retain the selected sites only - Parr with 4+ observations
SELECTED_SITES <- RIVERDATA_SITE[get(Fish.var) >= eval(nObs.min), list(River, Site)]
SELECTED_SITES <- SELECTED_SITES[order(River, decreasing=T), ]
SELECTED_SITES$NewSite <- 1:nrow(SELECTED_SITES)

# Left-join RIVERDATA
RIVERDATA_CLEAN <- RIVERDATA[SELECTED_SITES, , on=c('River', 'Site')]

# Rearrange the data frame
RIVERDATA_CLEAN <- RIVERDATA_CLEAN[, list(River,
                                          Site,
                                          NewSite,
                                          Parcelle,
                                          Y = nParr_tot,
                                          Velocity,
                                          Depth,
                                          D50)]

# Save the output
saveRDS(object   = RIVERDATA_CLEAN,
        file     = "data/River_data_combined_final.Rds",
        compress = "xz")
