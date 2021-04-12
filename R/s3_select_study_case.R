# s3_select_study_case.R


# Select optimal study case to be modelled (0+, 1+, 2+).


# Project : habitat_suitability_curves_fsa
# Author  : Jeremie Boudreault
# Email   : JeremieBoudreault11@gmail.com
# Depends : R (v3.6.3)
# License : CC BY-NC-ND 4.0



# Library ----------------------------------------------------------------------


library(data.table)
library(ggplot2)


# Imports ----------------------------------------------------------------------


# Cleaned data of SMR and PCR.
data <- qs::qread(
    file.path("out", "final", "smr_pcr_cleaned_final.qs")
)


# Part 1 - Generate some very low level statistics -----------------------------

# Calculate some basic statistics about per site data
data_SITE <- data[,
                            list(nFry = sum(nFry_tot),
                                 nObs_Fry = sum(nFry_tot > 0),
                                 nParr = sum(nParr_tot),
                                 nObs_Parr = sum(nParr_tot > 0)),
                            by=c('River', 'Site')]

# Minimum number of observed fish at each site to be used
nObs.min <- 4

# Summary of the usable sites
data_SITE[, list(nSites_Fry = sum(nObs_Fry >= eval(nObs.min)),
                      nSites_Parr = sum(nObs_Parr >= eval(nObs.min)))]

# Seeing the above results, the maximim will be to have at least 4 observations
# of used of  parr habitat per site, resulting in n=36 sites

# Part 2 - Generate the file of the retained data ------------------------------

# Chosen parameter
nObs.min <- 4
Fish.var <- 'nObs_Parr'

# Retain the selected sites only - Parr with 4+ observations
SELECTED_SITES <- data_SITE[get(Fish.var) >= eval(nObs.min), list(River, Site)]
SELECTED_SITES <- SELECTED_SITES[order(River, decreasing=T), ]
SELECTED_SITES$NewSite <- 1:nrow(SELECTED_SITES)

# Left-join data
data_CLEAN <- data[SELECTED_SITES, , on=c('River', 'Site')]

# Rearrange the data frame
data_CLEAN <- data_CLEAN[, list(River,
                                          Site,
                                          NewSite,
                                          Parcelle,
                                          Y = nParr_tot,
                                          Velocity,
                                          Depth,
                                          D50)]

# Save the output
saveRDS(object   = data_CLEAN,
        file     = "data/River_data_combined_final.Rds",
        compress = "xz")
