################################################################################
#    Predicting habitat suitability curves through FDA 🏞                      #
#    Created by :     Jeremie Boudreault (Jeremie.Boudreault@ete.inrs.ca)      #
#    Current file :   Data_salmon_lengths.R                                    #
#    Aim :            Convert salmon lengths into number of fish (0+, 1+, 2+)  #
################################################################################

##### Part 0 : Libraries, functions and variable definition

# Import library
library("data.table")    # Working with data.table

# Import raw data of SMR and PCR
SMR.raw <- readRDS("data/SMR_2017_field_data_clean.Rds")
PCR.raw <- readRDS("data/PCR_2017_field_data_clean.Rds")

