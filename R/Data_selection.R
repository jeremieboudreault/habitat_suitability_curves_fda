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

