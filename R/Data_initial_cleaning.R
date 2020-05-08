################################################################################
#    Predicting habitat suitability curves through FDA 🏞                      #
#    Created by :     Jeremie Boudreault (Jeremie.Boudreault@ete.inrs.ca)      #
#    Current file :   Data_initial_cleaning.R                                  #
#    Aim :            Clean the field data and produce nice files to use       #
################################################################################

##### Part 0 : Libraries, functions and variable definition

# Import library
library('xlsx')          # Importing Excel files (Field data)
library('data.table')    # Working with data.table
