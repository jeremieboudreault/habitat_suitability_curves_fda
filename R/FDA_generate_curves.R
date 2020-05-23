#  --------------------------------------------------------------------------- #
#    Predicting habitat suitability curves through FDA 🏞                      #
#    Created by :     Jeremie Boudreault (Jeremie.Boudreault@ete.inrs.ca)      #
#    Current file :   FDA_generate_curves.R                                    #
#    Aim :            Generate functional habitat suitability curves           #
#  --------------------------------------------------------------------------- #

# Part 0 : Libraries, functions and variable definition ------------------------

# Import library
library("data.table")    # Working with data.table
library("ggplot2")       # Plotting in ggplot

# Import final dataset of the used sites
RIVERDATA <- readRDS("data/River_data_combined_final.Rds")

# Variable - nKnots :: the number of knots for the density estimate
nKnots <- 2^9

# Function - findRange :: to find the range of a vector c(xmin, xmax)
findRange <- function(x, liminf="yes", factor=1.1) {
    if (liminf == "yes")
        liminf = min(x, na.rm=T)/factor
    return(c(liminf, max(x, na.rm=TRUE) * factor))
}

# Function - std01 :: Standardise curve to 0-1
curve01 <- function(data, adjust=2) {
    a <- density(unlist(data), adjust = adjust)
    a$y <- a$y/max(a$y)
    data.table(x=a$x, y=a$y)
}

# Variable - Vars :: the variable we are dealing with
Vars <- c('Velocity', 'Depth', 'D50')

# Part 1 : Generate a data.table of availability and selection  ----------------

# Create a data.table of available and used characteristics
RIVERDATA_AVAIL <- melt(RIVERDATA, measure.vars = c("Velocity", "Depth", "D50"))
RIVERDATA_USED <- melt(RIVERDATA[Y > 0, ], measure.vars = c("Velocity", "Depth", "D50"))

# Add an indicator
RIVERDATA_AVAIL$TYPE <- "Availability"
RIVERDATA_USED$TYPE <- "Selection"

# Merge back the two data.table
RIVERDATA_MELT <- rbind(RIVERDATA_AVAIL, RIVERDATA_USED)
