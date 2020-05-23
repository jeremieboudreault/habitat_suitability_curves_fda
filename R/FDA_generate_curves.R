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
library("ggforce")       # Paginates

# Import final dataset of the used sites
RIVERDATA <- readRDS("data/River_data_combined_final.Rds")

# Variable - Vars :: the variable we are dealing with
Vars <- c('Velocity', 'Depth', 'D50')

# Part 1 : Generate a data.table of availability and selection  ----------------

# Create a data.table of available and used characteristics
RIVERDATA_AVAIL <- melt(RIVERDATA, measure.vars = Vars)
RIVERDATA_USED <- melt(RIVERDATA[Y > 0, ], measure.vars = Vars)

# Add an indicator
RIVERDATA_AVAIL$TYPE <- "Availability"
RIVERDATA_USED$TYPE <- "Selection"

# Merge back the two data.table
RIVERDATA_MELT <- rbind(RIVERDATA_AVAIL, RIVERDATA_USED)

# Plotting availability of each HC versus selection
pdf("out/data visualisation/Availability_selection_overall.pdf", width=8, height=4)
print(
    ggplot(RIVERDATA_MELT, aes(x = value)) +
        geom_histogram(aes(fill=TYPE, y = ..density..), color="grey90", lwd=0.5, position = "dodge", bins=8) +
        facet_wrap(~variable, nrow=2, ncol=3, scales="free") +
        labs(fill="") +
        theme(legend.position="bottom")  +
        ylab("Probability density function (PDF)") +
        xlab("Velocity (m/s)                                            Depth (cm)                                           D50 (mm)") +
        scale_fill_manual(values = c("#9B9B93", "#63B0CD"))
)
dev.off()

# Part 3 : Get the range of each habitat characteristics -----------------------

# Function - findRange :: to find the range of a vector c(xmin, xmax)
findRange <- function(x, liminf="yes", factor=1) {
    if (liminf == "yes")
        liminf = min(x, na.rm=T)/factor
    return(c(liminf, max(x, na.rm=TRUE) * factor))
}

# Get the range of the three habitat characteristics
RANGE_TBL <- data.table(
    Limit     = c("lower", "upper"),
    Depth     = findRange(RIVERDATA$Depth, liminf = 0),
    Velocity  = findRange(RIVERDATA$Velocity, liminf = 0),
    D50       = findRange(RIVERDATA$D50, liminf = 0)
)

# Manual adjustment for this specific dataset
RANGE_TBL[2, 2] <- 100
RANGE_TBL[2, 3] <- 2
RANGE_TBL[2, 4] <- 300

# See the final result
RANGE_TBL

# Save the RANGE_TBL as a temporary output
saveRDS(RANGE_TBL, "out/tmp/RANGE_TBL.rds", compress="xz")

# Part 4 : Generate curves of availability and selection  ----------------------

# Generate availability curves at each site
SITES <- unique(RIVERDATA$NewSite)
NSITES <- length(SITES)

# Function to fit a curve, scale it to 0 - 1 and adjust kernel estimate
curve01 <- function(data, range, npoints = 2^8, adjust=2) {
    a <- density(x = unlist(data),
                 bw = 'nrd0',
                 adjust = 1,
                 kernel = "gaussian",
                 n = npoints,
                 from = unlist(range)[1],
                 to = unlist(range)[2],)
    a$y <- a$y/max(a$y)
    data.table(x=a$x, y=a$y)
}

# Generate curve01 of availability for all sites
CalcCurves <- function(w) {

    # Create an empty list
    CURVES <- list()

    # Generate an initial data.table to get the s values of the x(s) curve
    CURVES$sx<- curve01(RIVERDATA_AVAIL[NewSite == 1 & variable == w, value], RANGE_TBL[, get(w)])$
    CURVES$y <- curve01(RIVERDATA_AVAIL[NewSite == 1 & variable == w, value], RANGE_TBL[, get(w)])
    # Run on the NSITES availability curves x(s)
    CURVES$x <- scbind(CURVES$x, apply(12NSITES, function(z) curve01(RIVERDATA_AVAIL[NewSite == z & variable == w, value], RANGE_TBL[, get(w)])$y)
)
    # Run on the NSITES selection curves y(s)
    CURVES$y <- scbind(CURVES$y, apply(12NSITES, function(z) curve01(RIVERDATA_USED[NewSite == z & variable == w, value], RANGE_TBL[, get(w)])$y)
)
    # Return
    Ccolnames(CURVES$x) <- c("s", paste0("p", 1:NSITES))
    colnames(CURVES$y) <- c("s", paste0("p", 1:NSITES))

    CURVES}

# Generate curves
CURVES_LIST <- lapply(Vars, CalcCurves)
names(CURVES_LIST) <- Vars

## Melting the element of the list for graphing purpose#melt(CURVES_LIST[[1]]$x, id.vars="s")

 Plot the resulting curves
# Plotting availability versus selection at each site
pdf("out/data visualisation/Availability_selection_per_site.pdf")
for (var in Vars) {
    for (page.i in 1:2) {
        print(
            ggplot(CURVES_LIST[[1]]$s, aes(x = value)) +
                geom_density(aes(fill=TYPE), color="grey90", lwd=0.1, alpha=0.3, adjust =2) +
                #   geom_histogram(aes(fill=TYPE, y = ..density..), color="grey90", lwd=0.5, position = "dodge", bins=6) +
                facet_wrap_paginate(~NewSite, nrow=5, ncol=4, scales="free_y", page=page.i) +
                labs(fill="") +
                theme(legend.position="bottom")  +
                ylab("Probability density function (PDF)") +
                xlab("Value") +
                ggtitle(paste0("Habitat availability / selection for ", var)) +
                scale_fill_manual(values = c("#9B9B93", "#63B0CD"))
        )
    }
}
dev.off()


CURVES <- CalcCurves("Depth")
