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
pdf("out/data visualisation/Available_versus_selected_habitat.pdf", width=8, height=4)
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

# Part 2 : Get the range of each habitat characteristics -----------------------

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

# Save the RANGE.TBL as a temporary output
saveRDS(RANGE.TBL, "otmpRANGE_TBL.rds", compress="xz")

# Part 3 : Generate curves of availability and selection  ----------------------

# Generate availability curves at each site
SITES <- unique(RIVERDATA$NewSite)
NSITES <- length(SITES) 
# Function to fit a curve, scale it to 0 - 1 and adjust kernel estimate
curve01 <- function(data, range, npoints = 2^7, adjust=1) {
    a <- density(x = unlist(data),
                 bw = "SJ",
                 adjust = 1,
                 kernel = "gaussian",
                 n = npoints,
                 from = unlist(range)[1],
                 to = unlist(range)[2],)
    a$y <- a$y/max(a$y)
    data.table(x=a$x, y=a$y)
}

# Generate curve01 of availability for all sites
CURVES_DEPTH <- curve01(RIVERDATA_AVAIL[Site == 1 & variable == "Depth", value], RANGE_TBL[, "Depth"])
CURVES_DEPTH <- cbind(CURVES_DEPTH,
                      sapply(2:NSITES, function(w) curve01(RIVERDATA_AVAIL[NewSite == eval(w) & variable == "Depth", value], RANGE_TBL[, "Depth"])$y))
CURVES_DEPTH <- cbind(CURVES_DEPTH,
                      sapply(1:NSITES, function(w) curve01(RIVERDATA_USED[NewSite == eval(w) & variable == "Depth", value], RANGE_TBL[, "Depth"])$y))
c
olnames(CURVES_DEPTH) <- c("s", paste0("x_", SITES)), paste0("y_", SITES)

