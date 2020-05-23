#  --------------------------------------------------------------------------- #
#    Predicting habitat suitability curves through FDA 🏞                      #
#    Created by :     Jeremie Boudreault (Jeremie.Boudreault@ete.inrs.ca)      #
#    Current file :   Data_salmon_numbers.R                                    #
#    Aim :            Convert salmon lengths into number of fish (0+, 1+, 2+)  #
#  --------------------------------------------------------------------------- #

# Part 0 : Libraries, functions and variable definition ------------------------

# Import library
library("data.table")    # Working with data.table
library("ggplot2")        # Plotting in ggplot

# Import raw data of SMR and PCR
SMR.raw <- readRDS("data/SMR_2017_field_data_clean.Rds")
PCR.raw <- readRDS("data/PCR_2017_field_data_clean.Rds")


# Part 1 : Lengths of salmon visualisation for the two rivers ------------------

# Extract the length from the data.frame
SMR.lengths <- as.vector(na.omit(unlist(SMR.raw[, paste0('Length', 1:11)])))
PCR.lengths <- as.vector(na.omit(unlist(PCR.raw[, paste0('Length', 1:11)])))

# Create a data.table from the two river
All.lengths <- data.table(
    River  = c(rep('SMR', length(SMR.lengths)), rep("PCR", length(PCR.lengths))),
    Length = c(SMR.lengths, PCR.lengths)
)

# Producing the graph of the length for the SMR and PCR
print(ggplot(data=All.lengths, aes(x=Length)) +
          geom_histogram(binwidth = 2.5, boundary=0) +
          xlab('Juvenile salmons lengths (mm)') +
          scale_x_continuous(breaks=2:14 * 10) +
          ggtitle('Juvenile salmons lengths distribution') +
          facet_wrap(~River, nrow=2, scales="free"))

# Extract minimum and maximum
All.lengths[, list(min.length=min(Length), max.length=max(Length)), by=River]

# Limits of the SMR and PCR
Lengths.lim <- data.table(
    River    = rep(c('SMR', 'PCR'), each=4),
    Limit    = rep(c("FryMin", "Parr1Min", "Parr2Min", "Parr2Max"), length.out=8),
    Value    = c(30, 55, 88, 125, 43, 63, 93, 132)
)

# Producing the graph of the lengths + Limits
pdf('out/data visualisation/Salmon_lengths.pdf', width=5, height=6)
print(ggplot(data=All.lengths, aes(x=Length)) +
          geom_histogram(aes(group=River), boundary=0, binwidth=2.5,  fill='#c1c1c1', color='#636363', lwd=0.25) +
          xlab('Length (mm)') +
          ylab('Number of juvenile salmon') +
          scale_x_continuous(breaks=3:14 * 10) +
          scale_y_continuous(limits = c(0, 85)) +
          geom_vline(data = Lengths.lim, aes(xintercept=Value), color='red', lwd=1, lty=3) +
          #geom_text(data = data.frame(x=c(40, 70, 105), y=c(80, 35, 15), lab=c('Fry (0+)', 'Parr (1+)', 'Parr (2+)')), aes(x=x, y=y, label=lab), size=3) +
          facet_wrap(~River, nrow=2, scales="free")
)
dev.off()

# Part 2 : Create a new variable to convert lengths to number of salmon --------

# First dcast the above data.frame into a more convenient way
Lengths.lim.dcast <- dcast(Lengths.lim, River~Limit, value.var = "Value")

# Merge two data.table
SMR.raw$River <- "SMR"
PCR.raw$River <- "PCR"
COMB.raw <- rbind(SMR.raw, PCR.raw, fill=T)

# Merge the lengths limit and the data
COMB.raw <- COMB.raw[Lengths.lim.dcast, , on="River"]

# Calculate number of Fry, Parr 1+ and Parr 2+ from lengths at SMR
COMB.raw$nFry_M <- sapply(1:nrow(COMB.raw), function(w) sum(COMB.raw[w, paste0('Length', 1:11)] < COMB.raw$Parr1Min[w], na.rm=T))
COMB.raw$nParr1_M <- sapply(1:nrow(COMB.raw), function(w) sum(COMB.raw[w, paste0('Length', 1:11)] >= COMB.raw$Parr1Min[w] & COMB.raw[w, paste0('Length', 1:11)] < COMB.raw$Parr2Min[w], na.rm=T))
COMB.raw$nParr2_M <- sapply(1:nrow(COMB.raw), function(w) sum(COMB.raw[w, paste0('Length', 1:11)] >= COMB.raw$Parr2Min[w] & COMB.raw[w, paste0('Length', 1:11)] < COMB.raw$Parr2Max[w], na.rm=T))
COMB.raw$nParr3_M <- sapply(1:nrow(COMB.raw), function(w) sum(COMB.raw[w, paste0('Length', 1:11)] >= COMB.raw$Parr2Max[w], na.rm=T))

# Validation
COMB.raw[, list(nFry = sum(nFry_M),
                nParr1 = sum(nParr1_M),
                nParr2 = sum(nParr2_M),
                nParr3 = sum(nParr3_M)), by = list(River, Site)]

# Calculate the total number of fish
COMB.raw[ , nFry_tot    := as.integer(nFry + nFry_M) ]
COMB.raw[ , nParr_M     := as.integer(nParr1_M + nParr2_M + nParr3_M) ]
COMB.raw[ , nParr1_tot  := as.integer(nParr1_M) ]
COMB.raw[ , nParr2_tot  := as.integer(nParr2_M) ]
COMB.raw[ , nParr_tot   := as.integer(nParr + nParr_M) ]
COMB.raw[ , nSalmon_M   := as.integer(nFry_M + nParr_M) ]
COMB.raw[ , nSalmon_tot := as.integer(nFry_tot + nParr_tot + nSalmon) ]

# Here, we will only keep the variable of interest and reorganise the data.frame
names(COMB.raw)

# Cleaning of the dataset
COMB.clean <- COMB.raw[, list(River, Site, Parcelle, nFry_M, nFry_tot, nParr1_M, nParr1_tot, nParr2_M, nParr2_tot, nParr_M, nParr_tot, nSalmon_M, nSalmon_tot, Velocity, Depth, Temp, D50, D84)]

# Verify is there is any NAs
which(is.na(COMB.clean), arr.ind=T)

# Verify the class of the variables
sapply(COMB.clean, class)

# Save the dataset
saveRDS(object   = COMB.clean,
        file     = "data/River_data_combined_2017_with_nsalmon.Rds",
        compress = "xz")


