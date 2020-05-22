################################################################################
#    Predicting habitat suitability curves through FDA 🏞                      #
#    Created by :     Jeremie Boudreault (Jeremie.Boudreault@ete.inrs.ca)      #
#    Current file :   Data_salmon_lengths.R                                    #
#    Aim :            Convert salmon lengths into number of fish (0+, 1+, 2+)  #
################################################################################

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
    Value    = c(30, 55, 88, 125, 43, 65, 106, 132)
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

