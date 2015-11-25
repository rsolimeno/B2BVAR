### Code below by R. Solimeno   21-Nov-2013
# Individual and Moving Range charts (XmRqcc function by Thomas Hopper)
# Use package "qcc" and "qualityTools" to produce a graphical 4-pack output


# Individual and Moving Range charts (XmRqcc function by Thomas Hopper)
source("XmRqcc.R")

# Use package "qcc" and "qualityTools" to produce a graphical 4-pack output
library(qcc)
library(qualityTools)

data <- read.csv("twk2080h2015.csv", header = TRUE)


# Adjust LSL and USL as needed to achieve a desired Cpk
TARGET = 0.0
LSL = -1
USL = 1

#replace "test" with the TESTID value desired for analysis
test.data <- data[which(data$testid=="test"),]

# Create variable for test name to label various plots
Test.name = "ST KRUSS25"
test.data <- data[which(data$testid==Test.name),]

x <- test.data$nresult
objx <- qcc(x, 
            type = "xbar.one",
            plot=FALSE)

# Set CharacterEXpansion to accomodated desired text size
qcc.options("cex.stats" = 0.65)

# Define desired arrangement for multiple plots
# note: this 4-pack is formatted 2 x 1 due to the bundled output of 
# the XmRqcc function, attempting 2 x 2 format printing works but 
# control charts will appear side-by-side
par(mfrow = c(2, 2),
    cex.axis = 0.5,
    cex.lab = 0.75,
    cex.main = 0.95,
    cex.sub = 0.5)

speclimits <- c(LSL, USL)

# Call functions to create IMR charts
IMR <- XmRqcc(x, title.X=paste("Individuals Chart -", Test.name),
              ylab.X="Inidviduals",
              xlab.X="observation",
              title.mR=paste("Moving Range Chart -", Test.name),
              ylab.mR="Range", xlab.mR="Moving Range",
              print=FALSE, restore.par=FALSE)
 

# Execute the capability analysis
process.capability(objx,
                   speclimits,
                   target=TARGET,
                   restore.par=FALSE,
                   print=TRUE)

# Calculate overall mean, spec limits for capability analysis
xdbar <- mean(x)

# Check fit to a normal distribution for validity of results
ppPlot(x, "normal")

