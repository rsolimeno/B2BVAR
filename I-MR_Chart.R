#' Wrapper function to simplify the creation of XmR (IMR) charts using the qcc package.
#' 
#' By default, generates both the Individuals and Moving Range graphs.
#' Calls the qcc function once to generate the individuals chart using
#' type = "xbar.one" and a second call to generate the moving range
#' chart by transforming the data to a 2x(n-1) matrix and calling
#' the qcc function with type="R". 
#' Returns a list containing two qcc objects, one for the individuals
#' chart and the second for the moving range chart.
#' 
#' @name XmRqcc
#' @author Thomas Hopper \email{tomhopper@gmail.com}
#' @param x Required. A column vector of individual observations. Must be type double or integer.
#' @param title.X Optional. The title for the individuals chart.
#' @param title.mR Optional. The title for the moving range chart.
#' @param xlab.X Optional. The label for the x axis on the individuals chart.
#' @param ylab.X Optional. The label for the y axis on the individuals chart.
#' @param xlab.mR Optional. The label for the x axis on the moving range chart.
#' @param ylab.mR Optional. The label for the y axis on the moving range chart.
#' @param ... Other qcc parameters will be passed through directly to both  qcc 
#' @return \item{XmRqcc()[[1]]}{contains the individuals (type = xbar.one) qcc object}
#' @return \item{XmRqcc()[[2]]}{contains the moving range (type = R) qcc object}
#' @references http://cran.r-project.org/web/packages/qcc/qcc.pdf
#' @references http://www.r-project.org/doc/Rnews/Rnews_2004-1.pdf
#' @references http://www.qualitydigest.com/inside/twitter-ed/individual-charts-done-right-and-wrong.html
libresult <- require(qcc)
if(libresult == FALSE)
  stop("Could not load library qcc. XmRqcc relies on qcc.")

XmRqcc <- function(x, title.X = "Individuals Chart", ylab.X = "Individuals", xlab.X = "Observation", title.mR = "Moving Range Chart", ylab.mR = "Range", xlab.mR = "Range", ...) {
  #Error checking
  #print(sprintf("type of x = %s", typeof(x)))
  if (typeof(x) != c("double") && typeof(x) != c("integer"))
    stop("XmRqcc only accepts numbers of type integer or double.")
  if (NCOL(x) > 1)
    stop("XmRqcc only accepts a column vector; you have supplied more than one column")
  if (length(x) <= 1)
    stop("XmRqcc needs at least two values to create control charts. You have supplied less than one.")
  
  # Create a QCC object of type xbar.one (qcc's term for 
  # individuals charts) and plot the individuals graph.
  i.X <- qcc(data = x, 
             type = "xbar.one", 
             title = title.X, 
             ylab = ylab.X, 
             xlab = xlab.X, 
             std.dev = c("MR"), ...)
  
  i.adj <- matrix(nrow = length(x) - 1, ncol = 2)
  
  i.adj[, 1] <- x[1:(length(x) - 1)]
  i.adj[, 2] <- x[2:length(x)]
  
  i.mR <- qcc(data = i.adj, 
              type = "R", 
              title = title.mR, 
              ylab = ylab.mR, 
              xlab = xlab.mR,
              std.dev = c("UWAVE-R"), ...)
  rval <- list(i.X, i.mR)
  return(rval)
}



### Code below by R. Solimeno   21-Nov-2013
# Individual and Moving Range charts (XmRqcc function by Thomas Hopper)
# Use package "qcc" and "qualityTools" to produce a graphical 4-pack output


data <- read.csv("XRdata.csv", header = TRUE)

# For data files with MULTIPLE metrics:
# replace "test" with the TESTID value desired for analysis, and uncomment the next line:
# test.data <- data[which(data$TESTID=="test"),]

# For multiple metrics, replace "x <- data$Dev"  with "x <- test.data$Dev" on next line:
x <- data$Dev
obj <- qcc(x, type = "xbar.one")


qcc.options("cex.stats" = 0.65)
par(mfrow = c(2, 2),
    cex.axis = 0.5,
    cex.lab = 0.75,
    cex.main = 0.95,
    cex.sub = 0.5)

# replace "test" with the TESTID name for the chart titles
XmRqcc(x, 
       title.X = "Individuals Chart - test",
       ylab.X = "Inidviduals",
       xlab.X = "observation",
       title.mR = "Moving Range Chart - test",
       ylab.mR = "Range", xlab.mR="Moving Range",
       print = TRUE)


xdbar <- mean(x)

# Adjust LSL and USL as needed to achieve a desired Cpk
TARGET = 0.0
LSL = -4
USL = 4

speclimits <- c(LSL, USL)

process.capability(obj,
                   speclimits,
                   target = TARGET,
                   print = TRUE)


# Check fit to a normal distribution for validity of results
library(qualityTools)
ppPlot(x, "normal")
