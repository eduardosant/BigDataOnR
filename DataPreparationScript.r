#!/usr/bin/Rscript
## big.R - Preprocess the airline data
## Copyright © 2010 Allan Engelhardt (http://www.cybaea.net/)

## Install the packages we will use
install.packages("bigmemory",
                 dependencies = c("Depends", "Suggests", "Enhances"))

## Data sets are downloaded from the Data Expo '09 web site at
## http://stat-computing.org/dataexpo/2009/the-data.html
for (year in 1987:2008) {
  file.name <- paste(year, "csv.bz2", sep = ".")
  if ( !file.exists(file.name) ) {
    url.text <- paste("http://stat-computing.org/dataexpo/2009/",
                      year, ".csv.bz2", sep = "")
    cat("Downloading missing data file ", file.name, "\n", sep = "")
    download.file(url.text, file.name)
  }
}

## Read sample file to get column names and types
d <- read.csv("2008.csv.bz2")
integer.columns <- sapply(d, is.integer)
factor.columns  <- sapply(d, is.factor)
factor.levels   <- lapply(d[, factor.columns], levels)
n.rows <- 0L

## Process each file determining the factor levels
## TODO: Combine with next loop
for (year in 1987:2008) {
  file.name <- paste(year, "csv.bz2", sep = ".")
  cat("Processing ", file.name, "\n", sep = "")
  d <- read.csv(file.name)
  n.rows <- n.rows + NROW(d)
  new.levels <- lapply(d[, factor.columns], levels)
  for ( i in seq(1, length(factor.levels)) ) {
    factor.levels[[i]] <- c(factor.levels[[i]], new.levels[[i]])
  }
  rm(d)
}
save(integer.columns, factor.columns, factor.levels, file = "factors.RData")

## Now convert all factors to integers so we can create a bigmatrix of the data
col.classes <- rep("integer", length(integer.columns))
col.classes[factor.columns] <- "character"
cols  <- which(factor.columns)
first <- TRUE
csv.file <- "airlines.csv"   # Write combined integer-only data to this file
csv.con  <- file(csv.file, open = "w")

for (year in 1987:2008) {
  file.name <- paste(year, "csv.bz2", sep = ".")
  cat("Processing ", file.name, "\n", sep = "")
  d <- read.csv(file.name, colClasses = col.classes)
  ## Convert the strings to integers
  for ( i in seq(1, length(factor.levels)) ) {
    col <- cols[i]
    d[, col] <- match(d[, col], factor.levels[[i]])
  }
  write.table(d, file = csv.con, sep = ",", 
              row.names = FALSE, col.names = first)
  first <- FALSE
}
close(csv.con)

## Now convert to a big.matrix
library("bigmemory")
backing.file    <- "airlines.bin"
descriptor.file <- "airlines.des"
data <- read.big.matrix(csv.file, header = TRUE,
                        type = "integer",
                        backingfile = backing.file,
                        descriptorfile = descriptor.file,
                        extraCols = c("age"))

Sample analysis

All done now. Sample analysis:
  
  ## bigScale.R - Replicate the analysis from http://bit.ly/aTFXeN with normal R
  ##   http://info.revolutionanalytics.com/bigdata.html
  ## See big.R for the preprocessing of the data
  
  ## Load required libraries
  library("biglm")
library("bigmemory")
library("biganalytics")
library("bigtabulate")

## Use parallel processing if available
## (Multicore is for "anything-but-Windows" platforms)
if ( require("multicore") ) {
  library("doMC")
  registerDoMC()
} else {
  warning("Consider registering a multi-core 'foreach' processor.")
}

day.names <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
               "Saturday", "Sunday")

## Attach to the data
descriptor.file <- "airlines.des"
data <- attach.big.matrix(dget(descriptor.file))

## Replicate Table 5 in the Revolutions document:
## Table 5
t.5 <- bigtabulate(data,
                   ccols = "DayOfWeek",
                   summary.cols = "ArrDelay", summary.na.rm = TRUE)
## Pretty-fy the outout
stat.names <- dimnames(t.5.2$summary[[1]])[2][[1]]
t.5.p <- cbind(matrix(unlist(t.5$summary), byrow = TRUE,
                      nrow = length(t.5$summary),
                      ncol = length(stat.names),
                      dimnames = list(day.names, stat.names)),
               ValidObs = t.5$table)
print(t.5.p)
#             min  max     mean       sd    NAs ValidObs
# Monday    -1410 1879 6.669515 30.17812 385262 18136111
# Tuesday   -1426 2137 5.960421 29.06076 417965 18061938
# Wednesday -1405 2598 7.091502 30.37856 405286 18103222
# Thursday  -1395 2453 8.945047 32.30101 400077 18083800
# Friday    -1437 1808 9.606953 33.07271 384009 18091338
# Saturday  -1280 1942 4.187419 28.29972 298328 15915382
# Sunday    -1295 2461 6.525040 31.11353 296602 17143178

## Figure 1
plot(t.5.p[, "mean"], type = "l", ylab="Average arrival delay")