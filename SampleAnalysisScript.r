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
stat.names <- dimnames(t.5$summary[[1]])[2][[1]]
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