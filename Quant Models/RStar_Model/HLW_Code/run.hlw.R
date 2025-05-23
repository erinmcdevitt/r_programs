#------------------------------------------------------------------------------#
# File:        run.hlw.R
#
# Description: This the main file for HLW, which does the following:
#              (1) Prepares data to be used in estimation
#              (2) Runs the three-stage HLW estimation for each economy
#              (3) Saves output.
#------------------------------------------------------------------------------#
rm(list=ls())

#------------------------------------------------------------------------------#
# Prepare data to be used in estimation.
#
# Output will be saved in the inputData folder.
#
# Set the data start and end dates manually in each prepare.rstar.data file
#------------------------------------------------------------------------------#
source("prepare.rstar.data.us.R")
source("prepare.rstar.data.ca.R")
source("prepare.rstar.data.ea.R")
source("prepare.rstar.data.uk.R")


#------------------------------------------------------------------------------#
# Load required packages and source all programs to be used in HLW estimation.
#------------------------------------------------------------------------------#
if (!require("tis")) {install.packages("tis"); library("tis")} # Time series package
if (!require("mFilter")) {install.packages("mFilter"); library("mFilter")} # HP filter
if (!require("nloptr")) {install.packages("nloptr"); library("nloptr")} # Optimization

# Source all R programs; see code guide for details of each
source("calculate.covariance.R")
source("format.output.R")
source("kalman.log.likelihood.R")
source("kalman.standard.errors.R")
source("kalman.states.R")
source("kalman.states.wrapper.R")
source("log.likelihood.wrapper.R")
source("median.unbiased.estimator.stage1.R") 
source("median.unbiased.estimator.stage2.R")
source("rstar.stage1.R")
source("rstar.stage2.R")
source("rstar.stage3.R")
source("run.hlw.estimation.R") 
source("unpack.parameters.stage1.R") 
source("unpack.parameters.stage2.R") 
source("unpack.parameters.stage3.R") 
source("utilities.R")


#------------------------------------------------------------------------------#
# Define variables
#------------------------------------------------------------------------------#
# Upper bound on a_3 parameter (slope of the IS curve)
a3.constraint <- -0.0025

# Lower bound on b_2 parameter (slope of the Phillips curve)
b2.constraint <- 0.025

# Set the start and end dates of the estimation sample (format is c(year,quarter))
sample.start <- c(1961,1) 
sample.end   <- c(2019,2)

# Set the estimation sample start date for the Euro Area 
ea.sample.start <- c(1972,1)

# The estimation process uses data beginning 4 quarters prior to the sample start
data.start    <- shiftQuarter(sample.start,-4)
ea.data.start <- shiftQuarter(ea.sample.start,-4)

# Set start index for y
g.pot.start.index <- 1 + ti(shiftQuarter(sample.start,-3),'quarterly')-ti(data.start,'quarterly')

# Set column names for CSV output
output.col.names <- c("Date","rstar","g","z","output gap","","All results are output from the Stage 3 model.",rep("",8),"Standard Errors","Date","y*","r*","g","","rrgap")

# Set number of iterations for Monte Carlo standard error procedure
niter <- 5000

# Because the MC standard error procedure is time consuming, we include a run switch
# Set run.se to TRUE to run the procedure
run.se <- TRUE

#------------------------------------------------------------------------------#
# United States: Read in data, run estimation, and save output
#------------------------------------------------------------------------------#
# Read in output of prepare.rstar.data.us.R
us.data <- read.table("inputData/rstar.data.us.csv",
                      sep = ',', na.strings = ".", header=TRUE, stringsAsFactors=FALSE)

us.log.output             <- us.data$gdp.log
us.inflation              <- us.data$inflation
us.inflation.expectations <- us.data$inflation.expectations
us.nominal.interest.rate  <- us.data$interest
us.real.interest.rate     <- us.nominal.interest.rate - us.inflation.expectations

# Run HLW estimation for the US
us.estimation <- run.hlw.estimation(us.log.output, us.inflation, us.real.interest.rate, us.nominal.interest.rate,
                                    a3.constraint = a3.constraint, b2.constraint = b2.constraint, run.se = run.se)

# One-sided (filtered) estimates
one.sided.est.us <- cbind(us.estimation$out.stage3$rstar.filtered,
                          us.estimation$out.stage3$trend.filtered,
                          us.estimation$out.stage3$z.filtered,
                          us.estimation$out.stage3$output.gap.filtered)

# Save one-sided estimates to CSV
write.table(one.sided.est.us, 'output/one.sided.est.us.csv', row.names = FALSE, col.names = c("rstar","g","z","output gap"), quote = FALSE, sep = ',', na = ".")

# Save output to CSV
output.us <- format.output(us.estimation, one.sided.est.us, us.real.interest.rate, sample.start, sample.end, run.se = run.se)
write.table(output.us, 'output/output.us.csv', col.names = output.col.names, quote=FALSE, row.names=FALSE, sep = ',', na = '')

#stop()

#------------------------------------------------------------------------------#
# Canada: Read in data, run estimation, and save output
#------------------------------------------------------------------------------#
# Read in output of prepare.rstar.data.ca.R
ca.data <- read.table("inputData/rstar.data.ca.csv",
                      sep = ',', na.strings = ".", header=TRUE, stringsAsFactors=FALSE)

ca.log.output             <- ca.data$gdp.log
ca.inflation              <- ca.data$inflation
ca.inflation.expectations <- ca.data$inflation.expectations
ca.nominal.interest.rate  <- ca.data$interest
ca.real.interest.rate     <- ca.nominal.interest.rate - ca.inflation.expectations

# Run HLW estimation for Canada
ca.estimation <- run.hlw.estimation(ca.log.output, ca.inflation, ca.real.interest.rate, ca.nominal.interest.rate,
                                    a3.constraint = a3.constraint, b2.constraint = b2.constraint, run.se = run.se)

# One-sided (filtered) estimates
one.sided.est.ca <- cbind(ca.estimation$out.stage3$rstar.filtered,
                          ca.estimation$out.stage3$trend.filtered,
                          ca.estimation$out.stage3$z.filtered,
                          ca.estimation$out.stage3$output.gap.filtered)

# Save one-sided estimates to CSV
write.table(one.sided.est.ca, 'output/one.sided.est.ca.csv', row.names = FALSE, col.names = c("rstar","g","z","output gap"), quote = FALSE, sep = ',', na = ".")

# Save output to CSV
output.ca <- format.output(ca.estimation, one.sided.est.ca, ca.real.interest.rate, sample.start, sample.end, run.se = run.se)
write.table(output.ca, 'output/output.ca.csv', col.names = output.col.names, quote=FALSE, row.names=FALSE, sep = ',', na = '')

#------------------------------------------------------------------------------#
# Euro Area: Read in data, run estimation, and save output
#------------------------------------------------------------------------------#
# Read in output of prepare.rstar.data.ea.R
ea.data <- read.table("inputData/rstar.data.ea.csv",
                      sep = ',', na.strings = ".", header=TRUE, stringsAsFactors=FALSE)

ea.log.output             <- ea.data$gdp.log
ea.inflation              <- ea.data$inflation
ea.inflation.expectations <- ea.data$inflation.expectations
ea.nominal.interest.rate  <- ea.data$interest
ea.real.interest.rate     <- ea.nominal.interest.rate - ea.inflation.expectations

# Run HLW estimation for the Euro Area
ea.estimation <- run.hlw.estimation(ea.log.output, ea.inflation, ea.real.interest.rate, ea.nominal.interest.rate,
                                    a3.constraint = a3.constraint, b2.constraint = b2.constraint, run.se = run.se)

# One-sided (filtered) estimates
one.sided.est.ea <- cbind(ea.estimation$out.stage3$rstar.filtered,
                          ea.estimation$out.stage3$trend.filtered,
                          ea.estimation$out.stage3$z.filtered,
                          ea.estimation$out.stage3$output.gap.filtered)

# Save one-sided estimates to CSV
write.table(one.sided.est.ea, 'output/one.sided.est.ea.csv', row.names = FALSE, col.names = c("rstar","g","z","output gap"), quote = FALSE, sep = ',', na = ".")

# Save output to CSV
output.ea <- format.output(ea.estimation, one.sided.est.ea, ea.real.interest.rate, ea.sample.start, sample.end, run.se = run.se)
write.table(output.ea, 'output/output.ea.csv', col.names = output.col.names, quote=FALSE, row.names=FALSE, sep = ',', na = '')


#------------------------------------------------------------------------------#
# United Kingdom: Read in data, run estimation, and save output
#------------------------------------------------------------------------------#
# Read in output of prepare.rstar.data.uk.R
uk.data <- read.table("inputData/rstar.data.uk.csv",
                      sep = ',', na.strings = ".", header=TRUE, stringsAsFactors=FALSE)

uk.log.output             <- uk.data$gdp.log
uk.inflation              <- uk.data$inflation
uk.inflation.expectations <- uk.data$inflation.expectations
uk.nominal.interest.rate  <- uk.data$interest
uk.real.interest.rate     <- uk.nominal.interest.rate - uk.inflation.expectations

# Run HLW estimation for the UK
uk.estimation <- run.hlw.estimation(uk.log.output, uk.inflation, uk.real.interest.rate, uk.nominal.interest.rate,
                                    a3.constraint = a3.constraint, b2.constraint = b2.constraint, run.se = run.se)

# One-sided (filtered) estimates
one.sided.est.uk <- cbind(uk.estimation$out.stage3$rstar.filtered,
                          uk.estimation$out.stage3$trend.filtered,
                          uk.estimation$out.stage3$z.filtered,
                          uk.estimation$out.stage3$output.gap.filtered)

# Save one-sided estimates to CSV
write.table(one.sided.est.uk, 'output/one.sided.est.uk.csv', row.names = FALSE, col.names = c("rstar","g","z","output gap"), quote = FALSE, sep = ',', na = ".")

# Save output to CSV
output.uk <- format.output(uk.estimation, one.sided.est.uk, uk.real.interest.rate, sample.start, sample.end, run.se = run.se)
write.table(output.uk, 'output/output.uk.csv', col.names = output.col.names, quote=FALSE, row.names=FALSE, sep = ',', na = '')
