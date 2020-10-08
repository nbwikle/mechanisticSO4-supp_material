
### so4-mcmc.R
### author: Nathan Wikle
###
### This R script generates samples from posterior distributions for 
### various models, via MCMC, replicating the analysis in the paper.
###
### IMPORTANT: Running this script will take a very long time (~110 hours).
###   If possible, it is best to run parts 2-5 in parallel.

################################################################################
###### 1. Load USA SO4 data
################################################################################

### load packages ###
library(raster)
library(maps)
library(Matrix)
library(TruncatedNormal)
library(mvnfast)

### read in source code ###
source("./src/functions.R")

### load data ###
usa <- readRDS("./data/central-usa-data.RDS")

################################################################################
###### 2. Averaged process, coupled model, with wind (BEST MODEL)
################################################################################

####################
### i. 2011 data ###
####################

### set-up data structures ###

# create D matrix
dims <- dim(usa$so4$so4.2011)
inds.d <- FVMInds(dims, "insulated")
D <- diffFVM(dims, inds.d, "insulated")

# create C matrix
inds.w <- FVMInds(dims, "periodic")
C <- createC(big = usa$wind.big$wind.2011, small = usa$wind.small$wind.2011,
             dims, inds = inds.w)

# X and y values
y.2011 <- values(usa$so4$so4.2011)
X.2011 <- usa$X$X.2011

### run MCMC ###

# initialize parameters
gamma.init <- 1000
xi.init <- 1
alpha.init <- 50
beta.init <- 1
s2.init <- 5000
delta.init <- 50

params.init <- list(gamma = gamma.init,
                    xi = xi.init,
                    beta = beta.init,
                    s2 = s2.init,
                    delta = delta.init,
                    alpha = alpha.init)

# generate MCMC samples !!! CAUTION: THIS MAY TAKE ~48 HOURS !!!
mcmc.res <- coupledMCMC.Wind(N = 150000, y = y.2011, X = X.2011,
  mats = list(D = D, C = C),
  params = params.init,
  priors = list(g.sd = 10000, xi = 0.01, s2 = 0.001, beta = 10, alpha.sd = 1000),
  update = 50, iter = 1000)

# save results
saveRDS(mcmc.res, file = "./output/avg2-usa2011-mcmc1.RDS")


################################################################################
###### 3. Averaged process, uncoupled model, with wind
################################################################################

####################
### i. 2011 data ###
####################

### set-up data structures ###

# create D matrix
dims <- dim(usa$so4$so4.2011)
inds.d <- FVMInds(dims, "insulated")
D <- diffFVM(dims, inds.d, "insulated")

# create C matrix
inds.w <- FVMInds(dims, "periodic")
C <- createC(big = usa$wind.big$wind.2011, small = usa$wind.small$wind.2011,
             dims, inds = inds.w)

# X and y values
y.2011 <- values(usa$so4$so4.2011)
X.2011 <- usa$X$X.2011

### run MCMC ###

# initialize parameters
gamma.init <- 1000
xi.init <- 1
alpha.init <- 50
beta.init <- 1
s2.init <- 5000
delta.init <- 50

params.init <- list(gamma = gamma.init,
                    xi = xi.init,
                    beta = beta.init,
                    s2 = s2.init,
                    delta = delta.init,
                    alpha = alpha.init)

# generate MCMC samples !!! CAUTION: THIS MAY TAKE ~16 HOURS !!!
mcmc.res <- uncoupledMCMC.W(N = 150000, y = y.2011, X = X.2011,
  mats = list(D = D, C = C),
  params = params.init,
  priors = list(g.sd = 10000, xi = 0.01, s2 = 0.001, beta = 10, alpha.sd = 1000),
  update = 50, iter = 1000)

# save results
saveRDS(mcmc.res, file = "./output/avg1-usa2011-mcmc1.RDS")


################################################################################
###### 4. Snapshot process, coupled model, no wind
################################################################################

### set-up data structures ###

# create D matrix
dims <- dim(usa$so4$so4.2011)
inds.d <- FVMInds(dims, "insulated")
D <- diffFVM(dims, inds.d, "insulated")

# create C matrix
inds.w <- FVMInds(dims, "periodic")
C <- createC(big = usa$wind.big$wind.2011, small = usa$wind.small$wind.2011,
             dims, inds = inds.w)

# X and y values
y.2011 <- values(usa$so4$so4.2011)
X.2011 <- usa$X$X.2011

### run MCMC ###

# initialize parameters
gamma.init <- 1000
xi.init <- 1
alpha.init <- 50
beta.init <- 1
s2.init <- 5000
delta.init <- 50

params.init <- list(gamma = gamma.init,
                    xi = xi.init,
                    beta = beta.init,
                    s2 = s2.init,
                    delta = delta.init,
                    alpha = alpha.init)

# generate MCMC samples !!! CAUTION: THIS MAY TAKE ~29 HOURS !!!
mcmc.res <- coupledMCMC.snap(N = 150000, y = y.2011, X = X.2011,
                             mats = list(D = D, C = C),
                             params = params.init,
                             priors = list(g.sd = 10000, xi = 0.01, s2 = 0.001, beta = 10, alpha.sd = 1000),
                             update = 50, iter = 1000)

# save results
saveRDS(mcmc.res, file = "./output/snap2-usa2011-mcmc1.RDS")


################################################################################
###### 5. Snapshot process, uncoupled model, no wind
################################################################################

####################
### i. 2011 data ###
####################

### set-up data structures ###

# create D matrix
dims <- dim(usa$so4$so4.2011)
inds.d <- FVMInds(dims, "insulated")
D <- diffFVM(dims, inds.d, "insulated")

# create C matrix
inds.w <- FVMInds(dims, "periodic")
C <- createC(big = usa$wind.big$wind.2011, small = usa$wind.small$wind.2011,
             dims, inds = inds.w)

# X and y values
y.2011 <- values(usa$so4$so4.2011)
X.2011 <- usa$X$X.2011

### run MCMC ###

# initialize parameters
gamma.init <- 1000
xi.init <- 1
alpha.init <- 50
beta.init <- 1
s2.init <- 5000
delta.init <- 50

params.init <- list(gamma = gamma.init,
                    xi = xi.init,
                    beta = beta.init,
                    s2 = s2.init,
                    delta = delta.init,
                    alpha = alpha.init)

# generate MCMC samples  !!! CAUTION: THIS MAY TAKE ~17 HOURS !!!
mcmc.res <- uncoupledMCMC.snap(N = 150000, y = y.2011, X = X.2011,
  mats = list(D = D, C = C),
  params = params.init,
  priors = list(g.sd = 10000, xi = 0.01, s2 = 0.001, beta = 10, alpha.sd = 1000),
  update = 50, iter = 1000)

# save results
saveRDS(mcmc.res, file = "./output/snap1-usa2011-mcmc1.RDS")




