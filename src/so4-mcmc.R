
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
library(Matrix)
library(maps)
library(mvnfast)
library(raster)
library(TruncatedNormal)

### read in source code ###
source(here::here("src", "functions.R"))

### load data ###
usa <- readRDS(here::here("data", "central-usa-data.RDS"))

################################################################################
###### 2. Averaged process, coupled model, with wind (BEST MODEL)
################################################################################

# make subdirectory for MCMC results
dir.create(here::here("output", "main-models"), showWarnings = FALSE)

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

# generate MCMC samples !!! CAUTION: THIS MAY TAKE ~24 HOURS !!!
mcmc.res <- coupledMCMC.Wind(
  N = 150000, y = y.2011, X = X.2011,
  mats = list(D = D, C = C),
  params = params.init,
  priors = list(g.sd = 10000, xi = 0.01, s2 = 0.001, beta = 10, alpha.sd = 1000),
  update = 50, iter = 1000)

# save results
saveRDS(mcmc.res, file = here::here("output", "main-models", "avg2-usa2011-mcmc1.RDS"))


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

# generate MCMC samples !!! CAUTION: THIS MAY TAKE ~9 HOURS !!!
mcmc.res <- uncoupledMCMC.W(N = 150000, y = y.2011, X = X.2011,
  mats = list(D = D, C = C),
  params = params.init,
  priors = list(g.sd = 10000, xi = 0.01, s2 = 0.001, beta = 10, alpha.sd = 1000),
  update = 50, iter = 1000)

# save results
saveRDS(mcmc.res, file = here::here("output", "main-models", "avg1-usa2011-mcmc1.RDS"))


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

# generate MCMC samples !!! CAUTION: THIS MAY TAKE ~15 HOURS !!!
mcmc.res <- coupledMCMC.snap(N = 150000, y = y.2011, X = X.2011,
                             mats = list(D = D, C = C),
                             params = params.init,
                             priors = list(g.sd = 10000, xi = 0.01, s2 = 0.001, beta = 10, alpha.sd = 1000),
                             update = 50, iter = 1000)

# save results
saveRDS(mcmc.res, file = here::here("output", "main-models", "snap2-usa2011-mcmc1.RDS"))


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

# generate MCMC samples  !!! CAUTION: THIS MAY TAKE ~9 HOURS !!!
mcmc.res <- uncoupledMCMC.snap(N = 150000, y = y.2011, X = X.2011,
  mats = list(D = D, C = C),
  params = params.init,
  priors = list(g.sd = 10000, xi = 0.01, s2 = 0.001, beta = 10, alpha.sd = 1000),
  update = 50, iter = 1000)

# save results
saveRDS(mcmc.res, file = here::here("output", "main-models", "snap1-usa2011-mcmc1.RDS"))


################################################################################
###### 6. Phenomenological model
################################################################################

# create subdirectory for MCMC results
dir.create(here::here("output", "phenom"), showWarnings = FALSE)

### SAR model:
set.seed(290)
theta.sar <- c(0.5, 10, 1, 0, 0, 0.1, 0.9)
# generate MCMC samples !!! CAUTION: THIS MAY TAKE ~54 HOURS !!!
sar.mcmc <- phenom.mcmc(usa, n.mcmc = 150000, theta.0 = theta.sar, cov.type = "SAR", update = 100, print.iter = 1000)
# save mcmc results
saveRDS(sar.mcmc, file = here::here("output", "phenom", "phenom-sar.RDS"))


################################################################################
###### 7. Colored (Matern) noise
################################################################################

# ### This code replicates the anlysis with coupled model with Matern driving
# ###   noise (see appendix).

# # create subdirectory for MCMC results
# dir.create(here::here("output", "matern"), showWarnings = FALSE)

# # create D matrix
# dims <- dim(usa$so4$so4.2011)
# inds.d <- FVMInds(dims, "insulated")
# D <- diffFVM(dims, inds.d, "insulated")

# # create diffusion operator for Matern covariance
# #   ie, Sigma = ((L + kappa^2 I)'(L + kappa^2 I)^-1 (Matern GRF, nu = 1)
# L <- diffFVM(dims, inds.d, "Neumann")

# # create C matrix
# inds.w <- FVMInds(dims, "periodic")
# C <- createC(big = usa$wind.big$wind.2011, small = usa$wind.small$wind.2011,
#              dims, inds = inds.w)

# # X and y values
# y.2011 <- values(usa$so4$so4.2011)
# X.2011 <- usa$X$X.2011

# # initial parameter values
# params.0 <- list(
#   gamma = 1000,
#   xi = 0.7,
#   beta = 55,
#   alpha = 10,
#   sigma = 100,
#   rho = 0.5,
#   delta = 50
# )

# # hyperparameter values
# priors.0 <- list(
#   g.sd = 500,
#   xi = 0.1,
#   alpha.sd = 100,
#   # penalized complexity prior hyperparameters:
#   pc = list(alpha1 = 0.05, alpha2 = 0.05, rho0 = 5, sigma0 = 0.9),
#   beta = 100
# )

# # generate MCMC samples !!! CAUTION: THIS MAY TAKE ~50 HOURS !!!
# matern.mcmc <- coupledMCMC.Matern.W(
#   N = 250000, y = y.2011, X = X.2011, mats = list(D = D, L = L, C = C),
#   params = params.0, priors = priors.0, update = 100, iter = 1000
# )

# # save MCMC samples
# saveRDS(matern.mcmc, file = here::here("output", "matern", "matern-250k.RDS"))


################################################################################
###### 8. Grid sensitivity analysis
################################################################################

# ### This code replicates the grid sensitivity analysis in the supplementary
# ###   materials. This code should be repeated for all values of 'num.ag';
# ###   it is suggested that it be run in parallel, if possible.

# # aggregation size
# num.ag <- c(10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 36, 40, 44, 48, 52, 56, 60, 64)

# # create a new folder for grid data
# dir.create(here::here("data", "grid"), showWarnings = FALSE)

# ### A. Create covariate and response data for appropriate grid-size:
# for (k in 1:length(num.ag)) {

#   ### SO4 DATA ###

#   ### Read in SO4 data; files from Randall Martin's group available here:
#   ###    http://fizz.phys.dal.ca/~atmos/martin/?page_id=140

#   # 2011 SO4 data
#   file.in <- here("data", "GWRwSPEC_SO4_NA_201101_201112.nc")
#   so4.11 <- raster(file.in)

#   # combine SO4 data
#   so4 <- stack(so4.11)
#   my.extent <- c(-100, -81.5, 30.5, 41.7)

#   ### Create two spatial extents, one slightly larger than the other.
#   ###   This allows us to calculate wind velocities on the edge of the
#   ###   smaller spatial extent.

#   # create a bigger spatial extent, for wind matrix calculation...
#   if (k < 16){
#     big.extent <- c(-100.25, -81, 30, 42)
#   } else {
#     big.extent <- c(-101, -80, 29, 43)
#   }
#   so4.big <- raster()
#   extent(so4.big) <- extent(big.extent)
#   res(so4.big) <- res(so4)
#   so4big <- crop(so4, so4.big)

#   # create a smaller spatial extent, for the analysis
#   so4.c <- raster()
#   extent(so4.c) <- extent(my.extent)
#   res(so4.c) <- res(so4)
#   so4c <- crop(so4, so4.c)

#   so4.norm <- aggregate(so4c, num.ag[k])
#   so4.big <- aggregate(so4big, num.ag[k])

#   ### WIND DATA ###
#   ### grab 2011 wind data
#   wind.big <- list()
#   wind.norm <- list()

#   j = 1

#   uwind.raster <- yearRaster(j + 2010, met = "u-wind")
#   vwind.raster <- yearRaster(j + 2010, met = "v-wind")

#   uwind.new <- trimToSO4(uwind.raster, so4.norm)
#   vwind.new <- trimToSO4(vwind.raster, so4.norm)

#   uwind.big <- trimToSO4(uwind.raster, so4.big)
#   vwind.big <- trimToSO4(vwind.raster, so4.big)

#   big <- stack(uwind.big, vwind.big)
#   names(big) <- c("uwind", "vwind")

#   normal <- stack(uwind.new, vwind.new)
#   names(normal) <- c("uwind", "vwind")

#   wind.big[[j]] <- big
#   wind.norm[[j]] <- normal

#   names(wind.big) <- c("wind.2011")
#   names(wind.norm) <- c("wind.2011")

#   ### POWER PLANT EMISSIONS DATA ###

#   # store monthly data in a list
#   month <- list()
#   month$fac <- readRDS(here::here("data", "MonthlyFacilityData.RDS"))
#   month$unit <- readRDS(here::here("data", "MonthlyUnitData.RDS"))

#   # clean monthly unit data
#   month$unit <- cleanData(month$unit)

#   # store annual data in a list
#   annual <- list()
#   annual$fac <- readRDS(here::here("data", "AnnualFacilityData.RDS"))
#   annual$unit <- readRDS(here::here("data", "AnnualUnitData.RDS"))

#   # rename columns
#   colnames(month$unit) <- unitNames()
#   colnames(annual$unit) <- unitNames(TRUE)

#   ### trim emissions data by year
#   month.2011 <- trimYear(month, 2011)
#   annual.2011 <- trimYear(annual, 2011)

#   ### trim emissions data by spatial extent
#   emissions.2011 <- trimData(annual.2011, my.extent)

#   # save yearly emissions data in a list
#   emissions <- list(em.2011 = emissions.2011)

#   ### convert emissions data to X vector (design vector used in model)
#   X.2011 <- createSimpleX(so4.norm, emissions.2011$fac)

#   # create D matrix
#   dims <- dim(so4.norm)
#   inds.d <- FVMInds(dims, "insulated")
#   D <- diffFVM(dims, inds.d, "insulated")

#   # create diffusion operator for Matern covariance
#   #   ie, Sigma = ((L + kappa^2 I)'(L + kappa^2 I)^-1 (Matern GRF, nu = 1)
#   L <- diffFVM(dims, inds.d, "Neumann")

#   # create C matrix
#   inds.w <- FVMInds(dims, "periodic")
#   C <- createC(
#     big = wind.big$wind.2011, small = wind.norm$wind.2011,
#     dims, inds = inds.w
#   )

#   y.2011 <- values(so4.norm)

#   ### SAVE DATA ###
#   save(y.2011, X.2011, D, C, L, file = here::here("data", "grid", paste("grid-ag-", num.ag[k], ".RData", sep = "")))
# }


# ### B. Repeat the main analysis, for each grid size: !!! CAUTION: THIS MAY TAKE ~195 HOURS !!!
# set.seed(4533)

# # create a new folder for grid output
# dir.create(here::here("output", "grid"), showWarnings = FALSE)

# for (k in 1:length(num.ag)) {

#   # read in appropriate data
#   load(file = here::here("data", "grid", paste("grid-ag-", num.ag[k], ".RData", sep = "")))

#   # initialize parameters
#   gamma.init <- 1000
#   xi.init <- 0.5
#   beta.init <- 4.5
#   alpha.init <- 10
#   s2.init <- 100
#   delta.init <- 50

#   params.init <- list(
#     gamma = gamma.init,
#     xi = xi.init,
#     beta = beta.init,
#     s2 = s2.init,
#     delta = delta.init,
#     alpha = alpha.init
#   )

#   # generate MCMC samples !!! CAUTION: THIS MAY TAKE ~48 HOURS !!!
#   mcmc.res <- coupledMCMC.Wind(
#     N = 100000, y = y.2011, X = X.2011,
#     mats = list(D = D, C = C),
#     params = params.init,
#     priors = list(g.sd = 10000, xi = 0.01, s2 = 0.001, beta = 10, alpha.sd = 1000),
#     update = 50, iter = 1000
#   )

#   saveRDS(mcmc.res, file = here::here("output", "grid", paste("grid-res-", num.ag[k], ".RDS", sep = "")))
# }


