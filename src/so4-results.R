
### so4-results.R
### author: Nathan Wikle
###
### Process MCMC results (from so4-analysis.R), creating figures
###   found in the SO4 manuscript.

# required packages
library(dplyr)
library(forcats)
library(geosphere)
library(ggplot2)
library(ggridges)
library(hrbrthemes)
library(maps)
library(matrixStats)
library(mvnfast)
library(raster)
library(rwc)
library(sp)
library(viridis)

# make subdirectory for manuscript figures
dir.create(here::here("output", "ms-figures"), showWarnings = FALSE)

##################################
### 1. Load functions and data ###
##################################

# source functions
source(here::here("src", "functions.R"))

# load raster data (created with 'data_cleaning.R')
usa <- readRDS(here::here("data", "central-usa-data.RDS"))

# load MCMC results
avg2.mcmc1 <- readRDS(here::here("output", "main-models", "avg2-usa2011-mcmc1.RDS"))
avg1.mcmc1 <- readRDS(here::here("output", "main-models", "avg1-usa2011-mcmc1.RDS"))
snap2.mcmc1 <- readRDS(here::here("output", "main-models", "snap2-usa2011-mcmc1.RDS")) 
snap1.mcmc1 <- readRDS(here::here("output", "main-models", "snap1-usa2011-mcmc1.RDS")) 

######################################
### 2. Plot SO4 and emissions data ###
######################################

### Plot of 2011 Emissions Data (FIGURE 1a) ###
png(
  file = here::here("output", "ms-figures", "fig1a.png"),
  width = 7, h = 5.75, units = "in", res = 300, bg = "transparent"
)

par(
  xpd = FALSE,
  mar = c(2.5, 1, 2.5, 1),
  oma = c(0, 0, 0, 0)
)

plot(usa$so4$so4.2011,
  legend = FALSE, axes = FALSE,
  breaks = breakpoint.creation(usa$so4$so4.2011,
    n = 255,
    min.val = 0.93,
    max.val = 3.55
  ),
  col = rev(terrain.colors(255)),
  main = "", cex.main = 1.5,
  box = TRUE
)

map("state", add = TRUE)

plot(usa$so4$so4.2011,
  legend.only = TRUE,
  legend.width = 0.75, legend.shrink = 0.55,
  breaks = breakpoint.creation(usa$so4$so4.2011,
    n = 255,
    min.val = 0.93,
    max.val = 3.55
  ),
  col = rev(terrain.colors(255)),
  axis.args = list(
    at = c(1, 1.5, 2, 2.5, 3, 3.5),
    labels = c(1, 1.5, 2, 2.5, 3, 3.5),
    cex.axis = 0.75
  ),
  legend.args =
    list(
      text = expression(paste(S * O[4], "  (", mu * g / m^3, ")", sep = "")),
      side = 4, font = 2, line = 2.75, cex = 1
    )
)

points(
  usa$em$em.2011$fac$Fac.Longitude,
  usa$em$em.2011$fac$Fac.Latitude,
  pch = 16, cex = 0.00002 * usa$em$em.2011$fac$totSO2emissions * 12
)

par(xpd = TRUE)
legend("topleft",
  title = expression(paste("2011 ",
    S * O[2], " Emissions",
    sep = ""
  )),
  legend = c("50k tons", "100k tons"),
  bty = "o", pch = c(16, 16),
  pt.cex = c(50000 * 0.00002, 100000 * 0.00002),
  box.col = "black", bg = "white"
)
dev.off()


### Plot of 2011 average wind field (FIGURE 1b) ###

# coordinate system
wind <- usa$wind.small$wind.2011
xy <- coordinates(wind$uwind)

# u and v wind values
u.w.vals <- values(wind$uwind)
v.w.vals <- values(wind$vwind)

# create a raster for plotting
r <- wind$uwind

n.row <- dim(wind$uwind)[1]
n.col <- dim(wind$uwind)[2]

row.spacing <- (1:(floor(n.row / 4) - 1) * 4) - 1
arrow.pts <- sapply(row.spacing, function(x) {
  (n.col * x) + (1:floor(n.col / 4) * 4)
})

final.x <- seq(from = 7888, to = 8000, by = 4)
arrow.pts <- cbind(arrow.pts, final.x)

# plot the raster with wind vectors
png(
  file = here::here("output", "ms-figures", "fig1b.png"),
  width = 7, h = 5.75, units = "in", res = 300, bg = "transparent"
)

par(
  xpd = FALSE,
  mar = c(2.5, 1, 2.5, 1),
  oma = c(0, 0, 0, 0)
)

plot(usa$so4$so4.2011,
  legend = FALSE, axes = FALSE,
  breaks = breakpoint.creation(usa$so4$so4.2011,
    n = 255,
    min.val = 0.93,
    max.val = 3.55
  ),
  col = rev(terrain.colors(255)),
  main = "", cex.main = 1.5,
  box = TRUE
)

map("state", add = TRUE)

arrows(
  x0 = xy[arrow.pts, 1], y0 = xy[arrow.pts, 2],
  x1 = xy[arrow.pts, 1] + 0.3 * u.w.vals[arrow.pts],
  y1 = xy[arrow.pts, 2] + 0.3 * v.w.vals[arrow.pts],
  length = 0.03, col = "black"
)

plot(usa$so4$so4.2011,
  legend.only = TRUE,
  legend.width = 0.75, legend.shrink = 0.55,
  breaks = breakpoint.creation(usa$so4$so4.2011,
    n = 255,
    min.val = 0.93,
    max.val = 3.55
  ),
  col = rev(terrain.colors(255)),
  axis.args = list(
    at = c(1, 1.5, 2, 2.5, 3, 3.5),
    labels = c(1, 1.5, 2, 2.5, 3, 3.5),
    cex.axis = 0.75
  ),
  legend.args =
    list(
      text = expression(paste(S * O[4], "  (", mu * g / m^3, ")", sep = "")),
      side = 4, font = 2, line = 2.75, cex = 1
    )
)

dev.off()

############################
### 3. Inference Results ###
############################

### Posterior mean estimates and cred. ints. of model parameters (TABLE 1)

burnin <- 1:25000

# calculate credible intervals
avg2.ci <- t(apply(
  avg2.mcmc1$samples[-burnin, ], 2,
  function(x) {
    quantile(x, probs = c(0.025, 0.975))
  }
))

# create table one - note that order of params is slightly different than
#   manuscript order.
table1 <- cbind(colMeans(avg2.mcmc1$samples[-burnin, ]), avg2.ci)
colnames(table1) <- c("mean", "2.5%", "97.5%")
rownames(table1) <- c("gamma", "eta", "beta", "s2", "delta", "alpha")

# table 1 (should be very similar, but not identical, to those found in paper)
round(table1, 3)


### Calculate DIC for each model

# set-up data structures

# create D matrix
dims <- dim(usa$so4$so4.2011)
inds.d <- FVMInds(dims, "insulated")
D <- diffFVM(dims, inds.d, "insulated")

# create C matrix
inds.w <- FVMInds(dims, "periodic")
C <- createC(
  big = usa$wind.big$wind.2011, small = usa$wind.small$wind.2011,
  dims, inds = inds.w
)

# X and y values
y.2011 <- values(usa$so4$so4.2011)
X.2011 <- usa$X$X.2011

# DIC for avg2
dic.avg2 <- dic.calc(
  y = y.2011, X = X.2011,
  burnin = 1:25000,
  samples = avg2.mcmc1$samples,
  dic.vec = avg2.mcmc1$dic,
  mats = list(D = D, C = C),
  model = "avg2"
)

# DIC for avg1
dic.avg1 <- dic.calc(
  y = y.2011, X = X.2011,
  burnin = 1:25000,
  samples = avg1.mcmc1$samples,
  dic.vec = avg1.mcmc1$dic,
  mats = list(D = D, C = C),
  model = "avg1"
)

# DIC for snap2
dic.snap2 <- dic.calc(
  y = y.2011, X = X.2011,
  burnin = 1:25000,
  samples = snap2.mcmc1$samples,
  dic.vec = snap2.mcmc1$dic,
  mats = list(D = D, C = C),
  model = "snap2"
)

# DIC for snap1
dic.snap1 <- dic.calc(
  y = y.2011, X = X.2011,
  burnin = 1:25000,
  samples = snap1.mcmc1$samples,
  dic.vec = snap1.mcmc1$dic,
  mats = list(D = D, C = C),
  model = "snap1"
)


dic.scores <- c(dic.avg2, dic.avg1, dic.snap2, dic.snap1)
names(dic.scores) <- c("Avg2", "Avg1", "Snap2", "Snap1")

# scores found in paper
#   (note: only avg2, avg1, and snap2 were included in the paper)
dic.scores

which(min(dic.scores) == dic.scores) # Avg2 is best based on DIC.

### Empirical estimate of marginal variance of covariance, for best fit model.

# initialize parameters (as found in Table 1)
gamma.hat <- table1[1, 1]
alpha.hat <- table1[6, 1]
delta.hat <- table1[5, 1]
s2.hat <- table1[4, 1]

# A matrix
A <- ((gamma.hat * D) + (alpha.hat * C) + Diagonal(nrow(D), delta.hat))

# A'A
AprimeA <- t(A) %*% A

# Q = Sigma^-1 (see Equation 24)
Q <- AprimeA / s2.hat
mu <- rep(0, nrow(Q))

# simulate 1000 samples from multivariate normal with Sigma given by Q^-1.
samples <- matrix(data = NA_real_, nrow = nrow(Q), ncol = 1000)
set.seed(300)
for (k in 1:1000) {
  samples[, k] <- as.vector(rnorm.Q(Q, mu, zero.constraint = FALSE, canon = FALSE))
}

# estimate Sigma using 1000 samples
test.cov <- cov(t(samples))

# determine the min and max marginal variance parameters (found on page 20).
min.diag <- min(diag(test.cov)) # ~0.02
max.diag <- max(diag(test.cov)) # ~0.1

min.diag
max.diag

### Mean SO4 concentration using best model (FIGURE 2a) ###

# parameter estimates (posterior mean)
theta.hat <- colMeans(avg2.mcmc1$samples[-burnin, ])

# expected SO4 surface (V.hat, equation 25 in paper)
mean1 <- fittedMeanSO4Surface(theta.hat, mats = list(D = D, C = C), X = X.2011)

# plot raster with fitted mean surface
plot.avg2.t1 <- usa$so4$so4.2011
values(plot.avg2.t1) <- mean1

# expected annual SO4, given coupled, time-averaged model fit
png(
  file = here::here("output", "ms-figures", "fig2a.png"),
  width = 7, h = 5.75, units = "in", res = 300, bg = "transparent"
)

par(
  xpd = FALSE,
  mar = c(2.5, 1, 2.5, 1),
  oma = c(0, 0, 0, 0)
)

plot(plot.avg2.t1,
  legend = FALSE, axes = FALSE,
  breaks = breakpoint.creation(usa$so4$so4.2011,
    n = 255,
    min.val = 0.93,
    max.val = 3.55
  ),
  col = rev(terrain.colors(255)),
  main = "", cex.main = 1.5,
  box = TRUE
)

map("state", add = TRUE)

plot(plot.avg2.t1,
  legend.only = TRUE,
  legend.width = 0.75, legend.shrink = 0.55,
  breaks = breakpoint.creation(usa$so4$so4.2011,
    n = 255,
    min.val = 0.93,
    max.val = 3.55
  ),
  col = rev(terrain.colors(255)),
  axis.args = list(
    at = c(1, 1.5, 2, 2.5, 3, 3.5),
    labels = c(1, 1.5, 2, 2.5, 3, 3.5),
    cex.axis = 0.75
  ),
  legend.args =
    list(
      text = expression(paste(S * O[4], "  (", mu * g / m^3, ")", sep = "")),
      side = 4, font = 2, line = 2.75, cex = 1
    )
)

dev.off()


### Uncertainty quantification of mean SO4 concentration (FIGURE 2b) ###

set.seed(1994)

# number of samples used in plot
n.mean.samples <- 1000
sample.inds <- sample.int(
  n = nrow(avg2.mcmc1$samples[-burnin, ]),
  size = n.mean.samples
) + max(burnin)

mean.sample <- apply(
  X = avg2.mcmc1$samples[sample.inds, ], MARGIN = 1,
  FUN = fittedMeanSO4Surface, mats = list(D = D, C = C), X.2011
)

# determine standard deviations:
mean.sd <- apply(mean.sample, 1, sd)

# credible intervals:
mean.lower.ci <- apply(mean.sample, 1, quantile, prob = 0.025)
mean.upper.ci <- apply(mean.sample, 1, quantile, prob = 0.975)

### i) plot raster with fitted mean surface
plot.avg2.sd <- usa$so4$so4.2011
values(plot.avg2.sd) <- mean.sd

# expected annual SO4, given coupled, time-averaged model fit
png(
  file = here::here("output", "ms-figures", "fig2b.png"),
  width = 7, h = 5.75, units = "in", res = 300, bg = "transparent"
)

par(
  xpd = FALSE,
  mar = c(2.5, 1, 2.5, 1),
  oma = c(0, 0, 0, 0)
)

plot(plot.avg2.sd,
  legend = FALSE, axes = FALSE,
  main = "", cex.main = 1.5,
  box = TRUE
)

map("state", add = TRUE)

plot(plot.avg2.sd,
  legend.only = TRUE,
  legend.width = 0.75, legend.shrink = 0.55,
  axis.args = list(cex.axis = 0.75),
  legend.args =
    list(
      text = expression(paste(S * O[4], "  (", mu * g / m^3, ")", sep = "")),
      side = 4, font = 2, line = 2.75, cex = 1
    )
)

dev.off()


### 2011 Emissions Data with Labels (FIGURE 2c) ###
png(
  file = here::here("output", "ms-figures", "fig2c.png"), 
  width = 7, h = 5.75, units = "in", res = 300, bg = "transparent"
)

par(
  xpd = FALSE,
  mar = c(2.5, 1, 2.5, 1),
  oma = c(0, 0, 0, 0)
)

plot(usa$so4$so4.2011,
  legend = FALSE, axes = FALSE,
  breaks = breakpoint.creation(usa$so4$so4.2011,
    n = 255,
    min.val = 0.93,
    max.val = 3.55
  ),
  col = rev(terrain.colors(255)),
  main = "", cex.main = 1.5,
  box = TRUE
)

map("state", add = TRUE)

plot(usa$so4$so4.2011,
  legend.only = TRUE,
  legend.width = 0.75, legend.shrink = 0.55,
  breaks = breakpoint.creation(usa$so4$so4.2011,
    n = 255,
    min.val = 0.93,
    max.val = 3.55
  ),
  col = rev(terrain.colors(255)),
  axis.args = list(
    at = c(1, 1.5, 2, 2.5, 3, 3.5),
    labels = c(1, 1.5, 2, 2.5, 3, 3.5),
    cex.axis = 0.75
  ),
  legend.args =
    list(
      text = expression(paste(S * O[4], "  (", mu * g / m^3, ")", sep = "")),
      side = 4, font = 2, line = 2.75, cex = 1
    )
)

graphics::text(
  x = -88.2, y = 31.5,
  labels = c("Gulf Coast"), cex = 1.25,
  col = "blue3", font = 2
)
graphics::text(
  x = -92, y = 35,
  labels = c("Mississippi"), cex = 1.25,
  col = "blue3", font = 2
)
graphics::text(
  x = -92, y = 34.25,
  labels = c("River Valley"), cex = 1.25,
  col = "blue3", font = 2
)
graphics::text(
  x = -84.5, y = 40.1,
  labels = c("Ohio River Valley"), cex = 1.25,
  col = "blue3", font = 2
)

points(
  usa$em$em.2011$fac$Fac.Longitude,
  usa$em$em.2011$fac$Fac.Latitude,
  pch = 16, cex = 0.00002 * usa$em$em.2011$fac$totSO2emissions * 12
)

par(xpd = TRUE)
legend("topleft",
  title = expression(paste("2011 ",
    S * O[2], " Emissions",
    sep = ""
  )),
  legend = c("50k tons", "100k tons"),
  bty = "o", pch = c(16, 16),
  pt.cex = c(50000 * 0.00002, 100000 * 0.00002),
  box.col = "black", bg = "white"
)
dev.off()


### Simulated SO4 surface from the fitted coupled, time-averaged model (FIGURE 3a) ###

set.seed(833)

sim1 <- simCoupled.Wind(
  theta = colMeans(avg2.mcmc1$samples[-burnin, ]),
  mats = list(D = D, C = C), X = X.2011
)
plot.avg2.sim1 <- usa$so4$so4.2011
values(plot.avg2.sim1) <- sim1

png(
  file = here::here("output", "ms-figures", "fig3a.png"),
  width = 7, h = 5.75, units = "in", res = 300, bg = "transparent"
)

par(
  xpd = FALSE,
  mar = c(2.5, 1, 2.5, 1),
  oma = c(0, 0, 0, 0)
)

plot(plot.avg2.sim1,
  legend = FALSE, axes = FALSE,
  breaks = breakpoint.creation(usa$so4$so4.2011,
    n = 255,
    min.val = 0.93,
    max.val = 3.55
  ),
  col = rev(terrain.colors(255)),
  main = "", cex.main = 1.5,
  box = TRUE
)
map("state", add = TRUE)
plot(plot.avg2.sim1,
  legend.only = TRUE,
  legend.width = 0.75, legend.shrink = 0.55,
  breaks = breakpoint.creation(usa$so4$so4.2011,
    n = 255,
    min.val = 0.93,
    max.val = 3.55
  ),
  col = rev(terrain.colors(255)),
  axis.args = list(
    at = c(1, 1.5, 2, 2.5, 3, 3.5),
    labels = c(1, 1.5, 2, 2.5, 3, 3.5),
    cex.axis = 0.75
  ),
  legend.args =
    list(
      text = expression(paste(S * O[4], "  (", mu * g / m^3, ")", sep = "")),
      side = 4, font = 2, line = 2.75, cex = 1
    )
)

dev.off()

### Simulated SO4 surface from fitted uncoupled, time-averaged model (FIGURE 3b) ###

# set up new plotting scale
new.cols <- c(rev(colfunc2(181))[-181], colfunc1(12))
new.breaks <- seq(from = -0.9194118, to = 1.05329412, by = 0.01027451)

# simulate from likelihood model
set.seed(328311)

avg1.sim1 <- simUncoupled.W(colMeans(avg1.mcmc1$samples[-burnin, ]),
  mats = list(D = D, C = C), X = X.2011
)
p.avg1.sim <- usa$so4$so4.2011
values(p.avg1.sim) <- avg1.sim1

png(
  file = here::here("output", "ms-figures", "fig3b.png"),
  width = 7, h = 5.75, units = "in", res = 300, bg = "transparent"
)

par(
  xpd = FALSE,
  mar = c(2.5, 1, 2.5, 1),
  oma = c(0, 0, 0, 0)
)

plot(p.avg1.sim,
  legend = FALSE, axes = FALSE,
  breaks = new.breaks,
  col = new.cols,
  main = "", cex.main = 1.5,
  box = TRUE
)

map("state", add = TRUE)

plot(p.avg1.sim,
  legend.only = TRUE,
  legend.width = 0.75, legend.shrink = 0.55,
  breaks = new.breaks,
  col = new.cols,
  axis.args = list(
    at = c(-0.5, 0, 0.5, 1.0),
    labels = c(-0.5, 0, 0.5, 1.0),
    cex.axis = 0.75
  ),
  legend.args =
    list(
      text = expression(paste(S * O[4], "  (", mu * g / m^3, ")", sep = "")),
      side = 4, font = 2, line = 2.75, cex = 1
    )
)

dev.off()

### Simulated SO4 surface from fitted coupled, snapshot model (FIGURE 3c) ###

set.seed(9345)

snap2.sim1 <- simCoupled.snap(colMeans(snap2.mcmc1$samples[-burnin, ]),
  mats = list(D = D, C = C),
  X = X.2011
)
p.snap2.sim <- usa$so4$so4.2011
values(p.snap2.sim) <- snap2.sim1

png(
  file = here::here("output", "ms-figures", "fig3c.png"),
  width = 7, h = 5.75, units = "in", res = 300, bg = "transparent"
)

par(
  xpd = FALSE,
  mar = c(2.5, 1, 2.5, 1),
  oma = c(0, 0, 0, 0)
)

plot(p.snap2.sim,
  legend = FALSE, axes = FALSE,
  breaks = breakpoint.creation(usa$so4$so4.2011,
    n = 255,
    min.val = 0.93,
    max.val = 3.55
  ),
  col = rev(terrain.colors(255)),
  main = "", cex.main = 1.5,
  box = TRUE
)
map("state", add = TRUE)
plot(p.snap2.sim,
  legend.only = TRUE,
  legend.width = 0.75, legend.shrink = 0.55,
  breaks = breakpoint.creation(usa$so4$so4.2011,
    n = 255,
    min.val = 0.93,
    max.val = 3.55
  ),
  col = rev(terrain.colors(255)),
  axis.args = list(
    at = c(1, 1.5, 2, 2.5, 3, 3.5),
    labels = c(1, 1.5, 2, 2.5, 3, 3.5),
    cex.axis = 0.75
  ),
  legend.args =
    list(
      text = expression(paste(S * O[4], "  (", mu * g / m^3, ")", sep = "")),
      side = 4, font = 2, line = 2.75, cex = 1
    )
)

dev.off()

### Simulated SO4 surface from phenomenological model (Figure 3d) ###

set.seed(15)

sar.mcmc <- readRDS(here::here("output", "phenom", "sar-mcmc1.RDS"))

phenom.dic(usa, mcmc.res = sar.mcmc, burnin = 50000, cov.type = "SAR", mean.vrsn = 2)

# simulate sar plot
sar.sim <- plotPhenom(usa,
  theta = colMeans(sar.mcmc$samples[-c(1:50000), ]),
  cov.type = "SAR", mean.vrsn = 2, plot.type = "prec"
)

# mean sar plot
sar.mean <- plotPhenom(usa,
  theta = colMeans(sar.mcmc$samples[-c(1:50000), ]),
  cov.type = "SAR", mean.vrsn = 2, plot.type = "mean"
)

png(
  file = here::here("output", "ms-figures", "fig3d.png"),
  width = 7, h = 5.75, units = "in", res = 300, bg = "transparent"
)

par(
  xpd = FALSE,
  mar = c(2.5, 1, 2.5, 1),
  oma = c(0, 0, 0, 0)
)

plot(sar.sim,
  legend = FALSE, axes = FALSE,
  breaks = breakpoint.creation(usa$so4$so4.2011,
    n = 255,
    min.val = 0.93,
    max.val = 3.55
  ),
  col = rev(terrain.colors(255)),
  main = "", cex.main = 1.5,
  box = TRUE
)
map("state", add = TRUE)

plot(sar.sim,
  legend.only = TRUE,
  legend.width = 0.75, legend.shrink = 0.55,
  breaks = breakpoint.creation(usa$so4$so4.2011,
    n = 255,
    min.val = 0.93,
    max.val = 3.55
  ),
  col = rev(terrain.colors(255)),
  axis.args = list(
    at = c(1, 1.5, 2, 2.5, 3, 3.5),
    labels = c(1, 1.5, 2, 2.5, 3, 3.5),
    cex.axis = 0.75
  ),
  legend.args =
    list(
      text = expression(paste(S * O[4], "  (", mu * g / m^3, ")", sep = "")),
      side = 4, font = 2, line = 2.75, cex = 1
    )
)

dev.off()


png(
  file = here::here("output", "ms-figures", "fig-mean_sar2.png"),
  width = 7, h = 5.75, units = "in", res = 300, bg = "transparent"
)

par(
  xpd = FALSE,
  mar = c(2.5, 1, 2.5, 1),
  oma = c(0, 0, 0, 0)
)

plot(sar.mean,
  legend = FALSE, axes = FALSE,
  breaks = breakpoint.creation(usa$so4$so4.2011,
    n = 255,
    min.val = 0.93,
    max.val = 3.55
  ),
  col = rev(terrain.colors(255)),
  main = "", cex.main = 1.5,
  box = TRUE
)
map("state", add = TRUE)

points(usa$em$em.2011$fac$Fac.Longitude,
  usa$em$em.2011$fac$Fac.Latitude,
  pch = 16, cex = 0.00002 * usa$em$em.2011$fac$totSO2emissions * 12
)

plot(sar.mean,
  legend.only = TRUE,
  legend.width = 0.75, legend.shrink = 0.55,
  breaks = breakpoint.creation(usa$so4$so4.2011,
    n = 255,
    min.val = 0.93,
    max.val = 3.55
  ),
  col = rev(terrain.colors(255)),
  axis.args = list(
    at = c(1, 1.5, 2, 2.5, 3, 3.5),
    labels = c(1, 1.5, 2, 2.5, 3, 3.5),
    cex.axis = 0.75
  ),
  legend.args =
    list(
      text = expression(paste(S * O[4], "  (", mu * g / m^3, ")", sep = "")),
      side = 4, font = 2, line = 2.75, cex = 1
    )
)

dev.off()


###############################################################################
######  Air pollution reduction: a quick glance (Section 4.2)
################################################################################

### identify top ten emitting power plants:

# identify top five facilities
fac.2011 <- usa$em$em.2011$fac
n.fac <- nrow(fac.2011)
top.five <- order(fac.2011$totSO2emissions)[n.fac:(n.fac - 4)]

# top five facilities (all without scrubbers)
fac.2011[top.five, ]

cell.locs <- cellFromXY(usa$so4$so4.2011, as.matrix(fac.2011[top.five, 20:19]))

# create a new X after reducing SO2 emissions from top five facilities by 80% (ie, adding scrubber)
X.orig <- createSimpleX(usa$so4$so4.2011, usa$em$em.2011$fac)

# emissions values
X.new <- matrix(X.orig, nrow = length(X.orig), ncol = 5, byrow = FALSE)
for (j in 1:5) {
  new.emissions.fac <- usa$em$em.2011$fac
  new.emissions.fac[top.five[j], 12] <- fac.2011[top.five[j], 12] * 0.2
  X.new[, j] <- createSimpleX(usa$so4$so4.2011, new.emissions.fac)
}

# simulate from posterior model, calculate normalized exposure (equations 26,27)
# !!! CAUTION: THIS TAKES ~5 HOURS !!!
set.seed(99)
my.exp <- expCalc(
  N = 2000, X.orig, X.new, samples = avg2.mcmc1$samples,
  burnin = 25000, mats.exp = list(D = D, C = C), pop.r = usa$pop
)

diff.exp <- my.exp[, 1] - my.exp[, -1]

# identify the facilities emitting the 5 largest amounts of SO2
fac.top5 <- fac.2011[top.five, ]


### Plot emissions from 5 largest plants (FIGURE 4a) ###

png(
  file = here::here("output", "ms-figures", "fig4a.png"),
  width = 7, h = 5.75, units = "in", res = 300, bg = "transparent"
)

par(
  xpd = FALSE,
  mar = c(2.5, 1, 2.5, 1),
  oma = c(0, 0, 0, 0)
)

par(xpd = FALSE)
plot(usa$so4$so4.2011,
  legend = FALSE, axes = FALSE,
  breaks = breakpoint.creation(usa$so4$so4.2011,
    n = 255,
    min.val = 0.93,
    max.val = 3.55
  ),
  col = rev(terrain.colors(255)),
  main = "", cex.main = 1.5,
  box = TRUE
)

map("state", add = TRUE)

plot(usa$so4$so4.2011,
  legend.only = TRUE,
  legend.width = 0.75, legend.shrink = 0.55,
  breaks = breakpoint.creation(usa$so4$so4.2011,
    n = 255,
    min.val = 0.93,
    max.val = 3.55
  ),
  col = rev(terrain.colors(255)),
  axis.args = list(
    at = c(1, 1.5, 2, 2.5, 3, 3.5),
    labels = c(1, 1.5, 2, 2.5, 3, 3.5),
    cex.axis = 0.75
  ),
  legend.args =
    list(
      text = expression(paste(S * O[4], "  (", mu * g / m^3, ")", sep = "")),
      side = 4, font = 2, line = 2.75, cex = 1
    )
)

points(fac.top5$Fac.Longitude,
  fac.top5$Fac.Latitude,
  pch = 16, cex = 0.00002 * fac.top5$totSO2emissions * 12
)

par(xpd = TRUE)
legend("topleft",
  title = expression(paste("2011 ",
    S * O[2], " Emissions",
    sep = ""
  )),
  legend = c("50k tons", "100k tons"),
  bty = "o", pch = c(16, 16),
  pt.cex = c(50000 * 0.00002, 100000 * 0.00002),
  box.col = "black", bg = "white"
)

title("Five Largest Emissions Sources",
  adj = 0, line = 1, cex.main = 1.5, font = 2
)

dev.off()

### Expected decrease in SO4 after scrubber (FIGURE 4b) ###

# choose facility
scrub.fac <- 3 # E C Gaston, Alabama
# scrub.fac <- 5 # Clifty Creek, IN

# reduction in SO2 emissions after scrubber at
avg.diff <- (X.2011[cell.locs[scrub.fac]] * 0.2) - X.2011[cell.locs[scrub.fac]]

X.cc <- rep(0, length = length(X.2011))
X.cc[cell.locs[scrub.fac]] <- avg.diff

mean.reduct <- fittedMeanSO4Surface(
  theta = colMeans(avg2.mcmc1$samples[-burnin, ]),
  mats = list(D = D, C = C), X = X.cc
)

# raster with reduction in SO4 as values
reduction.plot <- usa$so4$so4.2011
values(reduction.plot) <- mean.reduct

png(
  file = here::here("output", "ms-figures", "fig4b.png"),
  width = 7, h = 5.75, units = "in", res = 300, bg = "transparent"
)

par(
  xpd = FALSE,
  mar = c(2.5, 1, 2.5, 1),
  oma = c(0, 0, 0, 0)
)

plot(reduction.plot, legend = FALSE, axes = FALSE, cex.main = 1.5)
map("state", add = TRUE)
plot(reduction.plot,
  legend.only = TRUE,
  legend.width = 0.5, legend.shrink = 0.55,
  axis.args = list(cex.axis = 0.8),
  legend.args =
    list(
      text = expression(paste(S * O[4], "  (", mu * g / m^3, ")", sep = "")),
      side = 4, font = 2, line = 3, cex = 1
    )
)

title("E.C. Gaston Electric Generating Plant",
  adj = 0, line = 1, cex.main = 1.5, font = 2
)

dev.off()

### Probabilistic forecasts of average reduction in human exposure to SO4 (FIGURE 4c) ###

my.data <- data.frame(
  text = as.character(
    c(
      rep("Kyger Creek, OH (143k tons)", nrow(diff.exp)),
      rep("Muskingum River, OH (104k tons)", nrow(diff.exp)),
      rep("E.C. Gaston, AL (92k tons)", nrow(diff.exp)),
      rep("Walter C. Beckjord, OH (91k tons)", nrow(diff.exp)),
      rep("Clifty Creek, IN (74k tons)", nrow(diff.exp))
    )
  ),
  value = as.vector(diff.exp)
)

# create plot with densities of reduction in exposure for each facility.
comp.plot <- my.data %>%
  mutate(text = fct_reorder(text, value)) %>%
  ggplot(aes(y = text, x = value, fill = text)) +
  geom_density_ridges() +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    panel.spacing = unit(1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("Forecasted Reduction in Exposure") +
  ylab("")

# save plot
ggsave(
  filename = here::here("output", "ms-figures", "fig4c.png"),
  plot = comp.plot,
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)
