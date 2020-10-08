
### so4-results.R
### author: Nathan Wikle.
###
### Process MCMC results (from so4-analysis.R), creating figures 
###   found in the SO4 manuscript.

# required packages
library(mvnfast)
library(raster)
library(sp)
library(maps)
library(rwc)
library(matrixStats)
library(ggridges)
library(dplyr)
library(forcats)
library(viridis)
library(hrbrthemes)

##################################
### 1. Load functions and data ###
##################################

# source functions
source("./src/functions.R")

# load raster data (created with 'data_cleaning.R')
usa <- readRDS("./data/central-usa-data.RDS")

# load MCMC results
avg2.mcmc.1 <- readRDS("./output/avg2-usa2011-mcmc1.RDS")
avg1.mcmc.1 <- readRDS("./output/avg1-usa2011-mcmc1.RDS")
snap2.mcmc.1 <- readRDS("./output/snap2-usa2011-mcmc1.RDS")
snap1.mcmc.1 <- readRDS("./output/snap1-usa2011-mcmc1.RDS")

######################################
### 2. Plot SO4 and emissions data ###
######################################

### Plot of 2011 Emissions Data (FIGURE 1a)

png(file = "./output/fig1a.png",
    width = 7, h = 5.75, units = "in", res = 300, bg = "transparent")

plot(usa$so4$so4.2011, legend = FALSE, axes = FALSE,
     main = "", cex.main=1.5)
map("state", add = TRUE)
points(usa$em$em.2011$fac$Fac.Longitude,
       usa$em$em.2011$fac$Fac.Latitude,
       pch = 16, cex = 0.00015 * usa$em$em.2011$fac$totSO2emissions)
plot(usa$so4$so4.2011, legend.only = TRUE,
     legend.width = 0.75, legend.shrink = 0.55,
     legend.args = 
       list(text = expression(paste(S*O[4], "  (", mu*g / m^3, ")", sep = "")), 
            side = 4, font = 2, line = 2.75, cex = 1))

dev.off()

### Plot of 2011 average wind field (FIGURE 1b)

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
arrow.pts <- sapply(row.spacing, function(x) {(n.col * x) + (1:floor(n.col / 4) * 4)})

final.x <- seq(from = 7888, to = 8000, by = 4)
arrow.pts <- cbind(arrow.pts, final.x)

# plot the raster with wind vectors
png(file = "./output/fig1b.png",
    width = 7, h = 5.75, units = "in", res = 300, bg = "transparent")

plot(usa$so4$so4.2011, legend = FALSE, axes = FALSE, 
     main = "", cex.main=1.5)
map("state", add = TRUE)
arrows(x0 = xy[arrow.pts,1], y0 = xy[arrow.pts,2],
       x1 = xy[arrow.pts,1] + 0.3 * u.w.vals[arrow.pts],
       y1 = xy[arrow.pts,2] + 0.3 * v.w.vals[arrow.pts],
       length = 0.03, col = "black")
plot(usa$so4$so4.2011, legend.only = TRUE,
     legend.width = 0.75, legend.shrink = 0.55,
     legend.args = 
       list(text = expression(paste(S*O[4], "  (", mu*g / m^3, ")", sep = "")), 
            side = 4, font = 2, line = 2.75, cex = 1))

dev.off()

############################
### 3. Inference Results ###
############################

### Posterior mean estimates and cred. ints. of model parameters (TABLE 1)

burnin <- 1:25000

# calculate credible intervals
avg2.ci <- t(apply(avg2.mcmc.1$samples[-burnin,], 2, 
        function(x) {quantile(x, probs = c(0.025, 0.975))}))

# create table one - note that order of params is slightly different than 
#   manuscript order.
table1 <- cbind(colMeans(avg2.mcmc.1$samples[-burnin,]), avg2.ci)
colnames(table1) <- c("mean", "2.5%", "97.5%")
rownames(table1) <- c("gamma", "eta", "beta", "s2", "delta", "alpha")

# table 1 (should be very similar, but not identical, to those found in paper)
table1


### Calculate DIC for each model

# set-up data structures 

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

# DIC for avg2
dic.avg2 <- dic.calc(y = y.2011, X = X.2011, 
                     burnin = 1:25000, 
                     samples = avg2.mcmc.1$samples, 
                     dic.vec = avg2.mcmc.1$dic, 
                     mats = list(D = D, C = C), 
                     model = "avg2")

# DIC for avg1
dic.avg1 <- dic.calc(y = y.2011, X = X.2011, 
                     burnin = 1:25000, 
                     samples = avg1.mcmc.1$samples, 
                     dic.vec = avg1.mcmc.1$dic, 
                     mats = list(D = D, C = C), 
                     model = "avg1")

# DIC for snap2
dic.snap2 <- dic.calc(y = y.2011, X = X.2011, 
                      burnin = 1:25000, 
                      samples = snap2.mcmc.1$samples, 
                      dic.vec = snap2.mcmc.1$dic, 
                      mats = list(D = D, C = C), 
                      model = "snap2")

# DIC for snap1
dic.snap1 <- dic.calc(y = y.2011, X = X.2011, 
                      burnin = 1:25000, 
                      samples = snap1.mcmc.1$samples, 
                      dic.vec = snap1.mcmc.1$dic, 
                      mats = list(D = D, C = C), 
                      model = "snap1")


dic.scores <- c(dic.avg2, dic.avg1, dic.snap2, dic.snap1)
names(dic.scores) <- c("Avg2", "Avg1", "Snap2", "Snap1")

# scores found in paper 
#   (note: only avg2, avg1, and snap2 were included in the paper)
dic.scores

which(min(dic.scores) == dic.scores) # Avg2 is best based on DIC.

### Empirical estimate of marginal variance of covariance, for best fit model.

# initialize parameters (as found in Table 1)
gamma.hat <- 1510 
alpha.hat <- .5
delta.hat <- 50
s2.hat <- 24000

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
system.time(
  for(k in 1:1000){
    samples[,k] <- as.vector(rnorm.Q(Q, mu, zero.constraint = FALSE, canon = FALSE))
  }
)

# estimate Sigma using 1000 samples
test.cov <- cov(t(samples))

# determine the min and max marginal variance parameters (found on page 20).
min(diag(test.cov)) # 0.0222245
max(diag(test.cov)) # 0.100128


### Mean SO4 concentration using best model (FIGURE 2a)

# parameter estimates (posterior mean)
theta.hat <- colMeans(avg2.mcmc.1$samples[-burnin,])

# expected SO4 surface (V.hat, equation 25 in paper)
mean1 <- fittedMeanSO4Surface(theta.hat, mats = list(D = D, C = C), X = X.2011)

# plot raster with fitted mean surface
plot.avg2.t1 <- usa$so4$so4.2011
values(plot.avg2.t1) <- mean1


# expected annual SO4, given coupled, time-averaged model fit
png(file = "./output/fig2a.png",
    width = 7, h = 5.75, units = "in", res = 300, bg = "transparent")

plot(plot.avg2.t1, legend = FALSE, axes = FALSE)
map("state", add = TRUE)
plot(plot.avg2.t1, legend.only = TRUE,
     legend.width = 0.75, legend.shrink = 0.55,
     legend.args =
       list(text = expression(paste(S*O[4], "  (", mu*g / m^3, ")", sep = "")),
            side = 4, font = 2, line = 2.75, cex = 1))

dev.off()


### Plot of 2011 Emissions Data (FIGURE 2b)
###   Note: Figure 2b is a repeat of Figure 1a.

png(file = "./output/fig2b.png",
    width = 7, h = 5.75, units = "in", res = 300, bg = "transparent")

plot(usa$so4$so4.2011, legend = FALSE, axes = FALSE,
     main = "", cex.main=1.5)
map("state", add = TRUE)
points(usa$em$em.2011$fac$Fac.Longitude,
       usa$em$em.2011$fac$Fac.Latitude,
       pch = 16, cex = 0.00015 * usa$em$em.2011$fac$totSO2emissions)
plot(usa$so4$so4.2011, legend.only = TRUE,
     legend.width = 0.75, legend.shrink = 0.55,
     legend.args = 
       list(text = expression(paste(S*O[4], "  (", mu*g / m^3, ")", sep = "")), 
            side = 4, font = 2, line = 2.75, cex = 1))

dev.off()


### Simulated SO4 surface from fitted coupled, time-averaged model (FIGURE 3a)

# set.seed(833)

sim1 <- simCoupled.Wind(theta = colMeans(avg2.mcmc.1$samples[-burnin,]), 
                        mats = list(D = D, C = C), X = X.2011)
plot.avg2.sim1 <- usa$so4$so4.2011
values(plot.avg2.sim1) <- sim1

png(file = "./output/fig3a.png",
    width = 7, h = 5.75, units = "in", res = 300, bg = "transparent")

plot(plot.avg2.sim1, legend = FALSE, axes = FALSE)
map("state", add = TRUE)
plot(plot.avg2.sim1, legend.only = TRUE,
     legend.width = 0.75, legend.shrink = 0.55,
     legend.args = 
       list(text = expression(paste(S*O[4], "  (", mu*g / m^3, ")", sep = "")), 
            side = 4, font = 2, line = 2.75, cex = 1))

dev.off()

### Simulated SO4 surface from fitted uncoupled, time-averaged model (FIGURE 3b)

# set.seed(328311)

avg1.sim1 <- simUncoupled.W(colMeans(avg1.mcmc.1$samples[-burnin,]),
                            mats = list(D = D, C = C),X = X.2011)
p.avg1.sim <- usa$so4$so4.2011
values(p.avg1.sim) <- avg1.sim1

png(file = "./output/fig3b.png",
    width = 7, h = 5.75, units = "in", res = 300, bg = "transparent")

plot(p.avg1.sim, legend = FALSE, axes = FALSE)
map("state", add = TRUE)
plot(p.avg1.sim, legend.only = TRUE,
     legend.width = 0.75, legend.shrink = 0.55,
     legend.args = 
       list(text = expression(paste(S*O[4], "  (", mu*g / m^3, ")", sep = "")), 
            side = 4, font = 2, line = 2.75, cex = 1))

dev.off()

### Simulated SO4 surface from fitted coupled, snapshot model (FIGURE 3c)

# set.seed(9345)

snap2.sim1 <- simCoupled.snap(colMeans(snap2.mcmc.1$samples[-burnin,]),
                              mats = list(D = D, C = C),
                              X = X.2011)
p.snap2.sim <- usa$so4$so4.2011
values(p.snap2.sim) <- snap2.sim1

png(file = "./output/fig3c.png",
    width = 7, h = 5.75, units = "in", res = 300, bg = "transparent")

plot(p.snap2.sim, legend = FALSE, axes = FALSE)
map("state", add = TRUE)
plot(p.snap2.sim, legend.only = TRUE,
     legend.width = 0.75, legend.shrink = 0.55,
     legend.args = 
       list(text = expression(paste(S*O[4], "  (", mu*g / m^3, ")", sep = "")), 
            side = 4, font = 2, line = 2.75, cex = 1))

dev.off()


################################################################################
######  Air pollution reduction: a quick glance (Section 4.2)
################################################################################

### identify top ten emitting power plants:

# identify top ten facilities
fac.2011 <- usa$em$em.2011$fac
top.ten <- which(fac.2011$totSO2emissions > 5000)
cell.locs <- cellFromXY(usa$so4$so4.2011, as.matrix(fac.2011[top.ten, 20:19]))

# X values
X.orig <- X.2011
X.new <- matrix(X.orig, nrow = length(X.orig), ncol = 10, byrow = FALSE)
for (j in 1:10){
  X.new[cell.locs[j],j] <- X.orig[cell.locs[j]] * 0.2
}

# simulate from posterior model, calculate normalized exposure (equations 26,27)
# !!! CAUTION: THIS TAKES ~5 HOURS !!!
set.seed(99)
my.exp <- expCalc(N = 2000, X.orig, X.new, samples = avg2.mcmc.1$samples, 
    burnin = 50000, mats.exp = list(D = D, C = C), pop.r = usa$pop)

diff.exp <- my.exp[,1] - my.exp[,-1]

# identify the facilities emitting the 5 largest amounts of SO2
fac.big <- fac.2011[top.ten,]
fac.top5 <- fac.big[c(2,5,6,7,9),]

# save exposure calculations for top 5 SO2-emitting facilities
small.exp <- diff.exp[,c(2,5,6,7,9)]

### TOP TEN PLANTS

#     # LAT.         # LONG.       # FACILITY NAME            # reduction  # Emissions ORDER
# 1   # 33.2442      # -86.4567    # E C Gaston (AL)          # 10         # 8
# 2   # 38.7383      # -85.4192    # Clifty Creek (IN)        # 3          # 2
# 3   # 38.9917      # -84.2981    # Walter C Beckjord (OH)   # 8          # 9    
# 4   # 39.5908      # -81.6797    # Muskingum River (OH)     # 6          # 7
# 5   # 38.9161      # -82.1281    # Kyger Creek (OH)         # 1          # 1
# 6   # 31.8206      # -96.0561    # Big Brown (TX)           # 7          # 4
# 7   # 32.2597      # -94.5703    # Martin Lake (TX)         # 5          # 3
# 8   # 33.0917      # -95.0417    # Monticello (TX)          # 9          # 6
# 9   # 37.9256      # -87.0372    # Rockport (IN)            # 4          # 5
# 10  # 38.9347      # -82.1158    # Gen. James M Gavin (OH)  # 2          # 10

### TOP FIVE PLANTS

#     # LAT.         # LONG.       # FACILITY NAME            # reduction  # Emissions ORDER
# 2   # 38.7383      # -85.4192    # Clifty Creek (IN)        # 2          # 2
# 5   # 38.9161      # -82.1281    # Kyger Creek (OH)         # 1          # 1
# 6   # 31.8206      # -96.0561    # Big Brown (TX)           # 5          # 4
# 7   # 32.2597      # -94.5703    # Martin Lake (TX)         # 4          # 3
# 9   # 37.9256      # -87.0372    # Rockport (IN)            # 3          # 5


### Plot emissions from 5 largest plants (FIGURE 4a)

png(file = "./output/fig4a.png",
    width = 7, h = 5.75, units = "in", res = 300, bg = "transparent")

plot(usa$so4$so4.2011, legend = FALSE, axes = FALSE, 
     main = "Top emissions facilities")
map("state", add = TRUE)
points(fac.top5$Fac.Longitude,
       fac.top5$Fac.Latitude,
       pch = 16, cex = 0.0002 * fac.top5$totSO2emissions)
plot(usa$so4$so4.2011, legend.only = TRUE,
     legend.width = 0.75, legend.shrink = 0.55,
     legend.args = 
       list(text = expression(paste(S*O[4], "  (", mu*g / m^3, ")", sep = "")), 
            side = 4, font = 2, line = 2.75, cex = 1))

dev.off()


### Expected decrease in SO4 after scrubber, Clifty Creek (FIGURE 4b)

# reduction in SO2 emissions after scrubber at Clifty Creek
avg.diff <- (X.2011[2180] * 0.2) - X.2011[2180]  
X.cc <- rep(0, length = length(X.2011))
X.cc[2180] <- avg.diff

mean.reduct <- fittedMeanSO4Surface(theta = colMeans(avg2.mcmc.1$samples[-burnin,]), 
                  mats = list(D = D, C = C), X = X.cc)

# raster with reduction in SO4 as values
reduction.plot <- usa$so4$so4.2011
values(reduction.plot) <- mean.reduct

png(file = "./output/fig4b.png",
    width = 7, h = 5.75, units = "in", res = 300, bg = "transparent")

plot(reduction.plot, legend = FALSE, axes = FALSE, 
     main = "Clifty Creek Facility", cex.main = 1.5)
map("state", add = TRUE)
plot(reduction.plot, legend.only = TRUE,
     legend.width = 0.5, legend.shrink = 0.55,
     legend.args = 
       list(text = expression(paste(S*O[4], "  (", mu*g / m^3, ")", sep = "")), 
            side = 4, font = 2, line = 3, cex = 1))

dev.off()

### Probabilistic forecasts of average reduction in human exposure to SO4 (FIGURE 4c)

my.data <- data.frame(text = as.character(
  c(rep("Clifty Creek, IN (12k tons)", 2000),
    rep("Kyger Creek, OH (20k tons)", 2000),
    rep("Big Brown, TX (10k tons)", 2000),
    rep("Martin Lake, TX (11k tons)", 2000),
    rep("Rockport, IN (9k tons)", 2000))),
    value = as.vector(small.exp))

# create plot with densities of reduction in exposure for each facility.
comp.plot <- my.data %>%
  mutate(text = fct_reorder(text, value)) %>%
  ggplot(aes(y=text, x=value,  fill=text)) +
  geom_density_ridges() +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("Forecasted reduction in Exposure") +
  ylab("")

# save plot
ggsave(
  filename = "./output/fig4c.png",
  plot = comp.plot,
  width = 7, 
  height = 5.75, 
  units = "in", 
  dpi = 300
)

