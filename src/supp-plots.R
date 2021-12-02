### supp-plots.R
### author: Nathan Wikle
###
### Create plots found in supplementary material detailing discretization steps.

### relevant packages ###
library(colortools)
library(data.table)
require(INLA)
library(maps)
library(ncdf4)
library(raster)
library(rgdal)
library(stringr)

# make subdirectory for supplementary figures
dir.create(here::here("output", "supp-figures"), showWarnings = FALSE)

# source functions
source(here::here("src", "functions.R"))

# read usa data
usa <- readRDS(file = here::here("data", "central-usa-data.RDS"))

# ### plot USA surface ###
# plot(usa$so4$so4.2011, main = "Example SO4 Raster")
# map("state", add = TRUE)

### plot regular grid (Supp Figure 1a) ###

# 58 x 35 regular grid
png(file = here::here("output", "supp-figures", "reg-grid-eg.png"),
    width = 7, h = 5.75, units = "in", res = 300, bg = "transparent")

map("state", ylim = c(30.5, 41.7), xlim = c(-100, -81.44),
    boundary = TRUE, lwd = 3)
grid(58, 35, lty = 1, lwd = 2, col = "darkgray")
map("state", add = TRUE, lwd = 3)
box(lwd = 3)

dev.off()

### create finite grid ###

# 1) create mesh (using INLA implementation) 

# points
x <- seq(from = -99.5, to = -82, by = 1)
y <- seq(from = 31, to = 41.3, by = 1)
loc <- as.matrix(expand.grid(x[-c(1,117)], y[-c(1,71)]))

# create boundary
loc.bnd = matrix(c(-100,   30.5, 
                   -81.44, 30.5,
                   -81.44, 41.7,
                   -100,   41.7),
                 4, 2, byrow=TRUE)

segm.bnd = inla.mesh.segment(loc.bnd, is.bnd = FALSE)
segm.bnd$idx <- matrix(c(1,2,2,3,3,4,4,1), 
                       nrow = 4, ncol = 2, byrow = T)
segm.bnd$grp <- matrix(rep(1,4), nrow = 4, ncol = 1)   

# create mesh from points and boundary
mesh1 <- inla.mesh.create(loc = loc,
                          boundary = list(segm.bnd),
                          refine = list(max.edge = .75))


# 2) plot mesh (Delauney triangulation)

### plot delauney triangulation grid (Supp Figure 1b) ###
png(file = here::here("output", "supp-figures", "reg-mesh-eg.png"),
    width = 7, h = 5.75, units = "in", res = 300, bg = "transparent")

map("state", ylim = c(30.5, 41.7), xlim = c(-100, -81.44),
    boundary = TRUE, lwd = 3)
plot(mesh1, #ylim = c(30.8, 41.4), xlim = c(-99.45, -81.99),
     draw.segments = FALSE, col = "darkgray", edge.col = "darkgray",
     main = "", add = TRUE, lwd = 2)
map("state", ylim = c(30.5, 41.7), xlim = c(-100, -81.44),
    boundary = TRUE, lwd = 3, add = TRUE)
box(lwd = 3)

dev.off()


### Grid used in paper (Supp Figure 3) ###
png(file = here::here("output", "supp-figures", "official-grid.png"),
    width = 7, h = 5.75, units = "in", res = 300, bg = "transparent")

# points
x.official <- seq(from = -99.92, to = -81.52, by = .16)
y.official <- seq(from = 30.58, to = 41.62, by = .16)
loc.official <- as.matrix(expand.grid(x.official, y.official))


plot(usa$so4$so4.2011, legend = FALSE, axes = FALSE,
     main = "", cex.main=1.5)#main = "estimated avg. SO4")
map("state", add = TRUE)

points(loc.official,
       pch = 16, cex = 0.25)

plot(usa$so4$so4.2011, legend.only = TRUE,
     legend.width = 0.75, legend.shrink = 0.55,
     legend.args = 
       list(text = expression(paste(S*O[4], "  (", mu*g / m^3, ")", sep = "")), 
            side = 4, font = 2, line = 2.75, cex = 1))
dev.off()


#### grid sensitivity analysis (Supp Figure 4) ###
{
png(file = here::here("output", "supp-figures", "grid-mean-plots.png"), 
  width = 8, h = 12, units = "in", res = 300, bg = "transparent")

  # plotting parameters
  par(mfrow = c(5,2))
  par(mar = c(2,2,2,3),
      oma = c(2,2,6,2))  

  ## 1. res = 64
  load(file = here::here("data", "grid", "grid-ag-64.RData"))
  mcmc <- readRDS(here::here("output", "grid", "grid-res-64.RDS"))

  plot.dims <- dim(so4.norm)[1:2]
  n.cells <- plot.dims[1] * plot.dims[2]
  cell.area <- median(values(raster::area(so4.norm))) 

  # mean SO4
  mu <- indep.mean(samples = mcmc$samples, X = X.2011,
                   mats = list(D = D, C = C), burnin = 25000)

  mean.plt <- so4.norm      
  values(mean.plt) <- as.vector(mu)

  plot(mean.plt, legend = FALSE, axes = FALSE,
     breaks = breakpoint.creation(mean.plt, n = 255, 
                min.val = 0.91,
                max.val = 3.5),
     col = rev(terrain.colors(255)),
     main = "", cex.main = 1.5,
     box = TRUE)

  map("state", add = TRUE)

  plot(mean.plt, legend.only = TRUE,
      legend.width = 0.75, legend.shrink = 0.55,
      breaks = breakpoint.creation(mean.plt, n = 255, 
                min.val = 0.91,
                max.val = 3.5),
      col = rev(terrain.colors(255)),
      axis.args = list(at=c(1, 1.5, 2, 2.5, 3, 3.5),
                      labels=c(1, 1.5, 2, 2.5, 3, 3.5), 
                      cex.axis=0.75))

  title(paste("1)  n = ", n.cells, " (", plot.dims[1], " x ", 
    plot.dims[2], "), cell area \u2248 ", round(cell.area), " sq.km", sep = ""), 
    adj = 0, line = 1, cex.main = 1.25, outer = F)

  title("Fitted Mean SO4 Surfaces, Arranged by Grid Size", adj = 0,
        line = 4.5, cex.main = 2, outer = TRUE)

  title("Expected Mean SO4", adj = 0.15,
        line = 2, cex.main = 1.5, outer = TRUE)

  title("Observed SO4", adj = 0.77,
        line = 2, cex.main = 1.5, outer = TRUE)

  # observed SO4
  plot(so4.norm, legend = FALSE, axes = FALSE,
    breaks = breakpoint.creation(so4.norm, n = 255, 
                min.val = 0.91,
                max.val = 3.5),
    col = rev(terrain.colors(255)),
    main = "", cex.main = 1.5,
    box = TRUE)
  map("state", add = TRUE)
  plot(so4.norm, legend.only = TRUE,
      legend.width = 0.75, legend.shrink = 0.55,
      breaks = breakpoint.creation(so4.norm, n = 255, 
                min.val = 0.91,
                max.val = 3.5),
      col = rev(terrain.colors(255)),
      axis.args = list(at=c(1, 1.5, 2, 2.5, 3, 3.5),
                      labels=c(1, 1.5, 2, 2.5, 3, 3.5), 
                      cex.axis=0.75))

  # grid points
  grid.centers <- xyFromCell(so4.norm, 1:n.cells)
  points(grid.centers,
       pch = 16, cex = 0.5)

  ## 2. res = 48

  load(file = here::here("data", "grid", "grid-ag-48.RData"))
  mcmc <- readRDS(here::here("output", "grid", "grid-res-48.RDS"))

  plot.dims <- dim(so4.norm)[1:2]
  n.cells <- plot.dims[1] * plot.dims[2]
  cell.area <- median(values(raster::area(so4.norm))) 

  # mean SO4
  mu <- indep.mean(samples = mcmc$samples, X = X.2011,
                   mats = list(D = D, C = C), burnin = 25000)

  mean.plt <- so4.norm      
  values(mean.plt) <- as.vector(mu)

  plot(mean.plt, legend = FALSE, axes = FALSE,
     breaks = breakpoint.creation(mean.plt, n = 255, 
                min.val = 0.91,
                max.val = 3.5),
     col = rev(terrain.colors(255)),
     main = "", cex.main = 1.5,
     box = TRUE)

  map("state", add = TRUE)

  plot(mean.plt, legend.only = TRUE,
      legend.width = 0.75, legend.shrink = 0.55,
      breaks = breakpoint.creation(mean.plt, n = 255, 
                min.val = 0.91,
                max.val = 3.5),
      col = rev(terrain.colors(255)),
      axis.args = list(at=c(1, 1.5, 2, 2.5, 3, 3.5),
                      labels=c(1, 1.5, 2, 2.5, 3, 3.5), 
                      cex.axis=0.75))

  title(paste("2)  n = ", n.cells, " (", plot.dims[1], " x ", 
    plot.dims[2], "), cell area \u2248 ", round(cell.area), " sq.km", sep = ""), 
    adj = 0, line = 1, cex.main = 1.25, outer = F)

  # observed SO4
  plot(so4.norm, legend = FALSE, axes = FALSE,
    breaks = breakpoint.creation(so4.norm, n = 255, 
                min.val = 0.91,
                max.val = 3.5),
    col = rev(terrain.colors(255)),
    main = "", cex.main = 1.5,
    box = TRUE)
  map("state", add = TRUE)
  plot(so4.norm, legend.only = TRUE,
      legend.width = 0.75, legend.shrink = 0.55,
      breaks = breakpoint.creation(so4.norm, n = 255, 
                min.val = 0.91,
                max.val = 3.5),
      col = rev(terrain.colors(255)),
      axis.args = list(at=c(1, 1.5, 2, 2.5, 3, 3.5),
                      labels=c(1, 1.5, 2, 2.5, 3, 3.5), 
                      cex.axis=0.75))

  # grid points
  grid.centers <- xyFromCell(so4.norm, 1:n.cells)
  points(grid.centers,
       pch = 16, cex = 0.5)

  ## 3. res = 32
  load(file = here::here("data", "grid", "grid-ag-32.RData"))
  mcmc <- readRDS(here::here("output", "grid", "grid-res-32.RDS"))

  plot.dims <- dim(so4.norm)[1:2]
  n.cells <- plot.dims[1] * plot.dims[2]
  cell.area <- median(values(raster::area(so4.norm))) 

  # mean SO4
  mu <- indep.mean(samples = mcmc$samples, X = X.2011,
                   mats = list(D = D, C = C), burnin = 25000)

  mean.plt <- so4.norm      
  values(mean.plt) <- as.vector(mu)

  plot(mean.plt, legend = FALSE, axes = FALSE,
     breaks = breakpoint.creation(mean.plt, n = 255, 
                min.val = 0.91,
                max.val = 3.5),
     col = rev(terrain.colors(255)),
     main = "", cex.main = 1.5,
     box = TRUE)

  map("state", add = TRUE)

  plot(mean.plt, legend.only = TRUE,
      legend.width = 0.75, legend.shrink = 0.55,
      breaks = breakpoint.creation(mean.plt, n = 255, 
                min.val = 0.91,
                max.val = 3.5),
      col = rev(terrain.colors(255)),
      axis.args = list(at=c(1, 1.5, 2, 2.5, 3, 3.5),
                      labels=c(1, 1.5, 2, 2.5, 3, 3.5), 
                      cex.axis=0.75))

  title(paste("3)  n = ", n.cells, " (", plot.dims[1], " x ", 
    plot.dims[2], "), cell area \u2248 ", round(cell.area), " sq.km", sep = ""), 
    adj = 0, line = 1, cex.main = 1.25, outer = F)

  # observed SO4
  plot(so4.norm, legend = FALSE, axes = FALSE,
    breaks = breakpoint.creation(so4.norm, n = 255, 
                min.val = 0.91,
                max.val = 3.5),
    col = rev(terrain.colors(255)),
    main = "", cex.main = 1.5,
    box = TRUE)
  map("state", add = TRUE)
  plot(so4.norm, legend.only = TRUE,
      legend.width = 0.75, legend.shrink = 0.55,
      breaks = breakpoint.creation(so4.norm, n = 255, 
                min.val = 0.91,
                max.val = 3.5),
      col = rev(terrain.colors(255)),
      axis.args = list(at=c(1, 1.5, 2, 2.5, 3, 3.5),
                      labels=c(1, 1.5, 2, 2.5, 3, 3.5), 
                      cex.axis=0.75))

  # grid points
  grid.centers <- xyFromCell(so4.norm, 1:n.cells)
  points(grid.centers,
       pch = 16, cex = 0.5)

  ## 4. res = 16
  load(file = here::here("data", "grid", "grid-ag-16.RData"))
  mcmc <- readRDS(here::here("output", "grid", "grid-res-16.RDS"))

  plot.dims <- dim(so4.norm)[1:2]
  n.cells <- plot.dims[1] * plot.dims[2]
  cell.area <- median(values(raster::area(so4.norm))) 

  # mean SO4
  mu <- indep.mean(samples = mcmc$samples, X = X.2011,
                   mats = list(D = D, C = C), burnin = 25000)

  mean.plt <- so4.norm      
  values(mean.plt) <- as.vector(mu)

  plot(mean.plt, legend = FALSE, axes = FALSE,
     breaks = breakpoint.creation(mean.plt, n = 255, 
                min.val = 0.91,
                max.val = 3.5),
     col = rev(terrain.colors(255)),
     main = "", cex.main = 1.5,
     box = TRUE)

  map("state", add = TRUE)

  plot(mean.plt, legend.only = TRUE,
      legend.width = 0.75, legend.shrink = 0.55,
      breaks = breakpoint.creation(mean.plt, n = 255, 
                min.val = 0.91,
                max.val = 3.5),
      col = rev(terrain.colors(255)),
      axis.args = list(at=c(1, 1.5, 2, 2.5, 3, 3.5),
                      labels=c(1, 1.5, 2, 2.5, 3, 3.5), 
                      cex.axis=0.75))

  title(paste("4)  n = ", n.cells, " (", plot.dims[1], " x ", 
    plot.dims[2], "), cell area \u2248 ", round(cell.area), " sq.km", sep = ""), 
    adj = 0, line = 1, cex.main = 1.25, outer = F)

  # observed SO4
  plot(so4.norm, legend = FALSE, axes = FALSE,
    breaks = breakpoint.creation(so4.norm, n = 255, 
                min.val = 0.91,
                max.val = 3.5),
    col = rev(terrain.colors(255)),
    main = "", cex.main = 1.5,
    box = TRUE)
  map("state", add = TRUE)
  plot(so4.norm, legend.only = TRUE,
      legend.width = 0.75, legend.shrink = 0.55,
      breaks = breakpoint.creation(so4.norm, n = 255, 
                min.val = 0.91,
                max.val = 3.5),
      col = rev(terrain.colors(255)),
      axis.args = list(at=c(1, 1.5, 2, 2.5, 3, 3.5),
                      labels=c(1, 1.5, 2, 2.5, 3, 3.5), 
                      cex.axis=0.75))

  # grid points
  grid.centers <- xyFromCell(so4.norm, 1:n.cells)
  points(grid.centers,
       pch = 16, cex = 0.25)

  ## 5. res = 10
  load(file = here::here("data", "grid", "grid-ag-10.RData"))
  mcmc <- readRDS(here::here("output", "grid", "grid-res-10.RDS"))

  plot.dims <- dim(so4.norm)[1:2]
  n.cells <- plot.dims[1] * plot.dims[2]
  cell.area <- median(values(raster::area(so4.norm))) 

  # mean SO4
  mu <- indep.mean(samples = mcmc$samples[1:50000,], X = X.2011,
                   mats = list(D = D, C = C), burnin = 25000)

  mean.plt <- so4.norm      
  values(mean.plt) <- as.vector(mu)

  plot(mean.plt, legend = FALSE, axes = FALSE,
     breaks = breakpoint.creation(mean.plt, n = 255, 
                min.val = 0.91,
                max.val = 3.5),
     col = rev(terrain.colors(255)),
     main = "", cex.main = 1.5,
     box = TRUE)

  map("state", add = TRUE)

  plot(mean.plt, legend.only = TRUE,
      legend.width = 0.75, legend.shrink = 0.55,
      breaks = breakpoint.creation(mean.plt, n = 255, 
                min.val = 0.91,
                max.val = 3.5),
      col = rev(terrain.colors(255)),
      axis.args = list(at=c(1, 1.5, 2, 2.5, 3, 3.5),
                      labels=c(1, 1.5, 2, 2.5, 3, 3.5), 
                      cex.axis=0.75))

  title(paste("5)  n = ", n.cells, " (", plot.dims[1], " x ", 
    plot.dims[2], "), cell area \u2248 ", round(cell.area), " sq.km", sep = ""), 
    adj = 0, line = 1, cex.main = 1.25, outer = F)

  # observed SO4
  plot(so4.norm, legend = FALSE, axes = FALSE,
    breaks = breakpoint.creation(so4.norm, n = 255, 
                min.val = 0.91,
                max.val = 3.5),
    col = rev(terrain.colors(255)),
    main = "", cex.main = 1.5,
    box = TRUE)
  map("state", add = TRUE)
  plot(so4.norm, legend.only = TRUE,
      legend.width = 0.75, legend.shrink = 0.55,
      breaks = breakpoint.creation(so4.norm, n = 255, 
                min.val = 0.91,
                max.val = 3.5),
      col = rev(terrain.colors(255)),
      axis.args = list(at=c(1, 1.5, 2, 2.5, 3, 3.5),
                      labels=c(1, 1.5, 2, 2.5, 3, 3.5), 
                      cex.axis=0.75))

  # grid points
  grid.centers <- xyFromCell(so4.norm, 1:n.cells)
  points(grid.centers,
       pch = 16, cex = 0.2)

dev.off()
}


### scaled beta values:
num.ag <- c(10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32,
            36, 40, 44, 48, 52, 56, 60, 64)

beta.scaled <- matrix(data = NA, nrow = 20, ncol = 3)
cell.area <- rep(0, length(num.ag))

for (k in 20:1){
 
    load(file = paste(here::here("data", "grid", "grid-ag-"), num.ag[k], ".RData", sep = ""))
    mcmc <- readRDS(paste(here::here("output", "grid", "grid-res-"), num.ag[k], ".RDS", sep = ""))

    cell.area[k] <- median(values(raster::area(so4.norm))) 

    if (is.na(mcmc)[1]){
        beta.scaled[k,] <- c(NA,NA,NA)
    } else {

        quant.int <- mcmc$samples[-c(1:25000),3] * cell.area[k] / 254.9036 # compared to beta with 0.16x0.16 resolution
        beta.scaled[k,] <- quantile(quant.int, c(0.025, 0.5, 0.975))
    }
}

png(file = here::here("output", "supp-figures", "grid-beta-scaled.png"), 
  width = 6, h = 6, units = "in", res = 300, bg = "transparent")

par(mar = c(4,4,4,4))

plot(cell.area, beta.scaled[, 2],
  pch = 20, cex = 1.5,
  main = expression(paste("Estimated rate of ", S * O[4], " emissions (", beta, ")")), xlab = "median cell area (sq. km)",
  ylab = expression(paste(beta)), ylim = c(3.5, 6)
)
segments(cell.area, beta.scaled[,1], cell.area, beta.scaled[,3], lwd = 1.5)

legend("topleft",
  pch = c(20, NA), lty = c(NA, 1), lwd = c(NA, 1.5),
  legend = c("posterior median", "95% credible interval"),
  box.lty = 0, bg = "transparent"
)

dev.off()

### scaled gamma values:
num.ag <- c(10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32,
            36, 40, 44, 48, 52, 56, 60, 64)

gamma.scaled <- matrix(data = NA, nrow = 20, ncol = 3)
cell.area <- rep(0, length(num.ag))

for (k in 20:1){
 
    load(file = paste(here::here("data", "grid", "grid-ag-"), num.ag[k], ".RData", sep = ""))
    mcmc <- readRDS(paste(here::here("output", "grid", "grid-res-"), num.ag[k], ".RDS", sep = ""))

    cell.area[k] <- median(values(raster::area(so4.norm))) 

    quant.int <- mcmc$samples[-c(1:25000),1] * sqrt(cell.area[k] / 254.9036) # compared to beta with 0.16x0.16 resolution
    gamma.scaled[k,] <- quantile(quant.int, c(0.025, 0.5, 0.975))
   
}

png(file = here::here("output", "supp-figures", "grid-gamma-scaled.png"), 
  width = 6, h = 6, units = "in", res = 300, bg = "transparent")

par(mar = c(4,4,4,4))

plot(cell.area, gamma.scaled[, 2],
  pch = 20, cex = 1.5,
  main = expression(paste("Estimated diffusion coefficient (", gamma, ")")), xlab = "median cell area (sq. km)",
  ylab = expression(paste(gamma)), ylim = c(1000, 2500)
)
segments(cell.area, gamma.scaled[,1], cell.area, gamma.scaled[,3], lwd = 1.5)

legend("topleft",
  pch = c(20, NA), lty = c(NA, 1), lwd = c(NA, 1.5),
  legend = c("posterior median", "95% credible interval"),
  box.lty = 0, bg = "transparent"
)

dev.off()

### scaled s2 values:
num.ag <- c(10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32,
            36, 40, 44, 48, 52, 56, 60, 64)

s2.scaled <- matrix(data = NA, nrow = 20, ncol = 3)
cell.area <- rep(0, length(num.ag))

for (k in 20:1){
 
    load(file = paste(here::here("data", "grid", "grid-ag-"), num.ag[k], ".RData", sep = ""))
    mcmc <- readRDS(paste(here::here("output", "grid", "grid-res-"), num.ag[k], ".RDS", sep = ""))

    cell.area[k] <- median(values(raster::area(so4.norm))) 

    quant.int <- sqrt(mcmc$samples[-c(1:25000),4]) * sqrt(cell.area[k] / 254.9036) # compared to beta with 0.16x0.16 resolution
    s2.scaled[k,] <- quantile(quant.int, c(0.025, 0.5, 0.975))
}

png(file = here::here("output", "supp-figures", "grid-s2-scaled.png"), 
  width = 6, h = 6, units = "in", res = 300, bg = "transparent")

par(mar = c(4,4,4,4))

plot(cell.area, s2.scaled[, 2],
  pch = 20, cex = 1.5,
  main = expression(paste("Estimated B.M. standard deviation (", sigma, ")")), xlab = "median cell area (sq. km)",
  ylab = expression(paste(sigma)), ylim = c(0, 500)
)
segments(cell.area, s2.scaled[,1], cell.area, s2.scaled[,3], lwd = 1.5)

legend("topleft",
  pch = c(20, NA), lty = c(NA, 1), lwd = c(NA, 1.5),
  legend = c("posterior median", "95% credible interval"),
  box.lty = 0, bg = "transparent"
)

dev.off()

### scaled s2 values:
num.ag <- c(10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32,
            36, 40, 44, 48, 52, 56, 60, 64)

eta.scaled <- matrix(data = NA, nrow = 20, ncol = 3)
cell.area <- rep(0, length(num.ag))

for (k in 20:1){
 
    load(file = paste(here::here("data", "grid", "grid-ag-"), num.ag[k], ".RData", sep = ""))
    mcmc <- readRDS(paste(here::here("output", "grid", "grid-res-"), num.ag[k], ".RDS", sep = ""))

    cell.area[k] <- median(values(raster::area(so4.norm))) 

    quant.int <- mcmc$samples[-c(1:25000),2] * cell.area[k] / 254.9036 # compared to beta with 0.16x0.16 resolution
    eta.scaled[k,] <- quantile(quant.int, c(0.025, 0.5, 0.975))
}

png(file = here::here("output", "supp-figures", "grid-eta-scaled.png"), 
  width = 6, h = 6, units = "in", res = 300, bg = "transparent")

par(mar = c(4,4,4,4))

plot(cell.area, eta.scaled[, 2],
  pch = 20, cex = 1.5,
  main = expression(paste(S * O[2], " to ", S * O[4], " reaction rate (", eta, ")")), xlab = "median cell area (sq. km)",
  ylab = expression(paste(eta)), ylim = c(0, 100)
)
segments(cell.area, eta.scaled[,1], cell.area, eta.scaled[,3], lwd = 1.5)

legend("topleft",
  pch = c(20, NA), lty = c(NA, 1), lwd = c(NA, 1.5),
  legend = c("posterior median", "95% credible interval"),
  box.lty = 0, bg = "transparent"
)

dev.off()

### Combined plot; parameter values across grid size (Supp Figure 5) ###
{
png(file = here::here("output", "supp-figures", "grid-combined-scaled.png"),
  width = 10, h = 10, units = "in", res = 300, bg = "transparent")

par(mfrow = c(2,2))

par(mar = c(4,4,4,4))
par(oma = c(1,1,1,1))

# A. beat
plot(cell.area, beta.scaled[, 2],
  pch = 20, cex = 1.5,
  main = "", xlab = "median cell area (sq. km)",
  ylab = expression(bold(paste(beta))), ylim = c(3.5, 6)
)
segments(cell.area, beta.scaled[,1], cell.area, beta.scaled[,3], lwd = 1.5)

legend("topleft",
  pch = c(20, NA), lty = c(NA, 1), lwd = c(NA, 1.5),
  legend = c("posterior median", "95% credible interval"),
  box.lty = 0, bg = "transparent"
)

title(main = expression(bold(paste("A.  Rate of ", S*O[4], " emissions (", beta, ")"))), line = 1, adj = 0.01)

# B. gamma
plot(cell.area, gamma.scaled[, 2],
  pch = 20, cex = 1.5,
  main = "", xlab = "median cell area (sq. km)",
  ylab = expression(bold(paste(gamma))), ylim = c(1000, 2500)
)
segments(cell.area, gamma.scaled[,1], cell.area, gamma.scaled[,3], lwd = 1.5)

legend("topleft",
  pch = c(20, NA), lty = c(NA, 1), lwd = c(NA, 1.5),
  legend = c("posterior median", "95% credible interval"),
  box.lty = 0, bg = "transparent"
)

title(main = expression(bold(paste("B.  Diffusion coefficient (", gamma, ")"))), line = 1, adj = 0.01)

# C. eta
plot(cell.area, eta.scaled[, 2],
  pch = 20, cex = 1.5,
  main = "", xlab = "median cell area (sq. km)",
  ylab = expression(bold(paste(eta))), ylim = c(0, 100)
)
segments(cell.area, eta.scaled[,1], cell.area, eta.scaled[,3], lwd = 1.5)

title(main = expression(bold(paste("C.  ", S * O[2], " to ", S * O[4], " reaction rate (", eta, ")"))), line = 1, adj = 0.01)

legend("topleft",
  pch = c(20, NA), lty = c(NA, 1), lwd = c(NA, 1.5),
  legend = c("posterior median", "95% credible interval"),
  box.lty = 0, bg = "transparent"
)

# D. sigma
plot(cell.area, s2.scaled[, 2],
  pch = 20, cex = 1.5,
  main = "", xlab = "median cell area (sq. km)",
  ylab = expression(bold(paste(sigma))), ylim = c(0, 500)
)
segments(cell.area, s2.scaled[,1], cell.area, s2.scaled[,3], lwd = 1.5)

title(main = expression(bold(paste("D.  B.M. standard deviation (", sigma, ")"))), line = 1, adj = 0.01)

legend("topleft",
  pch = c(20, NA), lty = c(NA, 1), lwd = c(NA, 1.5),
  legend = c("posterior median", "95% credible interval"),
  box.lty = 0, bg = "transparent"
)

title(main = "Estimated parameter values by grid size (cell area)", line = -0.5, adj = 0, outer = TRUE, cex.main = 1.5)

dev.off()
}


### Monthly Wind Fields (Supp Figure 6) ###

my.cols <- wheel("steelblue", num = 12)

### A. monthly wind vectors

# grab monthly wind data
wind.monthly <- usa$wind.norm.monthly
months <- c("January", "February", "March", "April",
            "May", "June", "July", "August",
            "September", "October", "November", "December")

## 1) plot the raster with wind vectors on same plot

png(file = here::here("output", "supp-figures", "wind-months-together.png"),
    width = 12, h = 8, units = "in", res = 300, bg = "white")

# set plot size and margins
par(mfrow = c(1,1))
par(mar = c(1,1,3,3),
    oma = c(1,1,1,5))

# plot blank raster with map of US states
r <- wind.monthly[[j]]
values(r) <- rep(NA, length(values(r)))
plot(r, legend = FALSE, axes = FALSE, cex.main=1.5)
map("state", add = TRUE, col = "grey")

for (j in 1:12){
    # u and v wind values
    xy <- coordinates(wind.monthly[[j]])
    u.w.vals <- values(wind.monthly[[j]])
    v.w.vals <- values(wind.monthly[[12 + j]])

    n.row <- dim(wind.monthly[[j]])[1]
    n.col <- dim(wind.monthly[[j]])[2]
    
    rows <- seq(from = 5, to = 65, by = 10)
    cols <- seq(from = 3, to = 113, by = 11)
    points <- expand.grid(rows, cols)

    arrow.pts <- n.col * (points[,1] - 1) + points[,2]

    arrows(x0 = xy[arrow.pts,1], y0 = xy[arrow.pts,2],
        x1 = xy[arrow.pts,1] + 0.3 * u.w.vals[arrow.pts],
        y1 = xy[arrow.pts,2] + 0.3 * v.w.vals[arrow.pts],
        length = 0.03, col = alpha(my.cols[j], alpha = 0.85),
        angle = 20, lwd = 1, code = 2)
}
plot(r, legend.only = TRUE,
     legend.width = 0.8, legend.shrink = 0.7,
     breaks = c(0:12), 
     col = my.cols,
     axis.args = list(at=seq(0.5, 11.5, by = 1),
                    labels=months, 
                    cex.axis=0.75),
     legend.args = list(text = "Month", adj = 0.25,
            side = 3, font = 2, line = 0.5, cex = 1.5))

title("2011 Monthly Wind Vector Fields", adj = 0.01, line = 0.5, 
    cex.main = 1.5, outer = FALSE)
dev.off()

