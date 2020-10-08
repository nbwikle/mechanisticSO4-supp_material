### supp-plots.R
### author: Nathan Wikle
###
### Create plots found in supplementary material detailing discretization steps.

### relevant packages ###
library(raster)
library(maps)
library(ncdf4)
library(data.table)
library(stringr)
library(rgdal)
require(INLA)

### plot USA surface ###

# read usa data
usa <- readRDS(file = "./data/central-usa-data.RDS")

# plot usa surface
plot(usa$so4$so4.2011, main = "Example SO4 Raster")
map("state", add=TRUE)


### plot regular grid ###


# 58 x 35 regular grid
png(file = "./output/reg-grid-eg.png",
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

png(file = "./output/reg-mesh-eg.png",
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


### Grid used in paper ###


png(file = "./output/official-grid.png",
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


