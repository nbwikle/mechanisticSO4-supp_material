### data_cleaning.R
### author: Nathan Wikle
###
### Clean up various data sources, placing everything in a standardized raster
###   format.

### Required packages ###
library(data.table)
library(maps)
library(ncdf4)
library(raster)
library(rgdal)
library(stringr)

### read in relevant functions ###
source(here::here("src", "functions.R"))

################################################################################
####### 1. Read in SO4 data, create raster
################################################################################

### Read in SO4 data; files from Randall Martin's group available here:
###    http://fizz.phys.dal.ca/~atmos/martin/?page_id=140

# 2011 SO4 data
file.in <- here::here("data", "GWRwSPEC_SO4_NA_201101_201112.nc")
so4.11 <- raster(file.in)

# combine SO4 data 
so4 <- stack(so4.11) 
my.extent <- c(-100, -81.5, 30.5, 41.7) # extent covering central USA

### Creat two spatial extents, one slightly larger than the other.
###   This allows us to calculate wind velocities on the edge of the 
###   smaller spatial extent.

# # create a bigger spatial extent, for wind matrix calculation...
big.extent <- c(-100.25, -81, 30, 42)
so4.big <- raster()
extent(so4.big) <- extent(big.extent)
res(so4.big) <- res(so4)
so4big <- crop(so4, so4.big)
so4big <- aggregate(so4big, 16)

# create a smaller spatial extent, for the analysis
so4.c <- raster()
extent(so4.c) <- extent(my.extent)
res(so4.c) <- res(so4)
so4c <- crop(so4, so4.c)
so4c <- aggregate(so4c, 4)

# aggregate the raster
central.usa <- aggregate(so4c, 4)

# rename SO4 rasters by year
names(central.usa) <- c("so4.2011") 


################################################################################
####### 2. Create and trim emissions data 
################################################################################

### Make unit and facility-level so2 data frames, using this source:
###   Papadogeorgou, Georgia, 2016, "Power Plant Emissions Data", 
###   https://doi.org/10.7910/DVN/M3D2NR, Harvard Dataverse, V2

# This step uses output from 'make-facility-data.R', which created four 
# .RDS files, stored in ./data/power-plants
#   AnnualFacilityData.RDS
#   AnnualUnitData.RDS
#   MonthlyFacilityData.RDS
#   MonthlyUnitData.RDS

# store monthly data in a list
month <- list()
month$fac <- readRDS(here::here("data", "MonthlyFacilityData.RDS"))
month$unit <- readRDS(here::here("data", "MonthlyUnitData.RDS"))

# clean monthly unit data
month$unit <- cleanData(month$unit)

# store annual data in a list
annual <- list()
annual$fac <- readRDS(here::here("data", "AnnualFacilityData.RDS"))
annual$unit <- readRDS(here::here("data", "AnnualUnitData.RDS"))

# rename columns
colnames(month$unit) <- unitNames()
colnames(annual$unit) <- unitNames(TRUE)

### trim emissions data by year
month.2011 <- trimYear(month, 2011)
annual.2011 <- trimYear(annual, 2011)

### trim emissions data by spatial extent
emissions.2011 <- trimData(annual.2011, my.extent)

# save yearly emissions data in a list
emissions <- list(em.2011 = emissions.2011)

### convert emissions data to X vector (design vector used in model)
X.2011 <- createSimpleX(central.usa[[1]], emissions.2011$fac)

# save emissions vectors in list
X.list <- list(X.2011 = X.2011) 


################################################################################
####### 3. Average Wind Velocities
################################################################################

### grab 2011 wind data
wind.big <- list()
wind.norm <- list()

j = 1  

uwind.raster <- yearRaster(j + 2010, met = "u-wind")
vwind.raster <- yearRaster(j + 2010, met = "v-wind")

uwind.new <- trimToSO4(uwind.raster, central.usa[[j]])
vwind.new <- trimToSO4(vwind.raster, central.usa[[j]])
  
uwind.big <- trimToSO4(uwind.raster, so4big[[j]])
vwind.big <- trimToSO4(vwind.raster, so4big[[j]])
  
big <- stack(uwind.big, vwind.big)
names(big) <- c("uwind", "vwind")
  
normal <- stack(uwind.new, vwind.new)
names(normal) <- c("uwind", "vwind")
  
wind.big[[j]] <- big
wind.norm[[j]] <- normal


names(wind.big) <- c("wind.2011")
names(wind.norm) <- c("wind.2011")


wind.big.monthly <- list()
wind.norm.monthly <- list()

j = 1  

uwind.raster.monthly <- yearRaster(j + 2010, met = "u-wind", return.mean = FALSE)
vwind.raster.monthly <- yearRaster(j + 2010, met = "v-wind", return.mean = FALSE)

uwind.new.monthly <- list()
vwind.new.monthly <- list()

for (t in 1:12){
  uwind.new.monthly[[t]] <- trimToSO4(uwind.raster.monthly[[t]], central.usa[[j]])
  vwind.new.monthly[[t]] <- trimToSO4(vwind.raster.monthly[[t]], central.usa[[j]])
}

uwind.big.monthly <- list()
vwind.big.monthly <- list()

for (t in 1:12){
  uwind.big.monthly[[t]] <- trimToSO4(uwind.raster.monthly[[t]], so4big[[j]])
  vwind.big.monthly[[t]] <- trimToSO4(vwind.raster.monthly[[t]], so4big[[j]])
}

months <- c("jan.2011", "feb.2011", "mar.2011", "apr.2011", "may.2011", "jun.2011", "jul.2011", 
                      "aug.2011", "sep.2011", "oct.2011", "nov.2011", "dec.2011")

uwind.big.monthly <- stack(uwind.big.monthly)
vwind.big.monthly <- stack(vwind.big.monthly)
big.monthly <- stack(uwind.big.monthly, vwind.big.monthly)

names(big.monthly) <- c(paste("uwind.", months, sep = ""), paste("vwind.", months, sep = ""))
  
uwind.new.monthly <- stack(uwind.new.monthly)
vwind.new.monthly <- stack(vwind.new.monthly)

normal.monthly <- stack(uwind.new.monthly, vwind.new.monthly)
names(normal.monthly) <- c(paste("uwind.", months, sep = ""), paste("vwind.", months, sep = ""))
  
wind.big.monthly <- big.monthly
wind.norm.monthly <- normal.monthly


################################################################################
####### 4. Meteorological Covariates
################################################################################

### Make rasters of annual surface temp, precip, and relative humidity
temp.list <- list()
precip.list <- list()
humidity.list <- list()

j = 1
# temperature
t.raster <- yearRaster(j + 2010, met = "temp")
t.new <- trimToSO4( t.raster, central.usa[[j]])
temp.list[[j]] <- t.new
  
# precipitation
precip.raster <- yearRaster(j + 2010, met = "precip")
precip.new <- trimToSO4(precip.raster, central.usa[[j]])
precip.list[[j]] <- precip.new
  
# relative humidity
hum.raster <- yearRaster(j + 2010, met = "humid")
hum.new <- trimToSO4(hum.raster, central.usa[[j]])
humidity.list[[j]] <- hum.new

temp.rast <- stack(temp.list[[1]]) 
precip.rast <- stack(precip.list[[1]]) 
humid.rast <- stack(humidity.list[[1]]) 

names(temp.rast) <- c("temp.2011") 
names(precip.rast) <- c("precip.2011") 
names(humid.rast) <- c("humid.2011") 


################################################################################
####### 5. Population Density (2010)
################################################################################

### create and trim population density raster
pop <- raster(here::here("data", "pden2010_60m.tif"))
pop <- trimToSO4(pop, central.usa[[1]])


################################################################################
####### 6. Save All Data (response = so4, emissions, wind, meteorology
################################################################################

### save results for future use
central.usa.data <- list(so4 = central.usa,
                         wind.big = wind.big,
                         wind.small = wind.norm,
                         X = X.list,
                         em = emissions,
                         pop = pop,
                         temp = temp.rast,
                         precip = precip.rast,
                         rel.hum = humid.rast,
                         wind.big.monthly = wind.big.monthly,
                         wind.norm.monthly = wind.norm.monthly)

saveRDS(central.usa.data, here::here("data", "central-usa-data.RDS"))

