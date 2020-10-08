### data_cleaning.R
### author: Nathan Wikle
###
### Clean up various data sources, placing everything in a standardized raster
###   format.

### Required packages ###
library(raster)
library(maps)
library(ncdf4)
library(data.table)
library(stringr)
library(rgdal)

### read in relevant functions ###
source("./src/functions.R")

################################################################################
####### 1. Read in SO4 data, create raster
################################################################################

### Read in SO4 data; files from Randall Martin's group available here:
###    http://fizz.phys.dal.ca/~atmos/martin/?page_id=140

# 2011 SO4 data
file.in <- "./data/GWRwSPEC_SO4_NA_201101_201112.nc"
so4.11 <- raster(file.in)

# # 2012 SO4 data
# file.in <- "./data/GWRwSPEC_SO4_NA_201201_201212.nc"
# so4.12 <- raster(file.in)
# 
# # 2013 SO4 data
# file.in <- "./data/GWRwSPEC_SO4_NA_201301_201312.nc"
# so4.13 <- raster(file.in)
# 
# # 2014 SO4 data
# file.in <- "./data/GWRwSPEC_SO4_NA_201401_201412.nc"
# so4.14 <- raster(file.in)
# 
# # 2015
# file.in <- "./data/GWRwSPEC_SO4_NA_201501_201512.nc"
# so4.15 <- raster(file.in)

# combine SO4 data 
so4 <- stack(so4.11) #so4.12,so4.13,so4.14,so4.15)
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
names(central.usa) <- c("so4.2011") #, "so4.2012", "so4.2013", "so4.2014", "so4.2015")

# # uncomment to plot raster
# plot(central.usa$SO4.2011, main = "Example SO4 Raster")
# map("state", add=TRUE)


################################################################################
####### 2. Create and trim emissions data 
################################################################################

### Make unit and facility-level so2 data frames, using this source:
###   Papadogeorgou, Georgia, 2016, "Power Plant Emissions Data", 
###   https://doi.org/10.7910/DVN/M3D2NR, Harvard Dataverse, V2

# # This creates 4 .RDS files, stored in ./data/power-plants
# #   AnnualFacilityData.RDS
# #   AnnualUnitData.RDS
# #   MonthlyFacilityData.RDS
# #   MonthlyUnitData.RDS
# source("./src/make_facility_data.R") # ignore the warnings, they are unimportant

# store monthly data in a list
month <- list()
month$fac <- readRDS("./data/MonthlyFacilityData.RDS")
month$unit <- readRDS("./data/MonthlyUnitData.RDS")

# clean monthly unit data
month$unit <- cleanData(month$unit)

# store annual data in a list
annual <- list()
annual$fac <- readRDS("./data/AnnualFacilityData.RDS")
annual$unit <- readRDS("./data/AnnualUnitData.RDS")

# rename columns
colnames(month$unit) <- unitNames()
colnames(annual$unit) <- unitNames(TRUE)

### trim emissions data by year
month.2011 <- trimYear(month, 2011)
annual.2011 <- trimYear(annual, 2011)

# month.2012 <- trimYear(month, 2012)
# annual.2012 <- trimYear(annual, 2012)
# 
# month.2013 <- trimYear(month, 2013)
# annual.2013 <- trimYear(annual, 2013)
# 
# month.2014 <- trimYear(month, 2014)
# annual.2014 <- trimYear(annual, 2014)
# 
# month.2015 <- trimYear(month, 2015)
# annual.2015 <- trimYear(annual, 2015)

### trim emissions data by spatial extent
emissions.2011 <- trimData(annual.2011, my.extent)
# emissions.2012 <- trimData(annual.2012, my.extent)
# emissions.2013 <- trimData(annual.2013, my.extent)
# emissions.2014 <- trimData(annual.2014, my.extent)
# emissions.2015 <- trimData(annual.2015, my.extent)

# save yearly emissions data in a list
emissions <- list(em.2011 = emissions.2011) #,
                  # em.2012 = emissions.2012,
                  # em.2013 = emissions.2013,
                  # em.2014 = emissions.2014,
                  # em.2015 = emissions.2015)

### convert emissions data to X vector (design vector used in model)
X.2011 <- createSimpleX(central.usa[[1]], emissions.2011$fac)
# X.2012 <- createSimpleX(central.usa[[2]], emissions.2012$fac)
# X.2013 <- createSimpleX(central.usa[[3]], emissions.2013$fac)
# X.2014 <- createSimpleX(central.usa[[4]], emissions.2014$fac)
# X.2015 <- createSimpleX(central.usa[[5]], emissions.2015$fac)

# save emissions vectors in list
X.list <- list(X.2011 = X.2011) # ,
               # X.2012 = X.2012,
               # X.2013 = X.2013,
               # X.2014 = X.2014,
               # X.2015 = X.2015)


################################################################################
####### 3. Average Wind Velocities
################################################################################

### grab 2011 wind data
wind.big <- list()
wind.norm <- list()

# for(j in 1:5){
j = 1  

uwind.raster <- yearRaster(j + 2000, met = "u-wind")
vwind.raster <- yearRaster(j + 2000, met = "v-wind")
  
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

# }

names(wind.big) <- c("wind.2011") # , "wind.2012", "wind.2013", "wind.2014", "wind.2015")
names(wind.norm) <- c("wind.2011") #, "wind.2012", "wind.2013", "wind.2014", "wind.2015")


################################################################################
####### 4. Meteorological Covariates
################################################################################

### Make rasters of annual surface temp, precip, and relative humidity
temp.list <- list()
precip.list <- list()
humidity.list <- list()

# for(j in 1:5){

j = 1
# temperature
t.raster <- yearRaster(j + 2000, met = "temp")
t.new <- trimToSO4( t.raster, central.usa[[j]])
temp.list[[j]] <- t.new
  
# precipitation
precip.raster <- yearRaster(j + 2000, met = "precip")
precip.new <- trimToSO4(precip.raster, central.usa[[j]])
precip.list[[j]] <- precip.new
  
# relative humidity
hum.raster <- yearRaster(j + 2000, met = "humid")
hum.new <- trimToSO4(hum.raster, central.usa[[j]])
humidity.list[[j]] <- hum.new
# }

temp.rast <- stack(temp.list[[1]]) # , 
  # temp.list[[2]], temp.list[[3]], temp.list[[4]], temp.list[[5]])

precip.rast <- stack(precip.list[[1]]) # , 
  # precip.list[[2]], precip.list[[3]], precip.list[[4]], precip.list[[5]])

humid.rast <- stack(humidity.list[[1]]) # , 
  # humidity.list[[2]], humidity.list[[3]], humidity.list[[4]], humidity.list[[5]])


names(temp.rast) <- c("temp.2011") #, "temp.2012", "temp.2013", "temp.2014", "temp.2015")
names(precip.rast) <- c("precip.2011") #, "precip.2012", "precip.2013", "precip.2014", "precip.2015")
names(humid.rast) <- c("humid.2011") #, "humid.2012", "humid.2013", "humid.2014", "humid.2015")


################################################################################
####### 5. Population Density (2010)
################################################################################

### create and trim population density raster
pop <- raster("./data/pden2010_60m.tif")
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
                         rel.hum = humid.rast)

saveRDS(central.usa.data, "./data/central-usa-data.RDS")


