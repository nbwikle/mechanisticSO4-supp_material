
### main.R
### author: Nathan Wikle.
###
### Recreates the analysis found in our manuscript,"A Mechanistic Model of 
###   Annual Sulfate Concentrations in the United States." This script serves
###   to 1) clean data, 2) call MCMC functions, and 3) reproduce relevant
###   results and figures found in the manuscript. However, if run sequentially,
###   it will likely take a very long time (~115 hours on 2.9 GHz Dual-Core 
###   Intel Core i7). It is recommended that components of 'so4-mcmc.R' be 
###   run in parallel.


### !! CAUTION: THIS R SCRIPT WILL TAKE A LONG TIME !!! ###

### 1. Required packages ###
library(raster)
library(maps)
library(ncdf4)
library(data.table)
library(stringr)
library(rgdal)
library(mvnfast)
library(sp)
library(rwc)
library(matrixStats)
library(ggridges)
library(dplyr)
library(forcats)
library(viridis)
library(hrbrthemes)
library(Matrix)
library(TruncatedNormal)
library(inborutils)
require(INLA)

### 2. Create subdirectories ###

# create a new folder for data
dir.create("./data/", showWarnings = FALSE)
# create a new folder for output
dir.create("./output/", showWarnings = FALSE)

### 3. Download data ###

# data used in analysis, size = 1.7 GB, doi = 10.5281/zenodo.4072504
download_zenodo(doi = "10.5281/zenodo.4072504", 
                path = "./data/", parallel = FALSE, quiet = FALSE)

### 4. Source in all functions used in the analysis ###
source("./src/functions.R")

### 5. Make facility data ###
source("./src/make_facility_data.R")

### 6. Clean data and create 'central-usa-data' raster ###
source("./src/data_cleaning.R")

### 7. Generate posterior samples from all relevant models ###
source("./src/so4-mcmc.R") # CAUTION! Very computationally expensive!

### 8. Compile results found in main manuscript ###
source("./src/so4-results.R") #CAUTION! Computationally expensive!

### 9. Create plots found in "supplementary-materials.pdf" ###
source("./src/supp-plots.R")

