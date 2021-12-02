
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

### 1. Required packages (version number) ###

library(colortools)       # 0.1.5
library(data.table)       # 1.13.2
library(dplyr)            # 1.0.2
library(forcats)          # 0.5.0
library(geosphere)        # 1.5.10
library(ggplot2)          # 3.3.2
library(ggridges)         # 0.5.2
library(here)             # 1.0.1
library(hrbrthemes)       # 0.8.0
library(inborutils)       # 0.1.0.9086
library(INLA)             # 19.9.3
library(maps)             # 3.3.0
library(Matrix)           # 1.2.18
library(matrixStats)      # 0.57.0
library(mvnfast)          # 0.2.5.1
library(ncdf4)            # 1.17
library(raster)           # 3.4.5
library(rgdal)            # 1.5.18
library(rwc)              # 1.11
library(sp)               # 1.4.4
library(stringr)          # 1.4.0
library(TruncatedNormal)  # 2.2
library(viridis)          # 0.5.1

### 2. Create subdirectories ###

# create a new folder for data
dir.create(here::here("data"), showWarnings = FALSE)
# create a new folder for output
dir.create(here::here("output"), showWarnings = FALSE)

### 3. Download data ###

# data used in analysis, size = 1.7 GB, doi = 10.5281/zenodo.4072504
download_zenodo(
  doi = "10.5281/zenodo.4072504",
  path = here::here("data"), parallel = FALSE, quiet = FALSE
)

### 4. Source in all functions used in the analysis ###
source(here::here("src", "functions.R"))

### 5. Make facility data ###
source(here::here("src", "make-facility-data.R"))

### 6. Clean data and create 'central-usa-data' raster ###
system.time(source(here::here("src", "data-cleaning.R")))

### 7. Generate posterior samples from all relevant models ###
#
# Note: Uncomment Sections 7 and 8 in 'so4-mcmc.R' to reproduce all 
#         supplementary materials.
#
source(here::here("src", "so4-mcmc.R")) # CAUTION! Very computationally expensive! (> 100 hours)

### 8. Compile results found in main manuscript ###
source(here::here("src", "so4-results.R")) # CAUTION! Computationally expensive! (~1.25 hours)

# ### 9. Create plots found in "supplementary-materials.pdf" ###
# #
# # Note: Uncomment this section to reproduce supplementary plots
# #
# source(here::here("src", "supp-plots.R"))

