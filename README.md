# Supplementary Material

*Authors: Nathan B. Wikle, Ephraim M. Hanks, Lucas R. F. Henneman, and Corwin M. Zigler*

This repository contains source code and additional supplementary materials from our manuscript, "A Mechanistic Model of Annual Sulfate Concentrations in the United States." Information on numerical methods for approximating discrete processes in discrete space can be found within `supp-material.pdf`. Relevant data have been archived (doi = 10.5281/zenodo.4072504) for reproducibility.

The following instructions provide details on how to run the source code underlying the analysis, including replication of the main figures and results.

## Requirements

The code has been tested with R version 4.0.1, "See Things Now."  The following R packages must be installed before the code will run successfully:

- `raster`
- `maps`
- `ncdf4`
- `data.table`
- `stringr`
- `rgdal`
- `mvnfast`
- `sp`
- `rwc`
- `matrixStats`
- `ggridges`
- `dplyr`
- `forcats`
- `viridis`
- `hrbrthemes`
- `Matrix`
- `TruncatedNormal`
- `inborutils`
- `INLA`

## Instructions

Before running any code, make sure the required R packages have been installed.  Set the R working directory to the location of this README file (this is NOT the location of the source code, as these can be found in the `./src/` folder). Data have been archived for download at the beginning of the analysis (doi = 10.5281/zenodo.4072504). They require 1.7 GB of space within the subdirectory.

Open and run the `main.R` file, found in the `./src/` folder.  Note that this script will take a long time to run sequentially. On a 2.9 GHz Dual-Core Intel Core i7 processor, the script will take approximately 115 hours to run.  The script contains 9 sequential steps, which perform the following:

### Step One: 

- Loads required packages into R.

### Step Two: 

- Creates `./data/` and `./output/` subdirectories, which will hold the underlying data sources and analysis output, respectively.

### Step Three:

- Downloads the raw data sources used in the analysis. These data are publicly available, and have been archived (doi = 10.5281/zenodo.4072504) for reproducibility. These data require 1.7 GB of space.

### Step Four: 

- Adds all functions from the script, `functions.R`, found in the `./src/` folder.

### Step Five: 

- Loads the raw coal-fired power plant facilities data, cleans the data, and creates a data frame with relevant covariate values. After step three, the raw facility data are stored in the `./data/` folder as `AMPD_Unit_with_Sulfur_Content_and_Regulations_with_Facility_Attributes.csv`. This section saves four output RDS files - `MonthlyUnitData.RDS`, `AnnualUnitData.RDS`, `MonthlyFacilityData.RDS`, and `AnnualFacilityData.RDS` - in the `./data/` folder. 

### Step Six: 

- Cleans all data (including environmental covariates, the SO4 response variable, and facilities data), and stores them as a single raster. This raster object is saved as `./data/central-usa-data.RDS`. This raster contains all data needed for the remaining analysis. The relevant raw data sources can be found in the subfolder, `./data/`, created in Step Two.

### Step Seven: 

- Generates posterior draws (via MCMC) from the 4 models considered in the manuscript. The samples are stored as RDS files in `./output/`. **CAUTION: THIS WILL TAKE A VERY LONG TIME**. If possible, it is recommended that the individual steps 2-5 found in `./src/so4-mcmc.R` be completed in parallel, if possible.

### Step Eight:  

- Summarizes the posterior draws with Figures, Tables, and results found in Section 4 of the manuscript. The figures will be saved as PNG files in `./output/`. **CAUTION: THIS MAY TAKE UP TO 5 HOURS** (due to the creation of Figure 4c).

### Step Nine:

- Generates the three plots found within `supp-materials.pdf`. These are saved as PNGs in `./output/`.


## Output

Upon successful completion of `main.R`, the results are saved as PNGs in the `./output/` folder. Example format includes `./output/fig1a.png`, etc. These figures will look very similar, if not identical, to those found in the manuscript. Differences can be explained by small changes induced by random draws from the posterior. However, all results should be qualitatively the same.




