# Nutrient concentrations between estuarine fish and other animal-based foods

This repository contains the data and R scripts used in the manuscript:  
**"The potential of estuarine fishes in supplying micronutrients to coastal and traditional populations in Northeast Brazil"**
https://doi.org/10.1016/j.pecon.2025.10.001

## Repository structure
- **data/**: includes raw and processed datasets (Papers from systematic review, nutrient concentrations for estuarine fishes and animal-source foods).
- **R/**: R scripts used for data cleaning, statistical analysis, and visualization.
  - `artic_reviw.R`: Script used for systematic review analyses.
  - `script_pecon.R`:
  - Data cleaning;
  - Perform of Principal Component Analysis to summarize nutrient variation across food categories;
  - Runs PERMANOVA and post-hoc permutation tests to compare nutrient concentrations among food categories;
  - Generates figures for the manuscript.
- **output/**: output files (tables and figures).

## How to use
1. Clone or download this repository.
2. Open R or RStudio.
3. Install the required R packages (listed below).
4. Run the scripts in numerical order (artic_reviw → script_pecon) to reproduce the results presented in the manuscript.

## Dependencies
The analyses were conducted in R (v. 4.3.1). Required packages include:
`lmPerm`, `ggplot2`, `dplyr`, `tidyr`, `FactoMineR`, `factoextra`, `rcompanion`, `readxl`, `sf`, `ggthemes`, `janitor`, `geobr`, `ggspatial`.

## Notes
- Nutrient concentration values for estuarine fish were compiled from FishBase.
- Comparative values for beef, pork, chicken, and processed meats were obtained from the USDA database.

