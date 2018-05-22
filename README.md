# Discontinuities

Main code for the GRL paper "Assessing the presence of discontinuities in the ocean color satellite record and their effects on chlorophyll trends and their uncertainties"

https://doi.org/10.5281/zenodo.804404

Please note that directories will need editing for the system of the user, please enter these as the variable user_wd.

Please note there are up to 6 files per step indicating the different discontinuity scenarios where x represents the identifier for the discontinuity scenario. n = no discontinuity, m = MERIS/MODIS discontinuity, sw = SeaWiFS discontinuity, ms = MERIS/MODIS & SeaWiFS discontinuity, mv = MERIS/MODIS & VIIRS discontinuity, msv = MERIS/MODIS & SeaWiFS & VIIRS discontinuity.

Scripts 1.1 and 1.2 (SST_import_1_1.m & ESA_import_1_2.m): compile required data for project using matlab. 1_1 is for SST sourced from: https://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.html. 1_2 is for ESA OC-CCI sourced from: ftp://oceancolour.org/occci-v3.1/geographic/netcdf/monthly/chlor_a/

Script 2 (2_Create_spT_input.R): formats the matlab input into separate Longhurst regions for use by spTimer

Script 3 (3_x_sptmodel_run_parallel.R): Run the spTimer model based on the input in the previous stage. This was designed for computers with a number of cores with the different regions being run in parallel. The MCMC is separated into individual sections of 1000 iterations, with the next chain running from where the last chain finished. Parallel is used to allow for the slow computation which is particularly obvious in larger regions, with the MCMC being run in sections to avoid memory constraints. 

Script 4 (4_x_Chain_combine.R) Combines the subdivided chains in the previous step, for further processing and analysis in following steps.

Script 5 provides tabulation of the results and regional time-series

Script 6 again reads this tabulation for global plots

Longhurst_180.mat contains the Longhurst divisions as featured in Longhurst (1995; 1998) with cells either NA for outide of the defined regions or with a number representing the Longhurst province in question. 

spT_simple is included as a simple script based off this code that can be more easily adapted for other usage and testing.

