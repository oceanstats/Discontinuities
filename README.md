# Discontinuities

Main code for the GRL paper "Assessing the presence of discontinuities in the ocean color satellite record and their effects on chlorophyll trends and their uncertainties"

https://doi.org/10.5281/zenodo.804404

please note that directories will need editing for the system of the user, please enter these as the variable user_wd.

Scripts 1_1 and 1_2: compile required data for project using matlab. 1_1 is for SST sourced from: https://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.html. 1_2 is for ESA OC-CCI sourced from: ftp://oceancolour.org/occci-v3.1/geographic/netcdf/monthly/chlor_a/

Script 2 formats the matlab input into separate Longhurst regions for use by spTimer

Script 3 was designed for a high power compute servers and has the different regions being run in parallel with the MCMC separated into individual sections of 1000 iterations, with the next chain running from where the last chain finished. Parallel is used to allow for the slow computation which is particularly obvious in larger regions. With the MCMC being run in sections to avoid memory constraints. 

Script 4 combines the subdivided chains.

Script 5 provides tabulation of the results and regional time-series

Script 6 reads this tabulation for the distribution and bar chart plots

Script 7 again reads this tabulation for global plots

Longhurst_180.mat contains the Longhurst divisions as featured in Longhurst (1995; 1998) with cells either NA for outide of the defined regions or with a number representing the Longhurst province in question. 

spT_simple is included as a simple script based off this code that can be more easily adapted for other usage and testing.

