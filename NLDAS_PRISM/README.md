# NLDAS_PRISM

the scripts in this folder collate and harmonize the NLDAS and PRISM data to be used in the SWAT model

## 1_NLDAS_download.R -- data access date 2023-03-26

This script uses NASA's ('data rods')[https://disc.gsfc.nasa.gov/information/tools?title=Hydrology%20Data%20Rods] mechanism to extract timeseries data from the NLDAS-2 forcing dataset. In this script, data are converted as necessary into the variables needed for the SWAT program. See script for details on conversions. Lat/lon match those used for PRISM extraction.

## 2_NLDAS_PRISM_harmonize.R

This script grabs the collated NLDAS files (From the `1_NLDAS_download.R` script) and the PRISM data (downloaded manually by LG by lat/lon).  Used PRISM's definition of a 'day' for data summaries of NLDAS data (noon-noon UTC time, 7a-7a EST). 

# Suggested Citations

## For data rods:

Teng, W., H. Rui, R. Strub, and B. Vollmer, 2016. Optimal reorganization of NASA earth science data for enhanced accessibility and usability for the hydrology community, Journal of the American Water Resources Association (JAWRA), 52(4), 825-835, doi:10.1111/1752-1688.12405.

## For NLDAS-2 forcing data:

Xia, Y., et al., NCEP/EMC (2009), NLDAS Primary Forcing Data L4 Hourly 0.125 x 0.125 degree V002, Edited by David Mocko, NASA/GSFC/HSL, Greenbelt, Maryland, USA, Goddard Earth Sciences Data and Information Services Center (GES DISC), Accessed: [Data Access Date], 10.5067/6J5LHHOHZHN4

Xia, Y., K. Mitchell, M. Ek, J. Sheffield, B. Cosgrove, E. Wood, L. Luo, C. Alonge, H. Wei, J. Meng, B. Livneh, D. Lettenmaier, V. Koren, Q. Duan, K. Mo, Y. Fan, and D. Mocko (2012). Continental-scale water and energy flux analysis and validation for the North American Land Data Assimilation System project phase 2 (NLDAS-2): 1. Intercomparison and application of model products. J. Geophys. Res., 117, D03109, doi:10.1029/2011JD016048