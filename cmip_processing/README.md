# cmip_processing
This folder contains scripts to process the manually-downloaded CMIP LOCA climate and hydrology files for use in SWAT. 

## General file locations

* CMIP LOCA files are located in the Google Drive Folder 'Daily Weather (1976-2050)'; created by Lars Gundersen (lgunders@bates.edu)
* Watershed shape files are located in the above folder as well. These are the SWAT-derived watersheds.
* extracted .csvs of the CMIP files are currently stored in the [Dropbox folder 'EPSCoR_swat'](https://www.dropbox.com/sh/s0vr0ojhf7kt9hp/AABeHATrYE07CTvNhy3sOU1ta?dl=0); created by B. Steele (steeleb@caryinstitute.org/bsteele@bates.edu)


## Overarching Workflow

1) Downscaled CMIP5 LOCA daily files are manually downloaded from [this website](https://gdo-dcp.ucllnl.org/downscaled_cmip_projections/dcpInterface.html#Welcome) and saved in the Google Drive folder 'Daily Weather (1976-2050)' (please request access permission from Lars Gundersen and Holly Ewing (lgunders@bates.edu and hewing@bates.edu))

2) SWAT-derived watersheds were created and saved in the same Google Drive folder.

3) Check the CMIP files to make sure the extent is correct for the SWAT-derived watershed.

4) Extract the CMIP netcdf's for each SWAT-derived watershed.

5) Collate the extraction files for SWAT input.


## Specific Repository Code Workflow

All scripts are meant to be run with the R Project file 'cmip_processing.Rproj' open in R Studio.

*0_check_cmip_area.R*: this script checks to make sure your watershed files are functioning as they should and that the extent of your CMIP LOCA files are correct. This runs one climate file and one hydrology file and outputs a png in the 'test' folder within this repo. Run this before extracting CMIP files to check for errors. 

*1_authorize_source.R*: this is the primary workhorse of this repo. This script sources a number of other scripts within the repo: grab_watershed.R, loca_clim_download_process_save.R, loca_hyd_download_process_save.R. Make sure you stop to double check the lakes you're going to extract in 'lake_list' before you start the loop that sources the following files.
    
 - *grab_watershed.R*: like the name implies, this just grabs the watershed files from Google Drive for the CMIP extraction.
    
 - *loca_clim_download_process_save.R*: this script extracts the climate files and saves them to a user-specified folder. Currently this is the Dropbox folder listed above. This iterates by netcdf file and per projection.
    
 - *loca_hyd_download_process_save.R*: this script extracts the hydrology files and saves them to a user-specified folder. Currently this is the Dropbox folder listed above. This iterates by netcdf file and per projection.
    
*2_collate_upload.R*: this script collates the extracted CMIP files from the user-specified folder and formats them into SWAT-friendly formats. 

Sourced scripts:
    
 - *cmip_functions.R*: this is a general-use script that contains functions used in many of the scripts. 


## Data citations

Acknowledgements:
    
    For CMIP5, the model output should be referred to as "the CMIP5 multi-model ensemble [archive/output/results/of simulations/dataset/ ...]". In publications, you should include a table (referred to below as Table XX) listing the models and institutions that provided model output used in your study. In this table and as appropriate in figure legends, you should use the CMIP5 official model names found in ["CMIP5 Modeling Groups and their Terms of Use"](https://pcmdi.llnl.gov/mips/cmip5/availability.html). In addition, an acknowledgment similar to the following should be included in your publication:

    "We acknowledge the World Climate Research Programme's Working Group on Coupled Modelling, which is responsible for CMIP, and we thank the climate modeling groups (listed in Table XX of this paper) for producing and making available their model output. For CMIP the U.S. Department of Energy's Program for Climate Model Diagnosis and Intercomparison provides coordinating support and led development of software infrastructure in partnership with the Global Organization for Earth System Science Portals."

    where, "Table XX" in your paper should list the models and modeling groups that provided the data you used. In addition it may be appropriate to cite one or more of the CMIP5 experiment design articles listed on the [CMIP5 reference page](https://pcmdi.llnl.gov/mips/cmip5/cmip5-references.html).

Specific citations:

    * (for LOCA CMIP5 projections) you can reference: Pierce, D. W., D. R. Cayan, and B. L. Thrasher, Statistical Downscaling Using Localized Constructed Analogs (LOCA), Journal of Hydrometeorology, 15(6), 2558-2585, 2014.; and Pierce, D. W., D. R. Cayan, E. P. Maurer, J. T. Abatzoglou, and K. C. Hegewisch, 2015: Improved bias correction techniques for hydrological simulations of climate change. J. Hydrometeorology, v. 16, p. 2421-2442. DOI: http://dx.doi.org/10.1175/JHM-D-14-0236.1.
    
    * (for LOCA CMIP5 hydrologic projections) you can reference the [technical memorandum](http://gdo-dcp.ucllnl.org/downscaled_cmip_projections/techmemo/LOCA_BCSD_hydrology_tech_memo.pdf) using the following citation: Vano, J., J. Hamman, E. Gutmann, A. Wood, N. Mizukami, M. Clark, D. W. Pierce, D. R. Cayan, C. Wobus, K. Nowak, and J. Arnold. (June 2020). Comparing Downscaled LOCA and BCSD CMIP5 Climate and Hydrology Projections - Release of Downscaled LOCA CMIP5 Hydrology. 96 p.