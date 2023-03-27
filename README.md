# swat-p-loading
data processing scripts for SWAT P loading project


## cmip_processing folder
The scripts in this folder process the manually-downloaded CMIP LOCA climate and hydrology files. See readme in the folder for further description and workflow.

## NLDAS_PRISM folder
The scripts here collate and harmonize the NLDAS/PRISM data to create observed daily met files

## SWAT_daily_input
These scripts collate the PRISM/NLDAS data (from the NLDAS_PRISM folder) alongside the CMIP projections (from the cmip_processing folder) to create the SWAT input files necessary to run the SWAT modeling program

## swat_collate folder
These scripts collate the data from etna into primary raw files, and also perform rolling averages over the data

## SWAT_viz folder
Just one script in here that does two quick visualizations. Will tweak and devleop in the future.
