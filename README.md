# TZCooccurrence
Data and code for Shea et al. 2022 "Pattern of tree species co-occurrence in an ecotone responds to spatially variable drivers" in Landscape Ecology. https://doi.org/10.1007/s10980-022-01485-x

Files include:

R scripts:
TZ_Cooccur_GridChange_LEFinal.R: R script file to identify the scale of co-occurrence. 
TZ_Cooccur_enviro_LEFinal.R: R script file where the environmental analysis, tests for spatial autocorrelation, and NMS are conducted, and where the Manuscript figures are created. 

Shapefiles:
Grids.zip: Zipped folder with shapefiles for each of the Grids used in the analysis (1000m, 1200m, 1500m, 2000m, 2500m, 3000m, 3750m, 4285m, 5000m, 6000m, 7500m, 30km) 
mean_FS_TZ4_PolygonZones.zip: Zipped folder with the shapefile of the TensionZone polygon
WI_HARN_mask.zip: Zipped folder with the boundary of Wisconsin 

Data files:
all_trees_pred_12Aug2019.csv: Data of witness tree observations on Wisconsin PLS survey corners and quarter corners. Includes X and Y coordinates (crs = 3070) and SP_new (which is the species identity of the witness trees, with differentiated results for the trees identified to the genus level; see Shea et al. 2021, Journal of Vegetation Science, for details)
NSpp_TZ4_FS.csv: List of species (SP_new) identified as Northern indicator species in at least one ISA run (count is the number of ISA runs) in the analysis completed in Shea et al. 2021, Journal of Vegetation Science.
SSpp_TZ4_FS.csv: List of species (SP_new) identified as Southern indicator species in at least one ISA run (count is the number of ISA runs) in the analysis completed in Shea et al. 2021, Journal of Vegetation Science.
Grid_hist_sig_scale.csv: 
elev_30km.csv:
ppet_30km.csv:
sand_30km.csv:
DI_30km.csv:
Grid_prop.csv:



