# streamflow_depletion_metrics

Changing precipitation patterns and increasing human reliance on groundwater resources drive changes in streamflow patterns and persistence. 
While streamflow depletion has wide-ranging effects for aquatic ecosystems and human populations, detecting and predicting the impacts of
streamflow depletion can be very difficult. Several low-flow metrics have been used in management to assess potential impacts of groundwater
withdrawals on streamflow, but the ecohydrological indicators most sensitive to groundwater withdrawals have not yet been identified. The code 
and data included in this repository constitute a comprehensive exploration of how stream hydrographs and thermographs respond to 
groundwater withdrawals, as measured by a suite of ecohydrological indicators. For a full description of methods and results, see 
REFERENCE TO UNPUBLISHED STUDY.

Programs included in the 'Code' directory of this repository:

- find_stations.ipynb: a jupyter notebook that identifies the set of GAGESII stations with continuous long-term stream temperature monitoring.

- Depletion_01_CalculateDepletionFraction.Rmd: R script that calculates fractional streamflow depletion using the Glover model

- Depletion_02_VisualizeDepletion.Rmd: R script to visualize output from Depletion_01_CalculateDepletionFraction.Rmd

- Depletion_03_ApplyDepletion-SyntheticHydrographs-TestSite.Rmd: R script to apply fraction streamflow depletion calculated in Depletion_01_CalculateDepletionFraction.Rmd to a long-term streamflow record.

- Depletion_04_ApplyDepletion-SyntheticHydrographs-AllSites.R: R script to calculate depleted streamflow for an example sites with parameter uncertainty

- Depletion_05_Calculate+ApplyDepletion-SyntheticHydrographs-AllSites.R: R script to calculate depleted streamflow at all study sites with parameter uncertainty

- Depletion_06_CompileEquilibrationTimeData.R: R script to provide information about time to equilibrium for depletionmodel with different parameter combinations

- aquifer_params.ipynb: a jupyter notebook that ingests data sources and estimates aquifer parameters for study sites

- calculate_streamflow_volume_metrics_071322.R: an R script that calculates streamflow metrics for all sites for measured and synthetically depleted timeseries

- metric_outcomes_publish.ipynb: a jupyter notebook that reads in calculated metrics on measured and synthetically depleted timeseries and generates manuscript figures

Data included in the 'Data' directory of this repository:

- end_member_performance1.csv: End-member mixing model performance at each GAGESII station with at least 15 years of continuous temperature data. Performance is measured by NSE, KGE, RMSE, and pbias.

- gridmet_tempsites: a directory containing Gridmet data at each GAGESII station with at least 15 years of continuous temperature data.

     - gridmet_Tmin_C_15yr_temp_gages.csv
     - gridmet_Tmax_C_15yr_temp_gages.csv
     - gridmet_P_mm_15yr_temp_gages.csv

- gagesII_data_and_subsets: GAGESII metadata and subset information for stations with stream temperature data

     - record_length_yrs.csv: A table including the length of continuous tream temperature record at each station.
     - gagesII.shp: A metadata shapefile for GAGESII sites
     - gagesII_15yr_temperature.csv: A table of GAGESII stations with at least 15 years of continuous stream temperature data.
     - gagesII_10yr_temperature.csv: A table of GAGESII stations with at least 10 years of continuous stream temperature data.
     - candidate_site_list.pkl: A pickle file of GAGESII stations that potentially have 15 years of stream temperature data.

- maps_and_boundaries: a directory of maps used for plotting in jupyter notebooks

     - US_state_outline: directory containing a shapfile of the United States with states delineated



# References
Abatzoglou, J. T. (2013), Development of gridded surface meteorological data for ecological applications and modelling. Int. J. Climatol., 33: 121–131.

Eckhardt, Klaus. "How to construct recursive digital filters for baseflow separation." Hydrological Processes: An International Journal 19.2 (2005): 507-515.
