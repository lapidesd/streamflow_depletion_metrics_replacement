## Depletion_06_CompileEquilibrationTimeData.R

## load packages
library(streamDepletr)
library(lubridate)
library(tidyverse)
library(sf)
library(patchwork)
options(dplyr.summarise.inform=F)   # suppress summarize info

# directory to keep stuff that is too big for GitHub
dir_big_files <- "C:/Users/s947z036/OneDrive - University of Kansas/Research/StreamflowDepletion/DepletionMetrics_LapidesEtAl/data"


## load gage shapefile
area_thres_km2 <- 10000  # only analyze gages < 10,000 km2
sf_gages <- 
  file.path("Data", "depletion", "Gages_Depletion.gpkg") %>% 
  st_read() %>% 
  mutate(GAGE_ID = as.numeric(STAID))

## load all gages summary stats
for (i in 1:length(sf_gages$STAID)){
  usgs_id <- sf_gages$STAID[i]
  
  # load data
  gage_summary <- read_csv(file.path("Data", "depletion", paste0("DepletionFraction_", usgs_id, "_IterationSummary.csv")),
                           col_types = "cidddiid") %>% 
    replace_na(list(eq_yrs_constant = 99, eq_yrs_seasonal = 99))
  
  # compile
  if (i == 1){
    all_summary <- gage_summary
  } else {
    all_summary <- bind_rows(all_summary, gage_summary)
  }
}

# save output
write_csv(all_summary, file.path("Data", "depletion", "DepletionFraction_TimeToEquilibrium.csv"))
