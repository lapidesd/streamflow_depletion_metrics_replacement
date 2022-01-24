## Depletion_05_Calculate+ApplyDepletion_SyntheticHydrographs_AllSites.R
# This script calculates depleted streamflow timeseries for all gages.
# It includes re-calculating the annual depletion fraction so that we can
# generate a timeseries of flow for each iteration.

## prep workspace
# load packages
library(lubridate)
library(tidyverse)
library(patchwork)
library(dataRetrieval)
options(dplyr.summarise.inform=F)   # suppress summarize info

# directory to keep stuff that is too big for GitHub
dir_big_files <- "C:/Users/samzipper/OneDrive - The University of Kansas/Research/StreamflowDepletion/DepletionMetrics_LapidesEtAl/data"

# categorical color palette from https://sashat.me/2017/01/11/list-of-20-simple-distinct-colors/
col.cat.grn <- "#3cb44b"   # green
col.cat.yel <- "#ffe119"   # yellow
col.cat.org <- "#f58231"   # orange
col.cat.red <- "#e6194b"   # red
col.cat.blu <- "#0082c8"   # blue
col.gray <- "gray65"       # gray for annotation lines, etc

# ggplot theme
windowsFonts(Arial=windowsFont("TT Arial"))
theme_scz <- function(...){
  theme_bw(base_size=10, base_family="Arial") + 
    theme(
      text=element_text(color="black"),
      plot.title=element_text(face="bold", size=rel(1)),
      axis.title=element_text(face="bold", size=rel(1)),
      axis.text=element_text(size=rel(1)),
      strip.text=element_text(size=rel(1)),
      legend.title=element_text(face="bold", size=rel(1)),
      legend.text=element_text(size=rel(1)),
      panel.grid=element_blank(),
      plot.margin=unit(c(1,1,1,1), "mm"),
      strip.background=element_blank())
}

theme_set(theme_scz())

## set up loop
# list of gages
gages <- 
  file.path(dir_big_files, "complete_years_data_q10_25_50") %>% 
  list.files(pattern = ".csv") %>% 
  str_replace(pattern = ".csv", replacement = "")
