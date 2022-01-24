## Depletion_05_Calculate+ApplyDepletion_SyntheticHydrographs_AllSites.R
# This script calculates depleted streamflow timeseries for all gages.
# It includes re-calculating the annual depletion fraction so that we can
# generate a timeseries of flow for each iteration.

# prep workspace
## load packages
library(lubridate)
library(tidyverse)
library(patchwork)
library(dataRetrieval)
options(dplyr.summarise.inform=F)   # suppress summarize info

## directory to keep stuff that is too big for GitHub
dir_big_files <- "C:/Users/samzipper/OneDrive - The University of Kansas/Research/StreamflowDepletion/DepletionMetrics_LapidesEtAl/data"

## categorical color palette from https://sashat.me/2017/01/11/list-of-20-simple-distinct-colors/
col.cat.grn <- "#3cb44b"   # green
col.cat.yel <- "#ffe119"   # yellow
col.cat.org <- "#f58231"   # orange
col.cat.red <- "#e6194b"   # red
col.cat.blu <- "#0082c8"   # blue
col.gray <- "gray65"       # gray for annotation lines, etc

## ggplot theme
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

# make a subtraction function that does not allow below 0
#  NOTE: this is different from the sub0 in Depletion_04_...
sub0 <- function(df, x, y, bound){
  new <- df[,x] - y
  new[new < bound] <- bound
  return(unlist(c(new)))
}
# test: 
#  ab = tibble(a = c(3, 5), b = c(2, 6))
#  sub0(ab, "a", "b", 0.01)

# define parameters
n_iter <- 100   # number of iterations for each gage
n_yrs <- 50     # how long to simulate?
times <- seq(1, 365*n_yrs, 1) # create timeseries

## for seasonal pumping, start/stop times
times_start <- seq(yday(ymd("2021-05-01")), yday(ymd("2021-05-01"))+365*n_yrs, 365)
times_stop <- seq(yday(ymd("2021-09-30")), yday(ymd("2021-09-30"))+365*n_yrs, 365)

## column names corresponding to dry, average, and wet
dry_name <- "p10_va" # q10
avg_name <- "p50_va" # q50
wet_name <- "p90_va" # q90

bound_0 <- 0  # value that any 0 calculation should be replaced with (may set to 0.01 for log plotting)
pump_Q_fraction <- 0.05  # proportion of mean annual streamflow used for pumping rate (based on withdrawal_fractions.xlsx sheet from John)
rounddig <- 3  # number fo digits to round

# load input data
## list of gages
gages <- 
  file.path(dir_big_files, "complete_years_data_q10_25_50") %>% 
  list.files(pattern = ".csv") %>% 
  str_replace(pattern = ".csv", replacement = "")

## load porosity from GLHYMPS: Porosity = value, GLHYMPS_Shape_Area = area of each porosity value within watershed
## since GLHYMPS gives porosity, may want to reduce to represent storativity since not all porosity
## is drainable. however, de Graaf et al. 2015 HESS and 2017 ADWR use these values directly for their models.
porosity <- 
  read_csv(file.path("Data", "GLHYMPS_Porosity_by_gauges.csv")) %>% 
  subset(GAGE_ID %in% gages) %>% 
  mutate(S = Porosity)  # could apply transformation if desired (i.e., S = 50% of porosity)
## there is a separate transmissivity file for each gage, load it within loop below

## load well-stream distances
jsko_files <- list.files(file.path("Data", "JasechkoEtAl_Fig2Data"), pattern = ".csv")
for (f in jsko_files){
  # compile all individual states into one data frame
  state <- gsub(".csv", "", f)
  jsko_state_data <- 
    file.path("Data", "JasechkoEtAl_Fig2Data", f) %>% 
    read_csv() %>% 
    mutate(State = state)
  
  if (f == jsko_files[1]){
    jsko_all_states <- jsko_state_data
  } else {
    jsko_all_states <- bind_rows(jsko_all_states, jsko_state_data)
  }
}

# loop through all gages
for (usgs_id in gages){
  ## build glover model inputs for each iteration sample
  # Well-stream distance [m] - randomly sample from a distribution of all known data for uncertainty analysis.
  set.seed(1)
  gage_d_sample <- sample(jsko_all_states$NearestFlowlineDistance_m, size = n_iter)
  
  # Storativity [-]
  porosity_gage <- subset(porosity, GAGE_ID == usgs_id)
  gage_S_sample <- sample(porosity_gage$S, size = n_iter, replace = T, prob = porosity_gage$GLHYMPS_Shape_Area)
  
  # Aquifer transmissivity [m2/d]
  df_Tr_gage <- 
    file.path("Data", "transmissivity_data", paste0(usgs_id, ".csv")) %>% 
    read_csv(col_types = cols()) %>% 
    subset(complete.cases(.))
  names(df_Tr_gage) <- "Tr_m2d"
  
  if (dim(df_Tr_gage)[1] < n_iter){
    gage_Tr_sample <- sample(df_Tr_gage$Tr_m2d, size = n_iter, replace = T)
  } else {
    gage_Tr_sample <- sample(df_Tr_gage$Tr_m2d, size = n_iter, replace = F)
  }
  
  ## load hydrographs
  # load historic streamflow percentiles - prepared by John
  daily_streamflow <- 
    file.path(dir_big_files, "complete_years_data_q10_25_50", paste0(usgs_id, ".csv")) %>% 
    read_csv(col_types = cols()) %>% 
    subset(!(month_nu == 2 & day_nu == 29)) %>%  # leap year data averages over a different period - eliminate
    mutate(DOY = seq(1, 365))
  
  # create dry/avg/wet data frames
  daily_flow <- 
    daily_streamflow %>% 
    dplyr::select(DOY, all_of(c(dry_name, avg_name, wet_name))) %>% 
    set_names("DOY", "Q_dry", "Q_avg", "Q_wet")
  
  Q_dry_constant <- 
    Q_dry_seasonal <- 
    tibble(Day = seq(1, 365*50),
           DOY = rep(seq(1, 365), 50),
           Q_cms = round(rep(daily_flow$Q_dry, 50), rounddig))
  
  Q_avg_constant <- 
    Q_avg_seasonal <- 
    tibble(Day = seq(1, 365*50),
           DOY = rep(seq(1, 365), 50),
           Q_cms = round(rep(daily_flow$Q_avg, 50), rounddig))
  
  Q_wet_constant <- 
    Q_wet_seasonal <- 
    tibble(Day = seq(1, 365*50),
           DOY = rep(seq(1, 365), 50),
           Q_cms = round(rep(daily_flow$Q_wet, 50), rounddig))
  
  # set pump rate
  pump_rate_cms <- mean(daily_flow$Q_avg)*pump_Q_fraction  # pump rate for constant pumping
  pump_vol_m3 <- pump_rate_cms*365*24*60*60
  seasonal_pump_days <- length(seq(yday(ymd("2021-05-01")), yday(ymd("2021-09-30"))))
  pump_rate_cms_seasonal <- pump_vol_m3/(seasonal_pump_days*24*60*60)
  
  ## loop through iterations and calculate depletion
  for (n in 1:n_iter){
    ## get parameters for this sample
    gage_d <- round(gage_d_sample[n], 3)
    gage_S <- round(gage_S_sample[n], 3)
    gage_Tr <- gage_Tr_sample[n]
    
    ## calculate depletion
    # constant pumping
    Qf_constant <- glover(t = times, d = gage_d, S = gage_S, Tr = gage_Tr)
    Qs_constant <- Qf_constant*pump_rate_cms
    # plot(times, Qf_constant)
    
    # seasonal pumping (may-sept)
    Qf_seasonal <- intermittent_pumping(t = times, starts = times_start, stops = times_stop, rates = 1, 
                                        method = "glover", d = gage_d, S = gage_S, Tr = gage_Tr)
    Qs_seasonal <- Qf_seasonal*pump_rate_cms_seasonal
    # plot(times, Qf_seasonal)
    
    Q_dry_constant[,paste0("Q_cms_dplt", n)] <- round(sub0(Q_dry_constant, "Q_cms", Qs_constant, bound = 0), rounddig)
    Q_dry_seasonal[,paste0("Q_cms_dplt", n)] <- round(sub0(Q_dry_seasonal, "Q_cms", Qs_seasonal, bound = 0), rounddig)
    
    Q_avg_constant[,paste0("Q_cms_dplt", n)] <- round(sub0(Q_avg_constant, "Q_cms", Qs_constant, bound = 0), rounddig)
    Q_avg_seasonal[,paste0("Q_cms_dplt", n)] <- round(sub0(Q_avg_seasonal, "Q_cms", Qs_seasonal, bound = 0), rounddig)
    
    Q_wet_constant[,paste0("Q_cms_dplt", n)] <- round(sub0(Q_wet_constant, "Q_cms", Qs_constant, bound = 0), rounddig)
    Q_wet_seasonal[,paste0("Q_cms_dplt", n)] <- round(sub0(Q_wet_seasonal, "Q_cms", Qs_seasonal, bound = 0), rounddig)
    
  }
  
  # write output
  write_csv(Q_dry_constant, file.path(dir_big_files, "ApplyDepletion_SyntheticHydrographs", paste0("DepletedQ_Dry_Constant_", usgs_id, ".csv")))
  write_csv(Q_dry_seasonal, file.path(dir_big_files, "ApplyDepletion_SyntheticHydrographs", paste0("DepletedQ_Dry_Seasonal_", usgs_id, ".csv")))
  
  write_csv(Q_avg_constant, file.path(dir_big_files, "ApplyDepletion_SyntheticHydrographs", paste0("DepletedQ_Avg_Constant_", usgs_id, ".csv")))
  write_csv(Q_avg_seasonal, file.path(dir_big_files, "ApplyDepletion_SyntheticHydrographs", paste0("DepletedQ_Avg_Seasonal_", usgs_id, ".csv")))
  
  write_csv(Q_wet_constant, file.path(dir_big_files, "ApplyDepletion_SyntheticHydrographs", paste0("DepletedQ_Wet_Constant_", usgs_id, ".csv")))
  write_csv(Q_wet_seasonal, file.path(dir_big_files, "ApplyDepletion_SyntheticHydrographs", paste0("DepletedQ_Wet_Seasonal_", usgs_id, ".csv")))
  
}