## CalculateDepletion.R
# This script is intended to calculate fractional streamflow depletion (Qf) using the Glover model.
# Qf = depletion as a fraction of pumping rate. To obtain volumetric streamflow depletion (Qs), multiple Qf
#      by the pumping rate (Qw). Qs = Qf*Qw

## load packages
library(streamDepletr)
library(lubridate)
library(tidyverse)

## times to test
n_yrs <- 50
times <- seq(1, 365*n_yrs, 1)

## load aquifer characteristics
aquifer_params <- 
  read_csv(file.path("Data", "GAGESII_aquifer_params.csv")) %>% 
  unique()

# plot and inspect
ggplot(aquifer_params, aes(x = Porosity)) +
  geom_histogram()

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

# get median by state and overall
median_dist_m <- median(jsko_all_states$NearestFlowlineDistance_m)
jsko_all_states %>% 
  group_by(State) %>% 
  summarize(median_dist_m = median(NearestFlowlineDistance_m))

# plot distribution
ggplot(jsko_all_states, aes(x = NearestFlowlineDistance_m)) +
  geom_histogram(aes(fill = State), binwidth = 100) +
  theme_bw() 

ggplot(jsko_all_states, aes(x = State, y = NearestFlowlineDistance_m)) +
  geom_boxplot(aes(fill = State)) +
  geom_hline(yintercept = median_dist_m, color = "red") +
  theme_bw()

## define USGS gage ID
# eventually turn this into a loop over all gages
usgs_id <- "07144780"  # 8-digit USGS gage ID

## glover model inputs
gage_d  <- median_dist_m # well-stream distance [m]
    # this is the median well-stream distance from all wells in 12 states with data shared from Jasechko et al.
    # the median of all wells is 318 m. the lowest state median is KY (246 m) and the highest is TX (437 m).
    # could use the data to randomly sample from a distribution for uncertainty analysis.

gage_S  <- aquifer_params$Porosity[aquifer_params$STAID == usgs_id] # storativity
    # since GLHYMPS gives porosity, may want to reduce to represent storativity since not all porosity
    # is drainable. however, de Graaf et al. 2015 HESS and 2017 ADWR use these values directly for their models.

gage_Tr <- aquifer_params$Transmissivity[aquifer_params$STAID == usgs_id]  # aquifer transmissivity [m2/d]
    # these values seem too high... need to check with Dana about units.

## calculate depletion
# constant pumping
Qf_constant <- glover(t = times, d = gage_d, S = gage_S, Tr = gage_Tr)

# seasonal pumping (may-sept)
times_start <- seq(yday(ymd("2021-05-01")), yday(ymd("2021-05-01"))+365*n_yrs, 365)
times_stop <- seq(yday(ymd("2021-09-30")), yday(ymd("2021-09-30"))+365*n_yrs, 365)
Qf_seasonal <- intermittent_pumping(t = times, starts = times_start, stops = times_stop, rates = 1, 
                                    method = "glover", d = gage_d, S = gage_S, Tr = gage_Tr)

## summarize data
daily_depletion <- tibble(day = times,
                          year = rep(seq(1, n_yrs, 1), each = 365),
                          DOY = rep(seq(1, 365, 1), times = n_yrs),
                          Qf_constant = Qf_constant,
                          Qf_seasonal = Qf_seasonal)

## calculate annual depletion
annual_depletion <- 
  daily_depletion %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarize(Qf_constant_sum = sum(Qf_constant),
                   Qf_seasonal_sum = sum(Qf_seasonal))

## figure out years to dynamic equilibrium
eq_thres <- 0.01  # 1% change in annual depletion volume
prc_change_constant <- c(NA, diff(annual_depletion$Qf_constant_sum))/annual_depletion$Qf_constant_sum
prc_change_seasonal <- c(NA, diff(annual_depletion$Qf_seasonal_sum))/annual_depletion$Qf_seasonal_sum
eq_yrs_constant <- min(annual_depletion$year[which(prc_change_constant < eq_thres)])
eq_yrs_seasonal <- min(annual_depletion$year[which(prc_change_seasonal < eq_thres)])
