## CalculateDepletion.R
# This script is intended to calculate fractional streamflow depletion (Qf) using the Glover model.
# Qf = depletion as a fraction of pumpign rate. To obtain volumetric streamflow depletion (Qs), multiple Qf
#      by the pumping rate (Qw). Qs = Qf*Qw

## load packages
library(streamDepletr)
library(lubridate)
library(tidyverse)

## times to test
n_yrs <- 50
times <- seq(1, 365*n_yrs, 1)

## define USGS gage ID
# eventually turn this into a loop over all gages
usgs_id <- "07144780"  # 8-digit USGS gage ID

## glover model inputs
gage_d  <- 500 # well-stream distance [m]
gage_S  <- 0.2 # storativity
gage_Tr <- 100  # aquifer transmissivity [m2/d]

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
