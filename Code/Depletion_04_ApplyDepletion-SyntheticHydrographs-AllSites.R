## Depletion_04_ApplyDepletion_SyntheticHydrographs_AllSites.R
# This script calculates depleted streamflow for all gages.
# It is illustrated for 1 gage in ApplyDepletion_SyntheticHydrographs.Rmd
# This will be run for all sites to determine the appropriate pumping volume.
# Then, a separate script will produce the output of depleted flow time series.

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

## some parameters
bound_0 <- 0  # value that any 0 calculation should be replaced with (may set to 0.01 for log plotting)
pump_Q_fraction <- 0.05  # proportion of mean annual streamflow used for pumping rate (based on withdrawal_fractions.xlsx sheet from John)
site_output <- F  # write output for each site?

# loop through gages
for (usgs_id in gages){
  #usgs_id <- gages[1]
  
  ## load data
  # load calculated depletion
  daily_depletion_summary <- 
    file.path(dir_big_files, paste0("DepletionFraction_", usgs_id, "_DailySummary.csv")) %>% 
    read_csv(col_types = cols()) %>% 
    mutate(Year = ceiling(day/365),
           DOY = day-(Year-1)*365)
  
  # load historic streamflow percentiles - prepared by John
  daily_streamflow <- 
    file.path(dir_big_files, "complete_years_data_q10_25_50", paste0(usgs_id, ".csv")) %>% 
    read_csv(col_types = cols()) %>% 
    mutate(DOY = seq(1, 365))
  
  ## select dry, avg, wet year
  # column names corresponding to dry, average, and wet
  dry_name <- "p10_va" # q10
  avg_name <- "p50_va" # q50
  wet_name <- "p90_va" # q90
  
  # select and organize into smaller data frame
  daily_DAW <- 
    daily_streamflow %>% 
    dplyr::select(DOY, all_of(c(dry_name, avg_name, wet_name))) %>% 
    set_names("DOY", "Q_dry", "Q_avg", "Q_wet")
  
  ## join streamflow with depletion summary
  daily_depletion_with_Q <- left_join(daily_depletion_summary, daily_DAW, by = "DOY")
  
  # inspect to make sure years are repeated correctly
  #ggplot(subset(daily_depletion_with_Q, Year <= 5), aes(x = day, y = Q_dry)) + geom_line()
  
  ## set pump rate
  pump_rate_cms <- mean(daily_DAW$Q_avg)*pump_Q_fraction  # pump rate for constant pumping
  pump_vol_m3 <- pump_rate_cms*365*24*60*60
  seasonal_pump_days <- length(seq(yday(ymd("2021-05-01")), yday(ymd("2021-09-30"))))
  pump_rate_cms_seasonal <- pump_vol_m3/(seasonal_pump_days*24*60*60)
  
  # make a data frame so you can join it with the depletion data frame
  # seasonal pump rate should be for every day since you will multiply by Qf (even when pumping is shut off)
  df_pump_rate <- tibble(pumping = c("constant", "seasonal"),
                         Qw_cms = c(pump_rate_cms, pump_rate_cms_seasonal))
  
  daily_depletion_with_Qs <- 
    left_join(daily_depletion_with_Q, df_pump_rate, by = c("pumping")) %>% 
    mutate(Qsmean = Qfmean*Qw_cms,
           Qssd = Qfsd*Qw_cms,
           Qsmin = Qfmin*Qw_cms,
           Qs5 = Qf5*Qw_cms,
           Qs25 = Qf25*Qw_cms,
           Qs50 = Qf50*Qw_cms,
           Qs75 = Qf75*Qw_cms,
           Qs95 = Qf95*Qw_cms,
           Qsmax = Qfmax*Qw_cms)
  
  # make a subtraction function that does not allow below 0
  sub0 <- function(df, x, y, bound){
    new <- df[,x] - df[,y]
    new[new < bound] <- bound
    return(unlist(c(new)))
  }
  # test: 
  #  ab = tibble(a = c(3, 5), b = c(2, 6))
  #  sub0(ab, "a", "b", 0.01)
  
  # calculate depleted streamflow
  Qs_options <- names(daily_depletion_with_Qs)[str_detect(names(daily_depletion_with_Qs), "Qs")]
  Q_options <- names(daily_depletion_with_Qs)[str_detect(names(daily_depletion_with_Qs), "Q_")]
  
  for (Q in Q_options){
    for (Qs in Qs_options){
      varname <- paste0(Q, "_sub", Qs)
      daily_depletion_with_Qs$new <- sub0(daily_depletion_with_Qs, Q, Qs, bound = bound_0)
      names(daily_depletion_with_Qs)[names(daily_depletion_with_Qs) == "new"] <- varname
    }
  }
  
  ## subset to just data you want to save
  df_depleted_flow <-
    daily_depletion_with_Qs %>% 
    dplyr::select(day, pumping, 
                  Q_dry, Q_dry_subQs5, Q_dry_subQs25, Q_dry_subQs50, Q_dry_subQs75,Q_dry_subQs95,
                  Q_avg, Q_avg_subQs5, Q_avg_subQs25, Q_avg_subQs50, Q_avg_subQs75,Q_avg_subQs95,
                  Q_wet, Q_wet_subQs5, Q_wet_subQs25, Q_wet_subQs50, Q_wet_subQs75,Q_wet_subQs95)
  df_depleted_flow[, 3:dim(df_depleted_flow)[2]] <-
    round(df_depleted_flow[, 3:dim(df_depleted_flow)[2]], 3)
  
  if (site_output){
    write_csv(df_depleted_flow, file.path(dir_big_files, "ApplyDepletion_SyntheticHydrographs", paste0("DepletedFlow_", usgs_id, ".csv")))
  }
  
  ## summary statistics
  # want to know: day for first 0-flow, percent of days with 0-flow
  df_site_summary <- 
    tibble(usgs_id = usgs_id,
           year = rep(c("dry", "avg", "wet"), each = 4),
           pumping = rep(c("constant", "constant", "seasonal", "seasonal"), times = 3),
           metric = rep(c("first0day", "prc0"), times = 6),
           value = c(min(subset(df_depleted_flow, pumping == "constant" & Q_dry_subQs50 == bound_0)$day),
                     length(subset(df_depleted_flow, pumping == "constant" & Q_dry_subQs50 == bound_0)$day)/max(df_depleted_flow$day),
                     min(subset(df_depleted_flow, pumping == "seasonal" & Q_dry_subQs50 == bound_0)$day),
                     length(subset(df_depleted_flow, pumping == "seasonal" & Q_dry_subQs50 == bound_0)$day)/max(df_depleted_flow$day),
                     min(subset(df_depleted_flow, pumping == "constant" & Q_avg_subQs50 == bound_0)$day),
                     length(subset(df_depleted_flow, pumping == "constant" & Q_avg_subQs50 == bound_0)$day)/max(df_depleted_flow$day),
                     min(subset(df_depleted_flow, pumping == "seasonal" & Q_avg_subQs50 == bound_0)$day),
                     length(subset(df_depleted_flow, pumping == "seasonal" & Q_avg_subQs50 == bound_0)$day)/max(df_depleted_flow$day),
                     min(subset(df_depleted_flow, pumping == "constant" & Q_wet_subQs50 == bound_0)$day),
                     length(subset(df_depleted_flow, pumping == "constant" & Q_wet_subQs50 == bound_0)$day)/max(df_depleted_flow$day),
                     min(subset(df_depleted_flow, pumping == "seasonal" & Q_wet_subQs50 == bound_0)$day),
                     length(subset(df_depleted_flow, pumping == "seasonal" & Q_wet_subQs50 == bound_0)$day)/max(df_depleted_flow$day))
    )
  
  if (usgs_id == gages[1]){
    df_summary <- df_site_summary
  } else {
    df_summary <- bind_rows(df_summary, df_site_summary)
  }
  
  print(paste0(which(gages == usgs_id), " of ", length(gages), " complete, ", Sys.time()))
  
}

# save summary
df_summary$year <- factor(df_summary$year, levels = c("dry", "avg", "wet"))
write_csv(df_summary, file.path("Data", "Depletion", "ApplyDepletion_SyntheticHydrographs_Summary.csv"))

# plot summary
p_summary_prc0 <-
  df_summary %>% 
  subset(metric == "prc0") %>% 
  ggplot(aes(x = value)) +
  geom_histogram(breaks = seq(0, 1, 0.1)) +
  facet_grid(pumping ~ year) +
  scale_x_continuous(name = "Percent of 50 year simulation with 0 flow") +
  scale_y_continuous(name = "Number of gages") +
  labs(title = "Time gages are dry for median depletion scenario",
       subtitle = paste0("Pumping at ", 100*pump_Q_fraction, "% mean annual flow"))

p_summary_first0day <-
  df_summary %>% 
  subset(metric == "first0day") %>% 
  ggplot(aes(x = value)) +
  geom_histogram(breaks = seq(1, 36501, 365)) +
  facet_grid(pumping ~ year) +
  scale_x_continuous(name = "Day of simulation with first 0 flow") +
  scale_y_continuous(name = "Number of gages") +
  labs(title = "Timing of drying for median depletion scenario",
       subtitle = paste0("Pumping at ", 100*pump_Q_fraction, "% mean annual flow"))

p_combo <-
  (p_summary_prc0 + p_summary_first0day) +
  plot_layout(ncol = 1)
ggsave(file.path("Figures", paste0("ApplyDepletion_SyntheticHydrographs_Summary_pump", 100*pump_Q_fraction, "Qmean.png")),
       p_combo, width = 190, height = 300, units = "mm")
