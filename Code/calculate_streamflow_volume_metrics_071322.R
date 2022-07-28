# code to read in depleted flow and temperature data and calculate annual and seasonal metrics for each site, year and senario
# John Hammond 01282022
library(ggplot2)
library(dplyr)
library(scales)
library(lubridate)
library(zoo)
library(EcoHydRology)
library(foreign)
library(tidyverse)

setwd("/Users/johnhammondplease/Desktop/dana_and_sam_depletion/ApplyDepletion_SyntheticHydrographs")

flows <- list.files(pattern = ".csv")

for(i in 1:1290){
  # i = 1
  #Sys.time()
  
  setwd("/Users/johnhammondplease/Desktop/dana_and_sam_depletion/ApplyDepletion_SyntheticHydrographs")
  
  climscenario <- substr(flows[i], 10,12)
  pumpscenario <- substr(flows[i], 14,21)
  site <- substr(flows[i], 23,30)
  
  current_flows <- read.csv(flows[i])
  
  flows_and_temps <- cbind(current_flows)
  flows_and_temps$start_of_year <- ifelse(flows_and_temps$DOY == 1, 1, 0)
  flows_and_temps$year <- cumsum(flows_and_temps$start_of_year)
  
  dates <- seq(as.Date("2001/1/1"), as.Date("2001/12/31"), "days")
  months <- month(dates)
  months_50 <- c(months,months,months,months,months,months,months,months,months,months,
                 months,months,months,months,months,months,months,months,months,months,
                 months,months,months,months,months,months,months,months,months,months,
                 months,months,months,months,months,months,months,months,months,months,
                 months,months,months,months,months,months,months,months,months,months)
  
  flows_and_temps$month <- months_50
  
  flows_and_temps$season <- ifelse(flows_and_temps$month > 3 & flows_and_temps$month < 7, "spring",
                                   ifelse(flows_and_temps$month > 6 & flows_and_temps$month < 10, "summer",
                                          ifelse(flows_and_temps$month > 9 & flows_and_temps$month, "fall","winter")))
  
  
  allannualflowmetrics <- as.data.frame(matrix(data= NA, nrow = 1, ncol = 17))
  colnames(allannualflowmetrics) <- c("year","totalannualflow","lowest7dayperyear","lowest7dayperyearDOY","daysbelow0.1year","zeroflowcounts","totalbelow0.1year",
                                      "totallowflowperiods","meanlowflowperiods","maxlowflowperiods","slope7","slope14","slope30","recess7","recess14","recess30","col")
  
  allseasonalflowmetrics <- as.data.frame(matrix(data= NA, nrow = 1, ncol = 9))
  colnames(allseasonalflowmetrics) <- c("year","season","totalseasonalflow","lowest7dayperseason","lowest7dayperseasonDOY","daysbelow0.1seasonal","zeroflowcountsseasonal","totalbelow0.1seasonal","col")
  
  
  # now calculate metrics for each year
  # this loop for calculating for each column of flow (Q_cms, Q_cms_dplt1... etc)
  for(c in 1:101){
    # c = 2
    # a few metrics that have to do with different elements of the natural flow regime, focusing on low flows
    # magnitude = minimum 7 day flow, volume deficit, total annual flow
    # duration = # of days below 10% threshold
    # frequency = # of periods below 10% threshold 
    # timing = date of minimum 7 day flow, 
    # rate of change = recession constant of X days prior, slope of X days prior
    
    cols_flows <- colnames(current_flows)[3:103]

    #flows_and_temps_current <- flows_and_temps[c(2,c,c+102,207,208,209)] # for statistical temp model files
    flows_and_temps_current <- flows_and_temps[c("DOY",cols_flows[c],"year" , "month", "season")] # for physical temp model files
    colnames(flows_and_temps_current) <- c("DOY", "Q_cms",  "year" , "month", "season")
    
    # total annual flow
    
    totalannualflow <- flows_and_temps_current %>% group_by(year) %>% summarise(total_annual_flow_mm = sum( Q_cms, na.rm = TRUE))
    
    # annual minimum 7-day flow, 7 day smoothing has already been done on the percentile values
    
    lowest7dayperyear <- flows_and_temps_current %>% group_by(year) %>% summarise(min = min( Q_cms, na.rm = TRUE))
    totalannualflow$lowest7dayperyear <- lowest7dayperyear$min
    # date of the lowest 7-day flow
    
    lowest7dayperyearDOY <- flows_and_temps_current %>% group_by(year) %>% slice(which.min(Q_cms)[1])
    totalannualflow$lowest7dayperyearDOY <- lowest7dayperyearDOY$DOY
    # the annual number of days below the 5-percent threshold
    
    thresh0.1 <- quantile(flows_and_temps_current$Q_cms[1:365], 0.1, na.rm = TRUE)+0.0001# adding 0.0001 because of 0 flow occurence
    
    flows_and_temps_current$lessthan0.1 <- ifelse(flows_and_temps_current$Q_cms < thresh0.1,1,0)
    daysbelow0.1year <- flows_and_temps_current %>% group_by(year) %>% summarise(days_below_0.1_quant = sum(lessthan0.1, na.rm = TRUE))
    totalannualflow$daysbelow0.1year <- daysbelow0.1year$days_below_0.1_quant
    
    flows_and_temps_current$zero <- ifelse(flows_and_temps_current$Q_cms == 0,1,0)
    
    zeroflowcounts <- flows_and_temps_current %>% group_by(year) %>% summarise(zerocounts = sum(flows_and_temps_current$zero, na.rm = TRUE))
    totalannualflow$zeroflowcounts <- zeroflowcounts$zerocounts
    
    # total deviation below 10% thresh (missing flow)
    
    flows_and_temps_current$below0.1quantile <- ifelse(flows_and_temps_current$Q_cms<thresh0.1,thresh0.1-flows_and_temps_current$Q_cms,0)
    totalbelow0.1year <- flows_and_temps_current %>% group_by(year) %>% summarise(deficit_mm = sum(below0.1quantile, na.rm = TRUE))
    totalannualflow$totalbelow0.1year <- totalbelow0.1year$deficit_mm
    
    # frequency below 10%
    
    fillwithannualvalues <- as.data.frame(matrix(data= NA, nrow = 50, ncol = 9))
    colnames(fillwithannualvalues) <- c("totallowflowperiods","meanlowflowperiods","maxlowflowperiods","slope7","slope14","slope30","recess7","recess14","recess30")
    
    years <- 1:50
    for(y in seq_along(years)){
      #y = 1
      flows_and_temps_currentyear <- subset(flows_and_temps_current,flows_and_temps_current$year == years[y])
      try(flow.rle  <- rle(flows_and_temps_currentyear$lessthan0.1))
      try(flow.values <- as.data.frame(flow.rle$values))
      try(flow.values$lengths <- flow.rle$lengths)
      try(colnames(flow.values) <- c("value","length"))
      try(lowflowperiods <- subset(flow.values, flow.values$value == 1))
      try(totallowflowperiods <- as.numeric(length(lowflowperiods$length)))
      try(meanlengthlowflow <- as.numeric(mean(lowflowperiods$length)))
      try(maxlengthlowflow <- as.numeric(max(lowflowperiods$length)))
      
      # which is date of minimum 7 day flow? now count back X days and calculate recession   
      daytocountbackfrom <- as.numeric(totalannualflow$lowest7dayperyearDOY[y])
      
      # need if else here so if negative....
      firstday30 <- daytocountbackfrom-30
      
      try(if(firstday30>0){
        recessionperiod30 <- flows_and_temps_currentyear[firstday30:daytocountbackfrom,]
        firstday14 <- daytocountbackfrom-14
        recessionperiod14 <- flows_and_temps_currentyear[firstday14:daytocountbackfrom,]
        firstday7 <- daytocountbackfrom-7
        recessionperiod7 <- flows_and_temps_currentyear[firstday7:daytocountbackfrom,]
        recessionperiod30$Q_cms <- ifelse(recessionperiod30$Q_cms<0.00001,0.00001,recessionperiod30$Q_cms)
        recessionperiod14$Q_cms <- ifelse(recessionperiod14$Q_cms<0.00001,0.00001,recessionperiod14$Q_cms)
        recessionperiod7$Q_cms <- ifelse(recessionperiod7$Q_cms<0.00001,0.00001,recessionperiod7$Q_cms)
        
        # can use lines commented out below to limit recession calculation only to days with flow below X percentile to limit influence of peak variability on recession
        #  recessionperiod30 <- subset(recessionperiod30, recessionperiod30$Q_cms < perXthresh)
        #  recessionperiod14 <- subset(recessionperiod14, recessionperiod14$Q_cms < perXthresh)
        #  recessionperiod7 <- subset(recessionperiod7, recessionperiod7$Q_cms < perXthresh)
        
        recessionperiod30$lnq <- log(recessionperiod30$Q_cms)
        recessionperiod14$lnq <- log(recessionperiod14$Q_cms)
        recessionperiod7$lnq <- log(recessionperiod7$Q_cms)
        
        recess30 <- lm(lnq ~ DOY, data = recessionperiod30)
        recess30 <- recess30$coefficients[2]
        recess14 <- lm(lnq ~ DOY, data = recessionperiod14)
        recess14 <- recess14$coefficients[2]
        recess7 <- lm(lnq ~ DOY, data = recessionperiod7)
        recess7 <- recess7$coefficients[2]
        # rate of change
        
        slope30 <- lm(Q_cms ~ DOY, data = recessionperiod30)
        slope30 <- slope30$coefficients[2]
        slope14 <- lm(Q_cms ~ DOY, data = recessionperiod14)
        slope14 <- slope14$coefficients[2]
        slope7 <- lm(Q_cms ~ DOY, data = recessionperiod7)
        slope7 <- slope7$coefficients[2]
        
        ######### ADD MORE COLUMNS TO OUTPUT
        
      } else{
        slope30 <- NA
        slope14 <- NA
        slope7 <- NA
        recess30 <- NA
        recess14 <- NA
        recess7 <- NA
      })
      
      try(fillrows <- c(totallowflowperiods,meanlengthlowflow,maxlengthlowflow,slope7,slope14,slope30,recess7,recess14,recess30))
      try(fillwithannualvalues[y,] <- fillrows)
      
    }
    
    onecolumnresults <- cbind(totalannualflow,fillwithannualvalues)
    onecolumnresults$col <- cols_flows[c]
    
    colnames(onecolumnresults) <- c("year","totalannualflow","lowest7dayperyear","lowest7dayperyearDOY","daysbelow0.1year","zeroflowcounts","totalbelow0.1year","totallowflowperiods","meanlowflowperiods","maxlowflowperiods","slope7","slope14","slope30","recess7","recess14","recess30","col")
    
    allannualflowmetrics <- rbind(allannualflowmetrics,onecolumnresults)
    
    
  }
  
  
  
  # now calculate metrics for each year/season
  # this loop for calculating for each column of flow (Q_cms, Q_cms_dplt1... etc)
  for(c in 1:101){
    # c = 1
    # a few metrics that have to do with different elements of the natural flow regime, focusing on low flows
    # magnitude = minimum 7 day flow, volume deficit, total annual flow
    # duration = # of days below 10% threshold
    # frequency = # of periods below 10% threshold 
    # timing = date of minimum 7 day flow, 
    # rate of change = recession constant of X days prior, slope of X days prior
    
    #flows_and_temps_current <- flows_and_temps[c(2,c,c+102,207,208,209)] # for statistical temp model files
    
    cols_flows <- colnames(current_flows)[3:103]

    #flows_and_temps_current <- flows_and_temps[c(2,c,c+102,207,208,209)] # for statistical temp model files
    flows_and_temps_current <- flows_and_temps[c("DOY",cols_flows[c],"year" , "month", "season")] # for physical temp model files
    colnames(flows_and_temps_current) <- c("DOY", "Q_cms",  "year" , "month", "season")
    
    # total annual flow
    
    totalannualflow <- flows_and_temps_current %>% group_by(year, season) %>% summarise(total_annual_flow_mm = sum( Q_cms, na.rm = TRUE))
    
    # annual minimum 7-day flow, 7 day smoothing has already been done on the percentile values
    
    lowest7dayperyear <- flows_and_temps_current %>% group_by(year, season) %>% summarise(min = min( Q_cms, na.rm = TRUE))
    totalannualflow$lowest7dayperyear <- lowest7dayperyear$min
    # date of the lowest 7-day flow
    
    lowest7dayperyearDOY <- flows_and_temps_current %>% group_by(year, season) %>% slice(which.min(Q_cms)[1])
    totalannualflow$lowest7dayperyearDOY <- lowest7dayperyearDOY$DOY
    # the annual number of days below the 5-percent threshold
    
    thresh0.1 <- quantile(flows_and_temps_current$Q_cms[1:365], 0.1, na.rm = TRUE)+0.0001# adding 0.0001 because of 0 flow occurence
    
    flows_and_temps_current$lessthan0.1 <- ifelse(flows_and_temps_current$Q_cms < thresh0.1,1,0)
    daysbelow0.1year <- flows_and_temps_current %>% group_by(year, season) %>% summarise(days_below_0.1_quant = sum(lessthan0.1, na.rm = TRUE))
    totalannualflow$daysbelow0.1year <- daysbelow0.1year$days_below_0.1_quant
    
    flows_and_temps_current$zero <- ifelse(flows_and_temps_current$Q_cms == 0,1,0)
    
    zeroflowcounts <- flows_and_temps_current %>% group_by(year, season) %>% summarise(zerocounts = sum(flows_and_temps_current$zero, na.rm = TRUE))
    totalannualflow$zeroflowcounts <- zeroflowcounts$zerocounts
    
    # total deviation below 10% thresh (missing flow)
    
    flows_and_temps_current$below0.1quantile <- ifelse(flows_and_temps_current$Q_cms<thresh0.1,thresh0.1-flows_and_temps_current$Q_cms,0)
    totalbelow0.1year <- flows_and_temps_current %>% group_by(year, season) %>% summarise(deficit_mm = sum(below0.1quantile, na.rm = TRUE))
    totalannualflow$totalbelow0.1year <- totalbelow0.1year$deficit_mm
    
    onecolumnresultsseasonal <- as.data.frame(totalannualflow)
    onecolumnresultsseasonal$col <- cols_flows[c]
    
    colnames(onecolumnresultsseasonal) <- c("year","season","totalseasonalflow","lowest7dayperseason","lowest7dayperseasonDOY","daysbelow0.1seasonal","zeroflowcountsseasonal","totalbelow0.1seasonal","col")
    
    allseasonalflowmetrics <- as.data.frame(rbind(allseasonalflowmetrics,onecolumnresultsseasonal))
    
    
    
  }
  
  
  
  setwd("/Users/johnhammondplease/Desktop/dana_and_sam_depletion/outputs")
  
  allannualflowmetrics <- allannualflowmetrics[-1,]
  allannualflowmetrics$site <- site
  allannualflowmetrics$climscenario <- climscenario
  allannualflowmetrics$pumpscenario <- pumpscenario
  
  allseasonalflowmetrics <- allseasonalflowmetrics[-1,]
  allseasonalflowmetrics$site <- site
  allseasonalflowmetrics$climscenario <- climscenario
  allseasonalflowmetrics$pumpscenario <- pumpscenario
  #Sys.time()
  write.csv(allannualflowmetrics, paste0(flows[i], "_annual_flow_metrics.csv"))
  write.csv(allseasonalflowmetrics, paste0(flows[i], "_seasonal_flow_metrics.csv"))
  
}

setwd("/Users/johnhammondplease/Desktop/dana_and_sam_depletion/outputs")

all <- list.files(pattern = "_annual_flow_metrics.csv")
alldata <- do.call(rbind, lapply(all, read.csv))

seasonal <- list.files(pattern = "_seasonal_flow_metrics.csv")
seasonaldata <- do.call(rbind, lapply(seasonal, read.csv))

# rounding needed

write.csv(alldata,"all_sites_50_year_annual_flow_metrics_072322.csv")
write.csv(seasonaldata,"all_sites_50_year_seasonal_flow_metrics_072322.csv")



