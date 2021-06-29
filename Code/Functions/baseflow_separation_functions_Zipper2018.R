# Examples of a bunch of methods from 
# https://github.com/samzipper/GlobalBaseflow/blob/master/src/BaseflowSeparationFunctions.R
# https://github.com/samzipper/GlobalBaseflow

#source('Code/Functions/baseflow_separation/Baseflow_Separation_Functions.R')
# packages required for sample data/examples
require(dataRetrieval)
require(lubridate)
library(tidyverse)

require(ggplot2)
require(magrittr)
require(reshape2)

baseflow_Eckhardt <- function(Q, BFImax, k){
  # R implementation of Eckhardt (2005) baseflow separation algorithm.  
  #
  # Inputs:
  #   Q = discharge timeseries (no missing data) (any units are OK)
  #   BFImax = maximum allowed value of baseflow index; recommended values are:
  #      0.8 for perennial stream with porous aquifer
  #      0.5 for ephemeral stream with porous aquifer
  #      0.25 for perennial stream with hardrock aquifer
  #   k = recession constant; this can be estimated with the function baseflow_RecessionConstant.
  #       
  # Output:
  #   bf = baseflow timeseries, same length and units as Q
  
  # empty output vector
  bf <- rep(NaN, length(Q))
  
  # fill in initial value
  bf[1] <- Q[1]*BFImax*0.9  # from Barlow 'Digital Filters' document
  
  # scroll through remaining values
  for (i in 2:length(Q)){
    # calculate bf using digital filter
    bf[i] <- (((1-BFImax)*k*bf[i-1]) + ((1-k)*BFImax*Q[i]))/(1-k*BFImax)
    
    # make sure 0 <= bf <= Q
    if (bf[i]<0)    bf[i] <- Q[i]*BFImax*0.9  # from Barlow 'Digital Filters' document
    if (bf[i]>Q[i]) bf[i] <- Q[i]
  }
  
  return(bf)
  
}

baseflow_RecessionConstant <- function(Q, UB_prc=0.95, method="Brutsaert", min_pairs=50){
  # Script to estimate baseflow recession constant.
  #
  # Inputs:
  #   Q = discharge timeseries (no missing data) (any units are OK)
  #   UB_prc = percentile to use for upper bound of regression
  #   method = method to use to calculate recession coefficient
  #     "Langbein" = Langbein (1938) as described in Eckhardt (2008)
  #     "Brutsaert" = Brutsaert (2008) WRR
  #   min_pairs = minimum number of date pairs retained after filtering out 
  #     quickflow events; 50 is from van Dijk (2010) HESS
  #       
  # Output:
  #   k = recession constant
  
  ## package dependencies
  require(quantreg)  # used for quantile regression
  
  if (method=="Langbein"){
    # calculate difference
    dQ_dt = c(NaN, diff(Q))
    
    # find days of five consecutive negative values
    which_negative <- which(dQ_dt < 0 & Q > 0)
    which_positive <- which(dQ_dt >= 0)
    which_positive_with_buffer <- unique(c(which_positive-2, which_positive-1,
                                           which_positive, 
                                           which_positive+1, which_positive+2))  # 2 days before and 2 days after a positive or 0 value
    which_positive_with_buffer <- which_positive_with_buffer[which_positive_with_buffer > 0]  # get rid of negative indices
    which_keep <- which_negative[!(which_negative %in% which_positive_with_buffer)]  # get rid of points within buffer around flow increases
    which_keep <- which_keep[(which_keep-1) %in% which_keep]  # trim to dates with both the current and previous day retained
    
    # any data exist to fit?
    if (length(which_keep) >= min_pairs){
      
      # fit regression
      fit.qr <- rq(Q[which_keep] ~ 0+Q[which_keep-1], tau=UB_prc)  # force intercept to go through origin
      
      # extract constant
      k <- as.numeric(coef(fit.qr)[1])
      
    } else {
      k <- NaN
    }
    return(k)
  }
  
  if (method=="Brutsaert"){
    # calculate lagged difference (dQ/dt) based on before/after point
    dQ_dt <- c(NaN, diff(Q, lag=2)/2, NaN)
    dQ_dt_left <- c(NaN, diff(Q))
    
    # screen data for which dQ_dt to calculate recession, based on rules in Brutsaert (2008) WRR Section 3.2
    which_negative <- which(dQ_dt < 0 & dQ_dt_left < 0 & Q > 0)
    which_positive <- which(dQ_dt >= 0)
    which_positive_with_buffer <- unique(c(which_positive-2, which_positive-1, which_positive,
                                           which_positive+1, which_positive+2, which_positive+3))  # 2 days before and 3 days after a positive or 0 value
    which_positive_with_buffer <- which_positive_with_buffer[which_positive_with_buffer > 0]  # get rid of negative indices; possible because of 2 days before
    which_keep <- which_negative[!(which_negative %in% which_positive_with_buffer)]  # get rid of points within buffer around flow increases
    which_keep <- which_keep[(which_keep-1) %in% which_keep]  # trim to dates with both the current and previous day retained
    
    # any data exist to fit?
    if (length(which_keep) >= min_pairs){
      
      # fit regression
      fit.qr <- rq(Q[which_keep] ~ 0+Q[which_keep-1], tau=UB_prc)  # force intercept to go through origin
      
      # extract constant
      k <- as.numeric(coef(fit.qr)[1])
      
    } else {
      k <- NaN
    }
    return(k)
  }
  
  
}

baseflow_BFImax <- function(Q, k){
  # Estimate BFImax parameter for Eckhardt baseflow separation filter
  # using a backwards-looking filter, based on Collischonn & Fan (2013).
  #
  # Inputs:
  #   Q = discharge timeseries (no missing data) (any units are OK)
  #   k = recession constant; this can be estimated with the function baseflow_RecessionConstant.
  #
  # Outputs:
  #   BFImax = maximum allowed value of baseflow index; Eckhardt estimates values of:
  #      0.8 for perennial stream with porous aquifer
  #      0.5 for ephemeral stream with porous aquifer
  #      0.25 for perennial stream with hardrock aquifer
  #    based on a few streams in eastern US
  
  # start from end of timeseries
  bf <- rep(NaN, length(Q))
  bf[length(Q)] <- Q[length(Q)]
  for (i in (length(Q)-1):1){
    if (bf[i+1]==0){
      bf[i] <- Q[i]
    } else {
      bf[i] <- bf[i+1]/k
    }
    
    # make sure bf isn't > Q
    if (bf[i]>Q[i]) bf[i] <- Q[i]
  }
  
  BFImax <- sum(bf)/sum(Q)
  return(BFImax)
  
}





get_eckhardt_bf <- function(siteno, discharge.df) {

  dv <- discharge.df 
  area_mi2 = readNWISsite(siteno)$drain_area_va

# Test with daily
# dv = dv %>% group_by(date = as.Date(date)) %>%
#   summarise(discharge = mean(discharge, na.rm = T))


# estimate recession constant
# The recession constant describes the rate at which baseflow recedes between storm events. 
# It is defined as the ratio of baseflow at the current time, to the baseflow one day earlier.
k <- baseflow_RecessionConstant(dv$MovingAverage_dis_cms, UB_prc=0.95, method="Brutsaert", min_pairs=30); k
BFImax <- baseflow_BFImax(Q=dv$MovingAverage_dis_cms, k=k)

## perform baseflow separations - using eckhardt for these analyses
 # dv$HYSEP_fixed <- baseflow_HYSEP(Q = dv$MovingAverage_dis_cms, area_mi2 = area_mi2, method="fixed")
 # dv$HYSEP_slide <- baseflow_HYSEP(Q = dv$MovingAverage_dis_cms, area_mi2 = area_mi2, method="sliding")
 # dv$HYSEP_local <- baseflow_HYSEP(Q = dv$MovingAverage_dis_cms, area_mi2 = area_mi2, method="local")
 # dv$UKIH <- baseflow_UKIH(Q = dv$MovingAverage_dis_cms, endrule="B")
 # dv$BFLOW_1pass <- baseflow_BFLOW(Q = dv$MovingAverage_dis_cms, beta=0.95, passes=1)
 # dv$BFLOW_3pass <- baseflow_BFLOW(Q = dv$MovingAverage_dis_cms, beta=0.95, passes=3)
dv$Eckhardt <- baseflow_Eckhardt(Q = dv$MovingAverage_dis_cms, BFImax=BFImax, k=k)

dv.melt <- 
  dv %>% 
  subset(select = c("dateTime", "MovingAverage_dis_cms", "Eckhardt")) %>%
  #subset(select=c("dateTime", "MovingAverage_dis_cms", "HYSEP_fixed", "HYSEP_slide", "HYSEP_local", "UKIH", "BFLOW_1pass", "BFLOW_3pass", "Eckhardt")) %>% 
  melt(id=c("dateTime", "MovingAverage_dis_cms")) %>%
  rename(eckhardt = value) %>%
  mutate(threshold_peak = eckhardt + mean(dv$MovingAverage_dis_cms)) 


}


calc_bfi <- function(data) {
## Calculate BFI
# Base flow is the component of streamflow that can be attributed to ground-water discharge into streams. 
# The BFI is the ratio of base flow to total flow, expressed as a percentage.
data %>% 
  group_by(variable) %>% 
  summarize(discharge.sum = sum(MovingAverage_dis_cms),
            baseflow.sum = sum(eckhardt),
            BFI = round(baseflow.sum/discharge.sum, 2))
}
  



