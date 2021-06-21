#code modified from:Gorski, G. (2020). EventPicker. GitHub Repository, https://github.com/galengorski/EventPicker 
library(tidyverse)

#df is a dataframe with baseflow and threshold calculated already 
 timestamp <- 'date'
 plot_var <- 'Daily_dis_cms'
 sb_pk_thresh <- 0.000001
 sf_pk_thresh <- 0
#cond_data is the EC timeseries for the site or cl_ts_data is the chloride timeseries calculated from the cond
source("Data/USGS_discharge/call_discharge_datasets.R")

df.orig = DC_discharge

#the find.peaks function should ingest a dataframe and output a dataframe with the peaks idenitified
find.peaks <- function(df.orig, timestamp, plot_var, sb_pk_thresh, sf_pk_thresh, cl_ts_data){
  df <- df.orig %>%
    group_by(as.Date(dateTime, format = "%m/%d/%y")) %>%
    summarise(mean(RunningMean_dis_cms)) %>%
    rename(date = 'as.Date(dateTime, format = "%m/%d/%y")',
           Daily_dis_cms = 'mean(RunningMean_dis_cms)',
           value = 'mean(value)',
           threshold_peak = 'mean(threshold_peak)') %>%
    ungroup() %>%
    as.data.frame() %>%
    mutate(threshold_peak = value + (mean(Daily_dis_cms))/2)
  
  
  #convert to seconds
  #df$Date <- as.POSIXct(df$Date)
  df$Seconds <- as.numeric(unlist(df[,timestamp]))
  ##-remove NAs
  #df <- df[!is.na(df[,plot_var]),]
  ##-make sure plotting variable is greater than zero
  #df <- df[df[,plot_var] > 0,]
  ##-create an empty column for the slope forward and backward
  df$slp.b <- rep(NA, length.out = nrow(df))
  df$slp.f <- rep(NA, length.out = nrow(df))
  
  for(i in 2:(nrow(df)-1)){
    ##-calculate the slope back one timestep
    df$slp.b[i] <- (df[,plot_var][i]-df[,plot_var][(i-1)])/(df$Seconds[i]-df$Seconds[(i-1)])
    ##-calculate the slope forward one timestep
    df$slp.f[i] <- (df[,plot_var][(i+1)]-df[,plot_var][i])/(df$Seconds[(i+1)]-df$Seconds[i])
  }
  
  ##-make a column for the peak of each event flagged by a change in derivative
  df$peak.flag <- rep(NA, length.out = nrow(df))
  
  ##-now flag those derivative changes
  for(i in 2:(nrow(df)-1)){
    ##-if the slope back is greater than some threshold and the slope forward is negative, flag it as a peak
    ##-default sb_pk_thresh 0.000001
    ##-default sf_pk_thresh 0
    if(df$slp.b[i]>sb_pk_thresh & df$slp.f[i]<sf_pk_thresh){
      df$peak.flag[i] <- 1
    }else{
      ##-otherwise don't
      df$peak.flag[i] <- 0
    }
  }
  
  df <- df %>%
    mutate(threshold = mean(df$Daily_dis_cms))
  
  ggplot() +
    geom_line(df, mapping = aes(date, Daily_dis_cms)) +
    geom_point(df%>%filter(peak.flag == 1), mapping = aes(date, Daily_dis_cms), color = "red") +
    geom_line(df, mapping = aes(date, threshold_peak),color = "blue")
  
  df <- df %>%
    mutate(peak.flag = ifelse(peak.flag == 1 & Daily_dis_cms > threshold_peak, 1, 0)) 
  
  
  
  ##-Flag the changes in derivatitives, events is the row of single site which have events
  events <- which(df$peak.flag == 1)
  ##-if there are no events return df
  if(length(events) == 0){
    return(df)
  }else{
    ##-within single site, make a column that will signal which observations belong to which peak
    ##-this will turn into the rising and falling limbs
    df$event.flag <- rep(NA, length.out = nrow(df))
    
    for(i in 1:length(events)){
      k <- events[i]
      ##-the while loop goes forward in time until the slope forward is no longer negative
      while(df$slp.f[k] <0){
        ##-and labels that with the event number (i) and makes it positive (+)
        df$event.flag[k] <- i
        k <- k +1
        
        ##-if the last row of single sites is an event peak then move on
        if(k == nrow(df)){
          break
        }else{
        }
      }
      
      ##-now step backward in time for the falling limb
      ##-starting with the point directly before the peak
      j <- events[i]-1
      ##-if it's the first two data points in the site don't bother
      if(j == 1|j == 0){
        next
      }else{
        ##-as you step backwards label the days with the event number (i) and make it negative (-)
        ##-this time to indicate it is the rising limb (before the peak)
        while(df$slp.b[j] > 0){
          df$event.flag[j] <- -1*i
          ##-label j-1 as part of the rising limb too because j-1 won't be grouped with the rising limb
          ##-if it has a negative slope on the next step going back
          df$event.flag[j-1] <- -1*i
          j <- j - 1
          ##-if i is 1,2 or 3 in the data frame forget about it
          if(j == 2| j == 1| j == 0){
            break
          }else{
          }
        }
      }
    }
  }
  
  df1 <- df.orig %>%
    rename(DATE = date) %>%
    mutate(date = as.Date(DATE)) %>%
    left_join(df, by = "date") %>%
    rename(dummy = date) %>%
    rename(date = DATE)
  
  
  df_combine <- df1 %>%
    left_join(cl_ts_data)
  
  df_peaks <- df_combine %>%
    drop_na(event.flag) %>%
    rename(event_flow = runningmeandis.x,
           event_cond = runningmean,
           event_cl = chloride_predict)
  
  df_baseflow_only <- df_combine %>%
    filter(is.na(event.flag)) %>%
    rename(bf = runningmeandis.x,
           bf_cond = runningmean,
           bf_cl = chloride_predict)
  
  df_all_flow <- df_baseflow_only %>%
    bind_rows(df_peaks) %>%
    arrange(date) %>%
    dplyr::select(date, bf, bf_cl, bf_cond, event_flow, event_cl, event_cond, value.x, threshold_peak.y, peak.flag, event.flag) %>%
    rename(value = value.x,
           threshold_peak = threshold_peak.y)
  return(df_all_flow)
}
