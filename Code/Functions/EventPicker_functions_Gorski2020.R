#code returns a dataframe with stormflow events identified. 

#code modified from:Gorski, G. (2020). EventPicker. GitHub Repository, https://github.com/galengorski/EventPicker 
library(tidyverse)

#df.orig is a dataframe with baseflow and threshold calculated already 

# cl_ts_data is the chloride timeseries calculated from the cond

#the find.peaks function should ingest a dataframe and output a dataframe with the peaks idenitified
find.peaks <- function(df.orig, cl_ts_data){
  
  timestamp <- 'date'
  plot_var <- 'Daily_dis_cms'
  sb_pk_thresh <- 0.000001
  sf_pk_thresh <- 0
  
  df <- df.orig %>%
    group_by(as.Date(dateTime, format = "%m/%d/%y")) %>%
    summarise(mean(MovingAverage_dis_cms),
              mean(eckhardt)) %>%
    rename(date = 'as.Date(dateTime, format = "%m/%d/%y")',
           Daily_dis_cms = 'mean(MovingAverage_dis_cms)',
           eckhardt = 'mean(eckhardt)') %>%
    ungroup() %>%
    as.data.frame() %>%
    mutate(threshold_peak = eckhardt + (mean(Daily_dis_cms))/2) 
  
  
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
  
  
  #get dates during rain and for 2 days following rain events
  precip_data <- read.csv("Data/precipitation.csv") %>%  #data in mm already
    mutate(date = as.POSIXct(as.character(DATE))) %>%
    filter(date > "2019-12-17") %>%
    mutate(rain = ifelse(PRCP == 0, "N", "Y")) %>%
   # mutate(keep = ifelse(lag(rain, n = 1L) == "Y" | PRCP > 0, 1, 0)) %>%
    mutate(keep = ifelse(lag(rain, n = 1L) == "Y" | lag(rain, n = 2L) == "Y" | PRCP > 0, 1, 0)) %>%
    filter(keep == 1) %>%
    dplyr::select(date, PRCP, rain, keep)

  
  df <- df %>%
    mutate(peak.flag = ifelse(peak.flag == 1 & Daily_dis_cms > threshold_peak, 1, 0)) 
  
  df <- df %>%
    left_join(precip_data) %>%
    mutate(peak.flag = ifelse(peak.flag == 1 & keep == 1, 1, 0))
 
    ggplot() +
     geom_line(df, mapping = aes(date, Daily_dis_cms)) +
     geom_point(df%>%filter(peak.flag == 1), mapping = aes(date, Daily_dis_cms), color = "red") +
     geom_line(df, mapping = aes(date, threshold_peak),color = "blue") +
      geom_point(df, mapping = aes(date, keep), color = "purple")
  
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

  #df now has daily values, remainder of lines add back in all sub-hourly data
  df1 <- df.orig %>% #sub-hourly discharge data
    mutate(date = as.Date(dateTime)) %>%
    left_join(df, by = "date")
  
  df_combine <- df1 %>%
    left_join(cl_ts_data) #sub-hourly chloride data

  #dataframe with events only  
  df_peaks <- df_combine %>% 
    drop_na(event.flag) %>%
    rename(event_flow_cms = MovingAverage_dis_cms,
           event_SpC_uScm = MovingAverage_SpCond_uScm,
           event_chloride_mgL = chloride_estimated_mgL,
           event_chloride_Mg = chloride_mass_Mg)
  
  #dataframe without any events
  df_baseflow_only <- df_combine %>%
    filter(is.na(event.flag)) %>%
    rename(bf_cms = MovingAverage_dis_cms,
           bf_SpC_uScm = MovingAverage_SpCond_uScm,
           bf_chloride_mgL = chloride_estimated_mgL,
           bf_chloride_Mg = chloride_mass_Mg)
  

  #all data
  df_all_flow <- df_baseflow_only %>%
    bind_rows(df_peaks) %>%
    arrange(date) %>%
    mutate(all_dis_cms = ifelse(is.na(bf_cms), event_flow_cms, bf_cms)) %>%
    mutate(all_chloride_mgL = ifelse(is.na(bf_chloride_mgL), event_chloride_mgL, bf_chloride_mgL)) %>%
    mutate(all_chloride_Mg = ifelse(is.na(bf_chloride_Mg), event_chloride_Mg, bf_chloride_Mg)) %>%
    mutate(all_SpC_uScm = ifelse(is.na(bf_SpC_uScm), event_SpC_uScm, bf_SpC_uScm)) %>%
    rename(eckhardt = eckhardt.x) %>%
    dplyr::select(dateTime, bf_cms, bf_SpC_uScm, bf_chloride_mgL, bf_chloride_Mg, event_flow_cms, event_SpC_uScm, event_chloride_mgL, event_chloride_Mg, eckhardt, all_dis_cms, all_SpC_uScm, all_chloride_mgL, all_chloride_Mg, event.flag)

  
  return(df_all_flow)
  #bf_cms = discharge (cms) not during events
  #bf_SpC_uScm = specific conductivity (microsiemens per liter) during non-events
  #bf_chloride_mgL = chloride concentration (mg L^-1) during non-events
  #bf_chloride_Mg = chloride mass (metric tonnes) during non-events
  #event_flow_cms = discharge (cms) during stormflow events
  #event_SpC_uScm = specific conductivity (microsiemens per liter) during stormflow events
  #event_chloride_mgL = chloride concentration (mg L^-1) during stormflow events
  #event_chloride_Mg = chloride mass (metric tonnes) during stormflow events
  #eckhardt = eckhardt baseflow (cms)
  #all_dis_cms = all (bf and stormflow) discharge (cms)
  #all_SpC_uScm = all (bf and stormflow) specific conductivity (microsiemens per liter)
  #all_chloride_mgL = all (bf and stormflow) chloride concentrations (mg L^-1)
  #all_chloride_mass = all (bf and stormflow) chloride mass loading (metric tonnes)
  #event.flag = identifies individual events
}


