#code returns a figure

#code to create daily chemographs and hydrographs. First need to run for daily peaks
#code modified from:Gorski, G. (2020). EventPicker. GitHub Repository, https://github.com/galengorski/EventPicker 
library(tidyverse)

#df.orig is a dataframe with baseflow and threshold calculated already 

# cl_ts_data is the chloride timeseries calculated from the cond

#the find.peaks function should ingest a dataframe and output a dataframe with the peaks idenitified
chemo_hydro_graph <- function(df.orig, cl_ts_data, rivername){
  
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
    geom_point(df%>%filter(peak.flag == 1), mapping = aes(date, Daily_dis_cms), color = "#E69F00") +
    geom_line(df, mapping = aes(date, threshold_peak),color = "#0072B2") +
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
  
  
  
  df_combine <- df %>%
    left_join(
      cl_ts_data %>% mutate(date = as.Date(dateTime)) %>% group_by(date) %>% summarise(
        Daily_chloride_mgL = mean(chloride_estimated_mgL),
        Daily_SpCond_uScm = mean(MovingAverage_SpCond_uScm),
        Daily_chloride_load_Mg = sum(chloride_mass_Mg)
      ),
      by = "date")
  
  
  df_peaks <- df_combine %>%
    drop_na(event.flag) %>%
    rename(event_flow = Daily_dis_cms,
           event_SpCond = Daily_SpCond_uScm,
           event_conc = Daily_chloride_mgL,
           event_mass = Daily_chloride_load_Mg)
  
  df_baseflow_only <- df_combine %>%
    filter(is.na(event.flag)) %>%
    rename(bf = Daily_dis_cms,
           bf_SpCond = Daily_SpCond_uScm,
           bf_conc = Daily_chloride_mgL,
           bf_mass = Daily_chloride_load_Mg)
  
  df_all_flow <- df_baseflow_only %>%
    bind_rows(df_peaks) %>%
    arrange(date) %>%
    mutate(all_dis = ifelse(is.na(bf), event_flow, bf)) %>%
    mutate(all_SpCond = ifelse(is.na(bf_SpCond), event_SpCond, bf_SpCond)) %>%
    mutate(all_conc = ifelse(is.na(bf_conc), event_conc, bf_conc)) %>%
    mutate(all_mass = ifelse(is.na(bf_mass), event_mass, bf_mass)) %>%
    dplyr::select(date, bf, bf_SpCond, bf_conc, bf_mass, event_flow, event_SpCond, event_conc, event_mass, eckhardt, all_dis, all_SpCond, all_conc, all_mass, event.flag) 
  
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
  
  library(cowplot)
  
  #date min/max for grid
  datemin <- as.POSIXct("2019-12-18")
  datemax <- as.POSIXct("2021-04-11")
  
  #chemograph (chloride concentration)
  a <- ggplot(df_all_flow) +
    geom_line(aes(date, all_SpCond)) +
    geom_line(aes(date, event_SpCond), color = "#E69F00") +
    theme_minimal(base_size = 9) +
    labs(x = "", y = "SpC"~(mu*S~cm^-1)~"@ 25"*~degree*C,
         title = rivername) + 
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(size = 9)) +
    scale_x_datetime(limits = c(datemin, datemax))
  #hydrograph (discharge)
  b <- ggplot(df_all_flow) +
    geom_line(aes(date, all_dis)) +
    geom_line(aes(date, event_flow), color = "#E69F00") +
    geom_ribbon(mapping = aes(x = date, ymin = 0, ymax = eckhardt, fill = "#0072B2")) +
    theme_minimal(base_size = 9) +
    scale_color_manual(labels = "Stormflow",
                       values = "#E69F00") +
    scale_fill_manual(labels = "Eckhardt Baseflow",
                     values = "#0072B2") +
    labs(x = "", y = "Discharge"~(m^3~s^-1)) + 
    theme(plot.title = element_text(size = 9),
          legend.title = element_blank(),
          legend.position = "bottom") +
    scale_x_datetime(limits = c(datemin, datemax)) 
  
  plot_grid(a, b, align = "v", ncol = 1) 
  
  
}


