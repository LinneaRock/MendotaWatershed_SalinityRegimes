#Baseflow cq relationships#####
#function to obtain a dataframe with the baseflow cQ slopes 

source("Code/Functions/cQ_stats_functions.R")

cQslopes_function <- function(events_bf, name, use = 'baseflow') {
  
  df <- events_bf %>%
    addSeason() %>%
    mutate(timestep = (dateTime - lag(dateTime)) *  60) %>% #timestep in seconds
    mutate(vol_water = bf_cms * timestep)
  
  if(use == 'baseflow') {
    df = df |> filter(is.na(event.flag))
    df1 <- df %>%
      drop_na(bf_chloride_mgL, bf_SpC_uScm) %>%
      drop_na(bf_SpC_uScm) %>%
      filter(bf_SpC_uScm > 0, bf_cms > 0, bf_chloride_mgL > 0) %>%
      mutate(discharge.log10 = log10(bf_cms)) %>%
      mutate(chloride.log10 = log10(bf_chloride_mgL)) %>%
      mutate(trib = "name")  %>%
      mutate(SpC.log10 = log10(bf_SpC_uScm))
  }
  
  if(use == 'full') {
    df1 = df |> mutate(discharge = all_dis_cms) %>%
      mutate(chloride = all_chloride_mgL) %>%
      mutate(SpC = all_SpC_uScm) %>%
      filter(chloride > 0) %>%
      filter(discharge > 0) %>%
      mutate(chloride.log10 = log10(chloride)) %>%
      mutate(discharge.log10 = log10(discharge)) %>%
      mutate(SpC.log10 = log10(SpC)) |> 
      mutate(trib = "name")
  }
  
  if(use == 'bulk') {
    df = df |> 
      mutate(event.flag = ifelse(event.flag < 0, event.flag * -1, event.flag)) %>%
      drop_na(event.flag) 
    
    df1 <- df %>%
      drop_na(event_chloride_mgL) %>% #drop NAs and anything below 0 (will not work with log)
      drop_na(event_SpC_uScm) %>%
      filter(event_flow_cms > 0) %>%
      filter(event_chloride_mgL > 0) %>%
      filter(event_SpC_uScm > 0) %>%
      mutate(discharge.log10 = log10(event_flow_cms)) %>% #calculate logs
      mutate(chloride.log10 = log10(event_chloride_mgL)) %>%
      mutate(SpC.log10 = log10(event_SpC_uScm))
  }
  
  df2 = df1 |> mutate(season = factor('Annual', levels = 'Annual')) |> 
    bind_rows(df1)
  
  # SpC fits
  spc.slopes = df2 |> 
    nest_by(season) |> 
    mutate(mod = list(lm(SpC.log10 ~ discharge.log10, data = data))) %>%
    summarize(tidy(mod)) |> 
    select(season, term, estimate) |> 
    pivot_wider(names_from = term, values_from = estimate) |> 
    rename(intercept_SpC = 2, slope_SpC = 3) 
  
  # Chloride fits
  cl.slopes = df2 |> 
    nest_by(season) |> 
    mutate(mod = list(lm(chloride.log10 ~ discharge.log10, data = data))) %>%
    summarize(tidy(mod)) |> 
    select(season, term, estimate) |> 
    pivot_wider(names_from = term, values_from = estimate) |> 
    rename(intercept_chloride = 2, slope_chloride = 3) 
  
  baseflow_fits =  spc.slopes |> left_join(cl.slopes) |> 
    mutate(trib = name) |> 
    select(trib, everything())
  
  return(baseflow_fits)
  
}