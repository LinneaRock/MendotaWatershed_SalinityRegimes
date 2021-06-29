#Function to create grid plot with chemograph and hydrograph with stormflows separated

library(cowplot)

grid <- function(df, name) {
  #date min/max for grid
  datemin <- as.POSIXct("2019-12-18")
  datemax <- as.POSIXct("2021-04-11")
  
  #chemograph (chloride concentration)
  a <- ggplot(df) +
    geom_line(aes(date, bf_conc)) +
    geom_line(aes(date, event_conc), color = "red") +
    theme_minimal() +
    labs(x = "", y = "Chloride Concentration"~(mg~L^-1)) + 
    theme(legend.title = element_blank(),
          legend.position = "bottom") +
    scale_x_datetime(limits = c(datemin, datemax))
  #hydrograph (discharge)
  b <- ggplot(df) +
    geom_line(aes(date, bf)) +
    geom_line(aes(date, event_flow, color = "red")) +
    geom_ribbon(mapping = aes(x = date, ymin = 0, ymax = eckhardt, fill = "#E5C4A1")) +
    theme_minimal() +
    scale_color_manual(labels = "Stormflow",
                       values = "red") +
    scale_fill_manual(labels = "Eckhardt",
                      values = "#E5C4A1") +
    labs(x = "", y = "Discharge"~(m^3~s^-1)) + 
    theme(legend.title = element_blank(),
          legend.position = "bottom") +
    scale_x_datetime(limits = c(datemin, datemax)) 
  
  plot_grid(a, b, align = "v", ncol = 1) 
  
  #ggsave(paste("Plots/QC_plots/Eckhardt_Method/grids/chloride/", name, ".png", sep = ""), height = 7.25, width = 6.25, units = "in")
  
}
