## Data load ###

# Load libraries
library(broom)
library(colorblindr)
library(easystats)
library(egg)
library(FedData)
library(ggiraphExtra)
library(ggspatial)
library(gt)
library(jtools)
library(patchwork)
library(MetBrewer)
library(multcompView)
library(NTLlakeloads) #install_github('hdugan/NTLlakeloads')
library(patchwork)
library(purrr)
library(raster)
library(readxl)
library(scales)
library(scatterpie)
library(sf)
library(tidyverse)
library(tigris)
options(tigris_use_cache = TRUE)
library(webshot)
library(xtable)


#load datasets in this order 
source("Code/DataLoad/Estimated_Chloride_Conc_Mass.R") 
source("Code/DataLoad/Baseflow_Events_Separation.R")
source("Code/DataLoad/cQ_Slope_Calculations.R") 
