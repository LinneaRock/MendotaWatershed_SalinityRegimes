library(tidyverse)

#script to call in cQ slopes for full record, baseflow only, individual stormflow events, and bulk stormflow events.

all_full <- read_rds("Data/cQ_slopes/fullRecord.rds")
all_baseflow <- read_rds("Data/cQ_slopes/baseflow.rds")
all_individual_events <- read_rds("Data/cQ_slopes/individualEvents.rds")
all_bulkstorm <- read_rds("Data/cQ_slopes/bulkStormflow.rds")
