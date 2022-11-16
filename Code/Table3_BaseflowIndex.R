#script to create table with relevant cQ slope information

################################################
######## ######## CQ Table ######## ######## #####
source("Code/Functions/count_stormflow_events_functions.R")

BFI_tbl <- data.frame(
  Trib = c(
    "YR-I",
    "SMC",
    "DC",
    "PB",
    "YR-O",
    "SH"
  ),
  #BFI found in cQ_Slope_Calculations.R
  BFI = c(42,
          82,
          77,
          49,
          76,
          20
  ),
  stormflow_pos = c(count_mobilization_events(all_individual_events, "YR-I"),
                    count_mobilization_events(all_individual_events, "SMC"),
                    count_mobilization_events(all_individual_events, "DC"),
                    count_mobilization_events(all_individual_events, "PB"),
                    count_mobilization_events(all_individual_events, "YR-O"),
                    count_mobilization_events(all_individual_events, "SH")
  ),
  stormflow_stat = c(count_chemostatic_events(all_individual_events, "YR-I"),
                     count_chemostatic_events(all_individual_events, "SMC"),
                     count_chemostatic_events(all_individual_events, "DC"),
                     count_chemostatic_events(all_individual_events, "PB"),
                     count_chemostatic_events(all_individual_events, "YR-O"),
                     count_chemostatic_events(all_individual_events, "SH")
                     
  ),
  stormflow_neg = c(count_dilution_events(all_individual_events, "YR-I"),
                    count_dilution_events(all_individual_events, "SMC"),
                    count_dilution_events(all_individual_events, "DC"),
                    count_dilution_events(all_individual_events, "PB"),
                    count_dilution_events(all_individual_events, "YR-O"),
                    count_dilution_events(all_individual_events, "SH")
                    
  )
)

library(gt)

gt_tbl <- gt(BFI_tbl)
simpleregtable <- gt_tbl %>%
  cols_label(
    Trib = "Site",
    BFI = "Eckhardt BFI (%)",
    stormflow_neg = "No. Dilution Events",
    stormflow_stat = "No. Chemostatic Events",
    stormflow_pos = "No. Mobilization Events"
  ) %>%
  tab_header(
    title = "Specific Conductivity - Discharge Relationships",
    subtitle = "Log(SpC) - Log(Q) for all cQ data"
  ); simpleregtable

as_latex(simpleregtable)

xtable(BFI_tbl)