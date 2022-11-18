#script to create table with relevant cQ slope information

################################################
######## ######## CQ Table ######## ######## #####
source("Code/Functions/cQ_stats_functions.R")

eventTotals = all_individual_events |> 
  mutate(slope_SpC = if_else(pval_SpC > 0.05, NA_real_, slope_SpC)) |> 
  mutate(group = case_when(slope_SpC > 0.05 ~ 'pos',
                           slope_SpC < -0.05 ~ 'neg', 
                           is.na(slope_SpC) ~ 'NS',
                           TRUE ~ 'zero')) |> 
  group_by(trib, group) |> summarise(count = n()) |> 
  pivot_wider(names_from = group, values_from = count, values_fill = 0) |> 
  mutate(sum = neg+NS+pos+zero) |> 
  bind_cols(BFI  = c(77, 49, 20, 83, 42, 76)) |> #BFI found in FigureS5_S10 script
  select(trib, BFI, sum, pos, zero, neg, NS) 

names(eventTotals) = c(
  trib = "Site",
  BFI = "Eckhardt BFI (%)",
  sum = "# of Events",
  pos = "No. Mobilization Events",
  zero = "No. Chemostatic Events",
  neg = "No. Dilution Events",
  NS = 'Undefined'
)

xtable(eventTotals)

# gt_tbl <- gt(BFI_tbl)
# simpleregtable <- gt_tbl %>%
#   cols_label(
#     trib = "Site",
#     BFI = "Eckhardt BFI (%)",
#     sum = "# of Events",
#     pos = "No. Mobilization Events",
#     zero = "No. Chemostatic Events",
#     neg = "No. Dilution Events",
#     NS = 'Undefined'
#   ) %>%
#   tab_header(
#     title = "Specific Conductivity - Discharge Relationships",
#     subtitle = "Log(SpC) - Log(Q) for all cQ data"
#   ); simpleregtable
# 
# as_latex(simpleregtable)

