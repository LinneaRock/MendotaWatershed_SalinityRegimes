#functions to get linear regression statistics

source("Code/Functions/join_field_cond_function.R")


#function to obtain slope:
slope <- function(field_data, logger_data) {
  round(coef(summary(lm(chloride_mgL ~ SpCond_uScm.x, join_for_linreg(field_data, logger_data))))[2,1], 2)
}
#function to obtain intercept
intercept <- function(field_data, logger_data) {
  round(coef(summary(lm(chloride_mgL ~ SpCond_uScm.x,join_for_linreg(field_data, logger_data))))[1,1], 2)
}
#function to obtain R^2
r.sqr.lm <- function(field_data, logger_data) {
  #round((info(cl, cond)$adj.r.squared), 2)
  round((summary(lm(chloride_mgL ~ SpCond_uScm.x,join_for_linreg(field_data, logger_data)))$r.squared), 2)
}
#function to obtain p-value
pvalue <- function(field_data, logger_data) {
  coef(summary(lm(chloride_mgL ~ SpCond_uScm.x,join_for_linreg(field_data, logger_data))))[2,4]
}