#functions to get cQ regression statistics

#slope
slope_cq <- function(fit) {
  round(coef(fit)[2,1], 2)
}

#intercept
intercept_cq <- function(fit) {
  round(coef(fit)[1,1], 2)
}



