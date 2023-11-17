## Calculating minimum calibration sample size based on the beta distribution of conformal coverage distribution
## converted to R from https://github.com/aangelopoulos/conformal-prediction/blob/main/notebooks/correctness_checks.ipynb

library(pracma)
calibration_n <- function(n){
  alpha=0.05
  epsilon=0.05
  l = floor((n+1)*alpha)
  a = n + 1 - l
  b = l
 if((qbeta(0.05, a, b) < 1-alpha-epsilon)|(qbeta(0.95, a, b) > 1-alpha+epsilon) ){
   return(-1)
  }
  else{
    return(1)
  }
}

alpha = 0.05
epsilons = c(0.1,0.05,0.01,0.005,0.001)

calibration_size <- ceiling(brent(calibration_n,  ceiling(1/alpha), 100000000000)$root)
