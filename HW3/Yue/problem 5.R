# load packages
library(survival)
library(mvtnorm)
library(survminer)
setwd("~/Desktop/ph240b-survival_analysis/HW3/Yue")
rm(list=ls())
# function that takes in number of iterations,
# and return an average coverage of 95% CI over the truth
CI_coverage <- function(n) {
  for(i in 1:n) {
    # draw the W from standard normals
    W1 = rnorm(n)
    W2 = rnorm(n)
    A = rbinom(n, 1, plogis(0.1 + W1 * W2))
    # draw T and C --both generated with random uniforms as perviously described but C could
    # be generated anyway independent of T given W for identifiability purposes
    C = -log(runif(n))/(0.01 * exp(0.3 * W1))
    T =  -log(runif(n))/(0.02 * exp(2 * W1 ** 2 - A))
    # Create the survival objec
    S = Surv(time = pmin(T, C), event = (C >= T & T <= 100), type = "right")
    data = data.frame(A = A, W1 = W1, W2 = W2)
    coxfit = coxph(S ~ ., data = data)
    # true value
    truth <- exp(-1)
    # 95% CI for A
    lower <- summary(coxfit)$conf.int[, 3][1]
    upper <- summary(coxfit)$conf.int[, 4][1]
    CI_count <- CI_count +  as.numeric(truth >= lower && truth <= upper)
  }
  return(CI_count/ 1000)
}

CI_count <- 0
#coverage
cat("The average coverage of 1000 95% CI's for A is", 100 * CI_coverage(1000), "%.")

