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

###################################################################################
# Bonus
###################################################################################
rm(list=ls())
# we repeat the experiment 10,000 times
N <- 1000
num_W = 2
CI_coverage_bonus <- function(N, num_W) {
  for(i in 1:N){
    # simulation
    W = list()
    for(j in 1:num_W) {
      W[[j]] = rnorm(1000)
    }
    sum_W = 0
    sum_W_half = 0
    for(j in 1:num_W) {
      sum_W = sum_W + W[[j]]
      if (j < floor(num_W/2) + 1) {
        sum_W_half = sum_W_half + W[[j]]
      }
    }
    W1 = rnorm(1000)
    W2 = rnorm(1000)
    A = rbinom(1000, 1, 0.5)
    # we make C and T dependent on covariates and we also
    # make C dependent on A,
    # such that drop out is effected by treatment
    C = abs(sum_W - 10 * A)
    T = abs(sum_W_half)
    S = Surv(time = pmin(T, C), event = (C >= T & T <= 2), type = "right")
    # because T is not dependent on A, the true coefficient for A is
    truth = exp(0)
    # they run a Cox Proportional Hazards regression
    # with treatment as the only covariate
    coxfit = coxph(S ~ ., data = data.frame(A))
    # did they cover the truth in their misspecified model?
    lower <- summary(coxfit)$conf.int[, 3]
    upper <- summary(coxfit)$conf.int[, 4]
    CI_count <- CI_count +  as.numeric(truth >= lower && truth <= upper)
  }
  # calculating the coverage
  return(CI_count/ 1000)
}
CI_count <- 0
cat("The average coverage rate of 1000 95% CI for A with 2 confounders is",
    100 * CI_coverage_bonus(1000, 2), "%.\n")
cat("The average coverage rate of 1000 95% CI for A with 5 confounders is",
    100 * CI_coverage_bonus(1000, 5), "%.\n")
cat("The average coverage rate of 1000 95% CI for A with 7 confounders is",
    100 * CI_coverage_bonus(1000, 7), "%.\n")
