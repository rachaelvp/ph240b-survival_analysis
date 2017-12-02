rm(list=ls())
setwd("~/Desktop/ph240b-survival_analysis/HW4/Yue")
load("longdata.RData")
library(survival)
library(survminer)
library(ltmle)
# Note, help recieved from Jonathan with setting up the LTMLE
Odat = Odat.wide[, c(2:5, 7:27)]
Anodes = grep("A1", colnames((Odat)))
Cnodes = grep("A2", colnames((Odat)))
Lnodes = grep("L", colnames((Odat)))
Ynodes = grep("Y", colnames((Odat)))
for (col in Cnodes) Odat[, col] = BinaryToCensoring(is.censored = Odat[col])
Odat[, "Y_1"][is.na(Odat$Y_1)] = 1
Odat[, "Y_2"][is.na(Odat$Y_2)] = 1
Odat[, "Y_3"][is.na(Odat$Y_3)] = 1
Odat[, "Y_4"][is.na(Odat$Y_4)] = 1
Odat[, "Y_5"][is.na(Odat$Y_5)] = 1
