# master script for outlier detection in WDI 
rm(list=ls())

source("wdi_anomaly_trees.R") #estimates outlier tree on non-normalized WDI data with indicators as features
source("wdi_isolation_forests_cells.R") # estimates isolation forests on non-normalized (long-form) WDI data. 
source("wdi_zscore.R") # estimates Z scores, isolation forests, with Z scores, and outlier trees on Z scores
xx
source("gather_outliers.R") # gathers together different estimates in one dataframe 
