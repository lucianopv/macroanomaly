# master script for outlier detection in WDI 
rm(list=ls())

#source("wdi_anomaly_trees.R") #estimates outlier tree on non-scaled WDI data with indicators as features
#source("wdi_isolation_forests_cells.R") # estimates isolation forests on non-scaled (long-form) WDI data. 
source("wdi_zscore.R",echo=T) # estimates scaled and detrended Z scores (using mad for scaling), isolation forests, and outlier trees on scaled and detrended Z scores 
source("wdi_tsoutliers.R",echo=T) # estimates tsoutliers using forecast package on scaled data. 
source("wdi_anomaly_package.R",echo=T) # estimates point and collective anomalies using the "anomaly" package on scaled (but not detrended) data 

source("gather_outliers.R",echo=T) # gathers together different estimates in one dataframe for analysis 
