# master script for outlier detection in WDI 
source("wdi_anomoly_trees") #estimates outlier tree on non-normalized WDI data with indicators as features
source("wdi_isolation_forests_cells") # estimates isolation forests on non-normalized (long-form) WDI data. 
source("wdi_zscore") # estimates Z scores, isolation forests, with Z scores, and outlier trees on Z scores 
source("gather_outliers") # gatheres together different estimates in one dataframe 