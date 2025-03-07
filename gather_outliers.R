rm(list=ls())
library(collapse)
setwd("D:/David/outlier detection")
wdi_df_long <- readRDS("WDI_df_long")
wdi_df_long$Year<-as.numeric(substr(wdi_df_long$Year,2,5))
 
#Non-normalized outputs 
outliertree_rank <- readRDS("WDI_outliers_trees_data")
isoforest_rank <- readRDS("WDI_isoforest_data")
norm_zscore_isoforest_rank<-readRDS("norm_zscore_isoforest")
norm_outliertree_rank <- readRDS("WDI_norm_outlier_tree_data")

wdi_df_long <- join(wdi_df_long,outliertree_rank,on=c("Country.Name","Year","Indicator.Code"))
wdi_df_long <- join(wdi_df_long,isoforest_rank,on=c("Country.Name","Year","Indicator.Code"))
wdi_df_long <- join(wdi_df_long,norm_zscore_isoforest_rank,on=c("Country.Name","Year","Indicator.Code"))
wdi_df_long <- join(wdi_df_long,norm_outliertree_rank,on=c("Country.Name","Year","Indicator.Code"))

saveRDS(wdi_df_long,file="WDI_outliers")
write.csv(wdi_df_long, file = "WDI_outliers.csv", na = "", row.names = F) 

#rm(list=ls())
wdi_df_long <- readRDS("WDI_outliers")


wdi_df_long <- roworder(wdi_df_long,outlier_tree_rank)
head(wdi_df_long,20)
wdi_df_long <- roworder(wdi_df_long,isoforest_rank)
head(wdi_df_long,20)
wdi_df_long <- roworder(wdi_df_long,Zscore_rank)
head(wdi_df_long,20)
wdi_df_long <- roworder(wdi_df_long,norm_isoforest_rank)
head(wdi_df_long,20)
wdi_df_long <- roworder(wdi_df_long,norm_outlier_tree_rank)
head(wdi_df_long,20)
