rm(list=ls())
setwd("D:/David/outlier detection")
wdi_df_long <- readRDS("WDI_df_long")
wdi_df_long$WDI_order <- 1:nrow(wdi_df_long)
wdi_df_long$Year<-as.numeric(substr(wdi_df_long$Year,2,5))
 
#Non-normalized outputs 
outliertree_rank <- readRDS("WDI_outliers_trees_data")
outliertree_rank <- outliertree_rank[order(outliertree_rank$WDI_order),] 
isoforest_rank <- readRDS("WDI_isoforest_data")
isoforest_rank <- isoforest_rank[order(outliertree_rank$WDI_order),] 
norm_zscore_isoforest_rank<-readRDS("norm_zscore_isoforest")
norm_zscore_isoforest_rank <- norm_zscore_isoforest_rank[order(norm_zscore_isoforest_rank$WDI_order),] 
norm_outliertree_rank <- readRDS("WDI_norm_outlier_tree_data")
norm_outliertree_rank <- norm_outliertree_rank[order(norm_outliertree_rank$WDI_order),] 
wdi_df_long$outliertree_rank <- outliertree_rank$outlier_tree_rank
wdi_df_long$isoforest_rank <- isoforest_rank$isoforest_rank
wdi_df_long$Zscore_rank <- norm_zscore_isoforest_rank$Zscore_rank
wdi_df_long$norm_isoforest_rank <- norm_zscore_isoforest_rank$norm_isoforest_rank
wdi_df_long$norm_outliertree_rank <- norm_outliertree_rank$norm_outliertree_rank
saveRDS(wdi_df_long,file="WDI_outliers")
write.csv(wdi_df_long, file = "WDI_outliers.csv", na = "", row.names = F) 
#wdi_df_long <- wdi_df_long[order(wdi_df_long$WDI_order),]
wdi_df_long <- wdi_df_long[order(wdi_df_long$outliertree_rank),]
head(wdi_df_long,20)
