rm(list=ls())
library(collapse)
library(reshape2)
library(ggplot2)
setwd("D:/David/outlier detection")
wdi_df_long <- readRDS("WDI_df_long")
wdi_df_long$Year<-as.numeric(substr(wdi_df_long$Year,2,5))
 
#Non-normalized outputs 
outliertree_rank <- readRDS("WDI_outliers_trees_data")
isoforest_rank <- readRDS("WDI_isoforest_data")
norm_zscore_isoforest_rank<-readRDS("norm_zscore_isoforest")
norm_outliertree_rank <- readRDS("WDI_norm_outlier_tree_data")
norm_tsoutlier_rank <- readRDS("WDI_tsoutliers")


wdi_df_long <- join(wdi_df_long,outliertree_rank,on=c("Country.Name","Year","Indicator.Code"))
wdi_df_long <- join(wdi_df_long,isoforest_rank,on=c("Country.Name","Year","Indicator.Code"))
wdi_df_long <- join(wdi_df_long,norm_zscore_isoforest_rank,on=c("Country.Name","Year","Indicator.Code"))
wdi_df_long <- join(wdi_df_long,norm_outliertree_rank,on=c("Country.Name","Year","Indicator.Code"))
wdi_df_long <- join(wdi_df_long,norm_tsoutlier_rank,on=c("Country.Name","Year","Indicator.Code"))

saveRDS(wdi_df_long,file="WDI_outliers")
write.csv(wdi_df_long, file = "WDI_outliers.csv", na = "", row.names = F) 

#rm(list=ls())
wdi_df_long <- readRDS("WDI_outliers")


wdi_df_long <- roworder(wdi_df_long,outlier_tree_rank)

wdi_df_long <- roworder(wdi_df_long,isoforest_rank)
wdi_df_long <- roworder(wdi_df_long,Zscore_rank,na.last = TRUE)
Zscore_top30 <- wdi_df_long[1:30,c("Indicator.Name","Country.Name","Year","value")]
Zscore_top30
wdi_df_long <- roworder(wdi_df_long,norm_isoforest_rank,na.last = TRUE)
norm_isoforest_rank_top30 <- wdi_df_long[1:30,c("Indicator.Name","Country.Name","Year","value")]
norm_isoforest_rank_top30

wdi_df_long <- roworder(wdi_df_long,norm_outlier_tree_rank,na.last = TRUE)
norm_outlier_tree_rank_top30 <- wdi_df_long[1:30,c("Indicator.Name","Country.Name","Year","value")]
norm_outlier_tree_rank_top30
wdi_df_long <- roworder(wdi_df_long,tsoutlier_rank,na.last = TRUE)
tsoutlier_rank_top30 <- wdi_df_long[1:30,c("Indicator.Name","Country.Name","Year","value")]

table1 <- cbind(Zscore_top30,tsoutlier_rank_top30,norm_isoforest_rank_top30,norm_outlier_tree_rank_top30)
write.csv(table1,"Table1.csv")

# draw time trend scatterplot of value, with outliers in different colors  
# Zscore first 
cutoff = 16
wdi_df_long$outlier = (wdi_df_long$Zscore_rank<=cutoff)
wdi_df_long$outlier_max <- fmax(wdi_df_long$outlier,g=wdi_df_long[,2:3],TRA=1)
wdi_df_outliers <-fsubset(wdi_df_long,wdi_df_long$outlier_max==1)
wdi_df_outliers <- roworder(wdi_df_outliers,"Indicator.Code","Country.Code","Year") 
wdi_df_outliers$min_Zscore_rank <- fmin(wdi_df_outliers$Zscore_rank,g=wdi_df_outliers[,2:3],TRA=1)

graphtimetrends <- function(df=df,by=by,x=x,y=y) {
  # start just doing the top  one 
  df <- fsubset(df,df[,by]==1)
  ggplot(df, aes(x=x, y=y)) +
    geom_line()
  
  
}

test <- fsubset(df,df[,"min_Zscore_rank"]==1)

graphtimetrends(df=wdi_df_outliers,by="min_Zscore_rank",x="Year",y="value")





wdi_df_outliers$norm_outlier_tree_rank[wdi_df_outliers$norm_outlier_tree_rank>cutoff] <- NA 

wdi_df_outliers_wide <- dcast(wdi_df_outliers,Country.Name + Country.Code + Indicator.Code + Indicator.Name ~ Year,
                              value.var="value")

wdi_df_outliers_rank <- dcast(wdi_df_outliers,Country.Name + Country.Code + Indicator.Code ~ Year,
                              value.var="norm_outlier_tree_rank")

colnames <- colnames(wdi_df_outliers_rank)
wdi_df_outlier_list <- data.frame(outlier_years=rep(NA,nrow(wdi_df_outliers_rank)))
for (col in 4:ncol(wdi_df_outliers_rank)) {
   year <- colnames[col]
   outliers <-  !is.na(wdi_df_outliers_rank[,col])
   noutliers <- sum(outliers)
 wdi_df_outlier_list[outliers,] <- paste0(wdi_df_outlier_list[outliers,],rep(" ",noutliers),rep(year,noutliers))
}
wdi_df_outlier_list <- gsub("NA ", "", wdi_df_outlier_list[,1])
wdi_df_outliers_wide <- cbind(wdi_df_outliers_wide,"outlier years"=wdi_df_outlier_list)

write.csv(wdi_df_outliers_wide,"top 30 outliers.csv")
