library(collapse)
library(reshape2)
library(ggplot2)

rm(list=ls())
setwd("D:/David/outlier detection")
wdi_df_long <- readRDS("WDI_df_long")
wdi_df_long$Year<-as.numeric(substr(wdi_df_long$Year,2,5))
 
#Non-normalized outputs 
#outliertree_rank <- readRDS("WDI_outliers_trees_data")
#isoforest_rank <- readRDS("WDI_isoforest_data")
norm_zscore_isoforest_rank<-readRDS("norm_zscore_isoforest")
norm_outliertree_rank <- readRDS("WDI_norm_outlier_tree_data")
norm_tsoutlier_rank <- readRDS("WDI_tsoutliers")
norm_point_rank <- readRDS("point_anomalies")
collective_outlier_rank <- readRDS("collective_anomalies")

#wdi_df_long <- join(wdi_df_long,outliertree_rank,on=c("Country.Name","Year","Indicator.Code"))
#wdi_df_long <- join(wdi_df_long,isoforest_rank,on=c("Country.Name","Year","Indicator.Code"))
wdi_df_long <- join(wdi_df_long,norm_zscore_isoforest_rank,on=c("Country.Name","Year","Indicator.Code"))
wdi_df_long <- join(wdi_df_long,norm_outliertree_rank,on=c("Country.Name","Year","Indicator.Code"))
wdi_df_long <- join(wdi_df_long,norm_tsoutlier_rank,on=c("Country.Name","Year","Indicator.Code"))
wdi_df_long <- join(wdi_df_long,norm_point_rank,on=c("Country.Name","Year","Indicator.Code"))
wdi_df_long <- join(wdi_df_long,collective_outlier_rank,on=c("Country.Name","Year","Indicator.Code"))

# create composite indicator based on median of others 
#composite_df <- wdi_df_long[!is.na(wdi_df_long[,"Zscore_rank"]),]
composite_df <- fsubset(wdi_df_long,!is.na(Zscore_rank))
composite_df <- fsubset(composite_df,!is.na(norm_isoforest_rank))
composite_df <- fsubset(composite_df,!is.na(norm_outlier_tree_rank))
composite_df <- fsubset(composite_df,!is.na(tsoutlier_rank))
composite_df <- fsubset(composite_df,!is.na(point_anomalies_rank))
composite_df$composite <- dapply(composite_df[,c("norm_isoforest_rank","norm_outlier_tree_rank","tsoutlier_rank","point_anomalies_rank")],FUN=median,MARGIN=1)
composite_df <- roworder(composite_df,composite,na.last=TRUE)
composite_df$composite_rank <- 1:nrow(composite_df)
composite_df <- fselect(composite_df,"Country.Name","Year","Indicator.Code","composite_rank")
wdi_df_long <- join(wdi_df_long,composite_df,on=c("Country.Name","Year","Indicator.Code"),overid=2)

saveRDS(wdi_df_long,file="WDI_outliers")
write.csv(wdi_df_long, file = "WDI_outliers.csv", na = "", row.names = F) 

#rm(list=ls())
#wdi_df_long <- readRDS("WDI_outliers")

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
tsoutlier_rank_top30 <- wdi_df_long[1:30,c("Indicator.Name","Country.Name","Year","value","tsoutlier_rank")]

wdi_df_long <- roworder(wdi_df_long,point_anomalies_rank,na.last = TRUE)
point_outlier_rank_top30 <- wdi_df_long[1:30,c("Indicator.Name","Country.Name","Year","value","point_anomalies_rank")]
point_outlier_rank_top30

wdi_df_long <- roworder(wdi_df_long,collective_anomalies_rank,na.last=TRUE)
collective_anomalies_rank_top30 <- wdi_df_long[1:60,c("Indicator.Name","Country.Name","Year","value","collective_anomalies_rank")]
collective_anomalies_rank_top30

wdi_df_long <- roworder(wdi_df_long,composite_rank,na.last = TRUE)
composite_rank_top30 <- wdi_df_long[1:30,c("Indicator.Name","Country.Name","Year","value","composite_rank")]
composite_rank_top30

table1 <- cbind(Zscore_top30,tsoutlier_rank_top30,norm_isoforest_rank_top30,norm_outlier_tree_rank_top30,point_outlier_rank_top30,composite_rank_top30)
write.csv(table1,"Table1.csv")

write.csv(collective_anomalies_rank_top30,"Table2.csv")







#wdi_df_outliers$norm_outlier_tree_rank[wdi_df_outliers$norm_outlier_tree_rank>cutoff] <- NA 

#wdi_df_outliers_wide <- dcast(wdi_df_outliers,Country.Name + Country.Code + Indicator.Code + Indicator.Name ~ Year,
#                              value.var="value")

#wdi_df_outliers_rank <- dcast(wdi_df_outliers,Country.Name + Country.Code + Indicator.Code ~ Year,
#                              value.var="norm_outlier_tree_rank")

#colnames <- colnames(wdi_df_outliers_rank)
#wdi_df_outlier_list <- data.frame(outlier_years=rep(NA,nrow(wdi_df_outliers_rank)))
#for (col in 4:ncol(wdi_df_outliers_rank)) {
#   year <- colnames[col]
#   outliers <-  !is.na(wdi_df_outliers_rank[,col])
#   noutliers <- sum(outliers)
# wdi_df_outlier_list[outliers,] <- paste0(wdi_df_outlier_list[outliers,],rep(" ",noutliers),rep(year,noutliers))
#}
#wdi_df_outlier_list <- gsub("NA ", "", wdi_df_outlier_list[,1])
#wdi_df_outliers_wide <- cbind(wdi_df_outliers_wide,"outlier years"=wdi_df_outlier_list)

#write.csv(wdi_df_outliers_wide,"top 30 outliers.csv")




graph_outliers <- function(data=data,rank=rank,cutoff=cutoff,title=title,group=group) {
  data$outlier <- data[,rank]<=cutoff
  data$outlier_max <- fmax(data$outlier,g=data[,group],TRA=1) 
  df_outliers <- fsubset(data,outlier_max==1)
  df_outliers$min_rank <- fmin(df_outliers[,rank],g=df_outliers[,group],TRA=1)
  ggplot(df_outliers, aes(x=Year,y=value)) + geom_line() + facet_wrap(~ min_rank,scales = "free_y") + labs(title = title) + geom_point(data = subset(df_outliers, df_outliers$outlier == 1),aes(color = "red")) + theme(axis.text=element_text(size=8),legend.position="none")  
                                                                                                                         
}

graph_outliers(data=wdi_df_long,rank="Zscore_rank",group=c("Country.Code","Indicator.Code"),cutoff=16,title="Biggest outlier series using Z-score method")
graph_outliers(data=wdi_df_long,rank="tsoutlier_rank",group=c("Country.Code","Indicator.Code"),cutoff=16,title="Biggest outlier series using tsoutlier method")
graph_outliers(data=wdi_df_long,rank="norm_isoforest_rank",group=c("Country.Code","Indicator.Code"),cutoff=16,title="Biggest outlier series using Isolation Forest method")
graph_outliers(data=wdi_df_long,rank="norm_outlier_tree_rank",group=c("Country.Code","Indicator.Code"),cutoff=16,title="Biggest outlier series using Outlier Tree method")
graph_outliers(data=wdi_df_long,rank="point_anomalies_rank",group=c("Country.Code","Indicator.Code"),cutoff=16,title="Biggest outlier series using Capa method: Point outliers")
graph_outliers(data=wdi_df_long,rank="collective_anomalies_rank",group=c("Country.Code","Indicator.Code"),cutoff=16,title="Biggest outlier series using Capa method: Collective outliers")
graph_outliers(data=wdi_df_long,rank="composite_rank",group=c("Country.Code","Indicator.Code"),cutoff=16,title="Biggest outlier series using composite method")

#fsubset(wdi_df_long,Indicator.Code=="ER.H2O.INTR.K3" & Country.Code=="MEA")
xx
 



