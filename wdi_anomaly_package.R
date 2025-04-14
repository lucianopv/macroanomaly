library(anomaly)
library(collapse)
library(stats)
library(xts)

rm(list=ls())
setwd("D:/David/outlier detection")
wdi_df_norm = readRDS(file="wdi_df_norm_nosmooth")

wdi_df_norm_long <- melt(wdi_df_norm, 
                         id.vars = c("Country.Name", "Country.Code",
                                     "Year"), variable.name = "Indicator.Code")
colnames(wdi_df_norm_long)[5] <- "Zscore"
wdi_df_norm_long$Zscore[abs(wdi_df_norm_long$Zscore)==Inf] <- NA 

wdi_df_norm_long$Zscore <- as.numeric(wdi_df_norm_long$Zscore) 



wdi_df_norm_long$country_indicator <- group(wdi_df_norm_long[,c("Indicator.Code","Country.Code")])
#wdi_df_norm_long$nobs <- NULL

wdi_df_norm_long <- fsubset(wdi_df_norm_long,!is.na(wdi_df_norm_long$Zscore)) 
wdi_df_norm_long$nobs <- fnobs(wdi_df_norm_long[,"Zscore"],g=wdi_df_norm_long[,"country_indicator"],TRA=1)
wdi_df_norm_long <- fsubset(wdi_df_norm_long,nobs>=10)

#wdi_df_norm_long$date <- paste0("07/02/",wdi_df_norm_long$Year)
#wdi_df_norm_long$date <- as.Date(wdi_df_norm_long$date,"%m/%d/%Y")


#wdi_df_norm_long <- colorder(wdi_df_norm_long,Zscore,country_indicator)
#wdi_df_norm_xts <- as.xts(wdi_df_norm_long[,1:2],order.by = wdi_df_norm_long$date)




wdi_df_norm_long$country_indicator <- group(wdi_df_norm_long[,c("Indicator.Code","Country.Code")])
saveRDS(wdi_df_norm_long,"wdi_df_norm_long")

capa_outliers_robust <- unlist(BY(as.matrix(wdi_df_norm_long[,"Zscore"]),g=wdi_df_norm_long[,"country_indicator"],FUN=capa,type="robustmean",return=4,min_seg_len=3)[[1]])
capa_outliers <- unlist(BY(as.matrix(wdi_df_norm_long[,"Zscore"]),g=wdi_df_norm_long[,"country_indicator"],FUN=capa,type="mean",return=4,min_seg_len=3)[[1]])
saveRDS(capa_outliers,"capa_outliers")
saveRDS(capa_outliers_robust,"capa_outliers_robust")



rm(list=ls())
capa_outliers <- readRDS("capa_outliers")
wdi_df_norm_long <- readRDS("wdi_df_norm_long")

collective_anomalies <- lapply(capa_outliers,FUN=collective_anomalies)
collective_anomalies <- unlist2d(collective_anomalies)
saveRDS(collective_anomalies,"collective_anomalies")
point_anomalies <- lapply(capa_outliers,FUN=point_anomalies)
point_anomalies <- unlist2d(point_anomalies,idcols = "country_indicator")

point_anomalies <-  point_anomalies[!is.na(point_anomalies$strength),]
point_anomalies$variate <- NULL 
point_anomalies <- roworder(point_anomalies,-strength)
point_anomalies$point_anomalies_rank <- seq(1:nrow(point_anomalies))

colnames(point_anomalies)[3] <- "point_anomalies_strength"

wdi_df_norm_long$Year <- as.numeric(wdi_df_norm_long$Year)
wdi_df_norm_long$ones <- 1 
wdi_df_norm_long$location <- fcumsum(wdi_df_norm_long$ones,g=wdi_df_norm_long$country_indicator) 
wdi_df_norm_long$ones <- NULL 
wdi_df_norm_long <- join(wdi_df_norm_long,point_anomalies,on=c("country_indicator","location"))

point_anomalies_output <- fselect(wdi_df_norm_long,c("Country.Name","Year","Indicator.Code","point_anomalies_strength","point_anomalies_rank"))
saveRDS(point_anomalies_output,"point_anomalies")  

rm(list=ls())
wdi_df_norm_long <- readRDS("wdi_df_norm_long")
wdi_df_norm_long$ones <- 1 
wdi_df_norm_long$location <- fcumsum(wdi_df_norm_long$ones,g=wdi_df_norm_long$country_indicator) 
wdi_df_norm_long$ones <- NULL 

collective_anomalies <- readRDS("collective_anomalies")
collective_anomalies <- roworder(collective_anomalies,-test.statistic)
collective_anomalies$collective_anomalies_rank <- seq(1:nrow(collective_anomalies))
colnames(collective_anomalies)[1] <- "country_indicator"

wdi_df_norm_long$start <- wdi_df_norm_long$location
wdi_df_norm_long$end <- wdi_df_norm_long$location 
collective_anomalies <- fselect(collective_anomalies,c("country_indicator","start","end","test.statistic","collective_anomalies_rank"))
wdi_df_norm_long <- join(wdi_df_norm_long,collective_anomalies,on=c("country_indicator","start"),drop.dup.cols=T)
wdi_df_norm_long <- join(wdi_df_norm_long,collective_anomalies,on=c("country_indicator","end"),suffix="_end")
wdi_df_norm_long$collective_anomalies_rank[is.na(wdi_df_norm_long$collective_anomalies_rank)] <- wdi_df_norm_long$collective_anomalies_rank_end[is.na(wdi_df_norm_long$collective_anomalies_rank)]
wdi_df_norm_long$test.statistic[is.na(wdi_df_norm_long$test.statistic)] <- wdi_df_norm_long$test.statistic_end[is.na(wdi_df_norm_long$test.statistic)]
collective_anomalies_output <- fselect(wdi_df_norm_long,c("Country.Name","Year","Indicator.Code","test.statistic","collective_anomalies_rank"))
saveRDS(collective_anomalies_output,"collective_anomalies")  

