library(forecast)
library(collapse)
setwd("D:/David/outlier detection")
rm(list=ls())
wdi_df_norm_long = readRDS(file="wdi_df_norm_long")
head(wdi_df_norm_long)
#test <- wdi_df_norm_long[wdi_df_norm_long$Country.Code=="AFG" & wdi_df_norm_long$Indicator.Code=="AG.CON.FERT.PT.ZS",]
#test <- wdi_df_norm_long[wdi_df_norm_long$Country.Code=="AFG",]

# count number of non-NAs 
Nnonmiss <- fsum(!is.na(wdi_df_norm_long[,"Zscore"]),g=wdi_df_norm_long[,c("Country.Code","Indicator.Code")],TRA=1)
wdi_df_norm_long_2 <- wdi_df_norm_long[(Nnonmiss>=2),]
tsoutliers <- BY(wdi_df_norm_long_2[,"Zscore"],g=wdi_df_norm_long_2[,c("Country.Code","Indicator.Code")],FUN=tsoutliers)
saveRDS(tsoutliers,"tsoutliers")
tsoutliers <- readRDS("tsoutliers")
 

indices <- unlist2d(get_elem(tsoutliers,".index",regex=TRUE))

indices$Country.Code=substr(indices$.id,1,3)
indices$Indicator.Code=substring(indices$.id,5,)
indices$Indicator.Code=sub(".index","",indices$Indicator.Code)
indices$.id <- NULL 
indices <- pivot(indices,how="l",id=c("Indicator.Code","Country.Code"),names=list("outlier","index"))

replacements <- unlist2d(get_elem(tsoutliers,"replacements",regex=TRUE))
replacements$Country.Code=substr(replacements$.id,1,3)
replacements$Indicator.Code=substring(replacements$.id,5,)
replacements$Indicator.Code=sub(".replacements","",replacements$Indicator.Code)
replacements$.id <- NULL 
replacements <- pivot(replacements,how="l",id=c("Indicator.Code","Country.Code"),names=list("outlier","replacement"))

outliers <- cbind(indices,replacement=replacements[,"replacement"])
outliers <- outliers[!is.na(outliers$index),]
outliers <- roworder(outliers,"Country.Code","Indicator.Code","index")

wdi_df_norm_long_2$ones = 1 
wdi_df_norm_long_2$index = fcumsum(wdi_df_norm_long_2$ones,g=wdi_df_norm_long_2[,c("Country.Code","Indicator.Code")])
wdi_df_norm_long_2$ones = NULL 

wdi_tsoutliers <- join(wdi_df_norm_long_2,outliers,on=c("Country.Code","Indicator.Code","index"),validate="1:1")
wdi_tsoutliers <- wdi_tsoutliers[!is.na(wdi_tsoutliers$outlier),]
wdi_tsoutliers$magnitude <- abs(wdi_tsoutliers$Zscore-wdi_tsoutliers$replacement)
wdi_tsoutliers <- roworder(wdi_tsoutliers,-magnitude)
wdi_tsoutliers$tsoutlier_rank <- seq(1:nrow(wdi_tsoutliers))
head(wdi_tsoutliers,30)

saveRDS(wdi_tsoutliers,"WDI_tsoutliers")




