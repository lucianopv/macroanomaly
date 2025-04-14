# ------------------------------------------------------------------------------
# Anomaly detection based on outlier trees 
# ------------------------------------------------------------------------------

library(reshape2)
library(rlist)
library(ggplot2)
library(outliertree)
library(isotree)
library(collapse)


#setwd("C:/Users/WB147665/OneDrive - WBG/_OD/WDI_ANOMALY")
setwd("D:/David/outlier detection")

# Download the WDI data
# ---------------------

url <- "https://databank.worldbank.org/data/download/WDI_CSV.zip"
wdi_csv <- "WDICSV.csv"
if(!file.exists(wdi_csv)) {
  download.file(url, "wdi.zip", mode = "wb")
  unzip("wdi.zip")
  file.remove("wdi.zip")
}


# Prepare the data file
# ---------------------
if(!file.exists("WDI_df")) {
wdi_df_ori <- read.csv(wdi_csv)
saveRDS(wdi_df_ori,"WDI_df_countryindicator")
wdi_df <- melt(wdi_df_ori, 
               id.vars = c("Country.Name", "Country.Code", "Indicator.Code",
                           "Indicator.Name"), variable.name = "Year")

wdi_df <- dcast(wdi_df, Country.Name + Country.Code + Year ~ Indicator.Code)
wdi_df[, "Year"] <- substring(wdi_df[, "Year"], 2, 6)
saveRDS(wdi_df, "WDI_df")
}

#start here 
rm(list=ls())
wdi_df <- readRDS("wdi_df")



# Standardize wdi 
# ---------------------


# use robust scaling  method for normalization, subtract median and divide by IQR  
#wdi_df_med <- fmedian(wdi_df[,4:1499],g=wdi_df[,2],TRA=1)
#wdi_df_p25 <- fnth(wdi_df[,4:1499],n=0.25,g=wdi_df[,2],TRA=1) 
#wdi_df_p75 <- fnth(wdi_df[,4:1499],n=0.75,g=wdi_df[,2],TRA=1) 
#rm(list="wdi_df_med","wdi_df_p25","wdi_df_p75")

# a robust scaling method that relies on median absolute deviation from the median
#wdi_df_med <- fmedian(wdi_df[,4:1499],g=wdi_df[,2],TRA=1)
#wdi_df_mad <- abs(wdi_df[,4:1499]-wdi_df_med)
#wdi_df_mad <- fmedian(wdi_df_mad,g=wdi_df[,2],TRA=1)
#wdi_df_norm <- (wdi_df[,4:1499]-wdi_df_med)/wdi_df_mad 
#rm(list="wdi_df_mad","wdi_df_med")
#wdi_df_norm <- replace(wdi_df_norm,wdi_df_norm==Inf,NA)

#Leave one out mean and SD 
#wdi_df_mean <- fmean(wdi_df[,4:1499],g=wdi_df[,2],TRA=1)
#wdi_df_count <- fnobs(wdi_df[,4:1499],g=wdi_df[,2],TRA=1)
#wdi_df_var <- fvar(wdi_df[,4:1499],g=wdi_df[,2],TRA=1)

#wdi_df_loomean <- (wdi_df_mean*wdi_df_count-wdi_df[,4:1499])/(wdi_df_count-1)
#wdi_df_loosd <- ((wdi_df_var-(wdi_df[,4:1499]-wdi_df_mean)^2)*wdi_df_count/(wdi_df_count-1))^0.5 

#wdi_df_norm <- (wdi_df[,4:1499]-wdi_df_loomean)/(wdi_df_loosd)
#wdi_df_norm <- replace(wdi_df_norm,wdi_df_loosd<1e-13,NA)

# for constant series, set standardized values to NA manually (fscale has a bug that will divide by a very small sd)
# This can create  variation in the Z scoress when none actually exist. 
wdi_df_norm <- fscale(wdi_df[,4:1499],g=wdi_df[,2]) # Simple normalization 
wdi_df_norm_sd <- fsd(wdi_df[,4:1499],g=wdi_df[,2],TRA=1)
wdi_df_norm <- replace(wdi_df_norm,wdi_df_norm_sd<1e-13,NA)





#wdi_df_norm <- (wdi_df[,4:1499]-wdi_df_norm_mean)/wdi_df_norm_sd
wdi_df_norm <- cbind(wdi_df[1:3],wdi_df_norm) 
saveRDS(wdi_df_norm,"wdi_df_norm_nosmooth")

# apply smoother to each country and column
wdi_df_norm$t <- as.integer(wdi_df_norm$Year)-1959

detrend <- function(Y=Y,t=t) {
  Nnonmiss <- fsum(is.finite(Y))
  output <- rep(NA,length(Y)) 
    if (Nnonmiss>0) {
  S=supsmu(x=t, y=Y)  
  Yhat <- unlist2d(S$y)
  output[is.finite(Y)] <- Y[is.finite(Y)]-Yhat
  }
  return(output)
}


wdi_df_norm <- cbind(wdi_df_norm[,1:3],BY(x=wdi_df_norm[4:1499],detrend,g=wdi_df_norm[,"Country.Code"],t=wdi_df_norm[,1500]))
saveRDS(wdi_df_norm,"wdi_df_norm")
  
wdi_df_norm_long <- melt(wdi_df_norm, 
     id.vars = c("Country.Name", "Country.Code",
                 "Year"), variable.name = "Indicator.Code")
colnames(wdi_df_norm_long)[5] <- "Zscore"
wdi_df_norm_long$Zscore[abs(wdi_df_norm_long$Zscore)==Inf] <- NA 
saveRDS(wdi_df_norm_long,"wdi_df_norm_long")
#fsubset(wdi_df_norm_long,Indicator.Code=="ER.H2O.INTR.K3" & Country.Code=="MEA")


rm(list=ls())
wdi_df_norm_long <- readRDS("wdi_df_norm_long")


# Estimate isolation  forest on normalized detrended values 
wdi_df_norm_long$Year <- as.numeric(wdi_df_norm_long$Year)
model <- isolation.forest(wdi_df_norm_long[,-1], ndim=1, ntrees=10, nthreads=2)
scores <- predict(model, wdi_df_norm_long[,-1], type="score")
wdi_df_norm_long <- cbind(wdi_df_norm_long,scores)
wdi_df_norm_long$norm_isoforest_rank <- NA 
wdi_df_norm_long <- roworder(wdi_df_norm_long,-scores)
wdi_df_norm_long[1:sum(!is.na(wdi_df_norm_long$scores)),"norm_isoforest_rank"] <- 1:sum(!is.na(wdi_df_norm_long$scores))

#Generate rank variable based on descending order of Zscore 
wdi_df_norm_long$Zscore_rank <- NA 
wdi_df_norm_long$absZscore <- abs(wdi_df_norm_long$Zscore)
wdi_df_norm_long <- roworder(wdi_df_norm_long,-absZscore)
wdi_df_norm_long[1:sum(!is.na(wdi_df_norm_long$"Zscore")),"Zscore_rank"] <- 1:sum(!is.na(wdi_df_norm_long$"Zscore"))





saveRDS(wdi_df_norm_long[,c("Country.Name","Year","Indicator.Code","Zscore","Zscore_rank","norm_isoforest_rank")],"norm_Zscore_isoforest")

write.csv(wdi_df_norm_long, file = "WDI_outliers_Zscore_isoforest.csv", na = "", row.names = F)                         

# stopped here 
# Now try using a outlier tree on the standardized and values 

rm(list=ls())
wdi_df_norm <- readRDS("wdi_df_norm")



#if(!file.exists("wdi_outliers_tree_norm_object")) {
wdi_outliers_tree_norm  <- outlier.tree(wdi_df_norm, save_outliers = TRUE, cols_ignore="Year", nthreads = 2)
saveRDS(wdi_outliers_tree_norm, file = "wdi_outliers_tree_norm_object")
#} 


# Look at detected outliers
# -------------------------
wdi_outliers_tree_norm <- readRDS(file = "wdi_outliers_tree_norm_object")
lst <- outliertree:::list.to.outliers(wdi_outliers_tree_norm$outliers_data)
df_outlierness <- data.frame(WDI_order = 1:nrow(wdi_df_norm), 
                             uses_NA_branch = lst$uses_NA_branch, tree_depth = lst$tree_depth, 
                             outlier_score = lst$outlier_score, Country.Name=wdi_df_norm$Country.Name,Year=wdi_df_norm$Year
)

outliers <- as.data.frame(do.call(rbind, lst$suspicous_value))[,-3] ## drop decimals column 
outliers$value<-unlist(outliers$value)
outliers$Indicator.Code <- unlist(outliers$column)
outliers$column <- NULL 

outliers <- cbind(outliers,WDI_order=rownames(outliers)) ## add WDI index numbers which are stored in row names 

# outliers is a subset of the wide form 
outliers$WDI_order <- as.numeric(outliers$WDI_order)

wdi_df_outliers <-join(outliers,df_outlierness,on="WDI_order")
#wdi_df_outliers <- merge(df_outlierness,outliers,by="WDI_order",all=T) 
wdi_df_outliers$norm_outlier_tree_rank <- NA 
wdi_df_outliers <- wdi_df_outliers[with(wdi_df_outliers, order(uses_NA_branch, 
                                                               tree_depth, outlier_score)), ]
wdi_df_outliers[1:sum(!is.na(wdi_df_outliers$outlier_score)),"norm_outlier_tree_rank"]<-1:sum(!is.na(wdi_df_outliers$outlier_score))
wdi_df_outliers$Year <- as.numeric(wdi_df_outliers$Year)


wdi_df_long <- readRDS("wdi_df_long")
wdi_df_long$Year <- as.numeric(substr(wdi_df_long$Year,2,6))

wdi_df_outliers$WDI_order <- NULL 
wdi_df_outliers_long <- join(wdi_df_long,wdi_df_outliers,on=c("Country.Name","Year","Indicator.Code"),how="full",validate="1:1",column="merge",overid=2)
saveRDS(wdi_df_outliers_long[,c("Country.Name","Year","Indicator.Code","norm_outlier_tree_rank")],"WDI_norm_outlier_tree_data")

write.csv(wdi_df_outliers, file = "WDI_outliers_trees.csv", na = "", row.names = F)      




