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
#wdi_df[, "Year"] <- substring(wdi_df[, "Year"], 2, 6)

# Standardize wdi 
# ---------------------
wdi_df_norm_mean <- fmean(wdi_df[,4:1499],g=wdi_df[,2],TRA=1)
wdi_df_norm_sd <- fsd(wdi_df[,4:1499],g=wdi_df[,2],TRA=1)
wdi_df_norm <- cbind(wdi_df[,1:3],(wdi_df[,4:1499]-wdi_df_norm_mean)/wdi_df_norm_sd)

#wdi_df_norm_check <- fmean(wdi_df_norm[,4:1499],g=wdi_df_norm[,2],TRA=1) 
#wdi_df_norm_check2 <- fsd(wdi_df_norm[,4:1499],g=wdi_df_norm[,2],TRA=1) 

wdi_df_norm_long <- melt(wdi_df_norm, 
     id.vars = c("Country.Name", "Country.Code",
                 "Year"), variable.name = "Indicator.Code")
colnames(wdi_df_norm_long)[5] <- "Zscore"
wdi_df_norm_long$Zscore[abs(wdi_df_norm_long$Zscore)==Inf] <- NA 
saveRDS(wdi_df_norm_long,"wdi_df_norm_long")

wdi_df_norm_long$Zscore_rank <- NA 
wdi_df_norm_long$absZscore <- abs(wdi_df_norm_long$Zscore)
wdi_df_norm_long <- roworder(wdi_df_norm_long,-absZscore)
wdi_df_norm_long[1:sum(!is.na(wdi_df_norm_long$"Zscore")),"Zscore_rank"] <- 1:sum(!is.na(wdi_df_norm_long$"Zscore"))

# Estimate isolation  forest on the standardized values 
wdi_df_norm_long$Year <- as.numeric(wdi_df_norm_long$Year)
model <- isolation.forest(wdi_df_norm_long[,-1], ndim=1, ntrees=10, nthreads=1)
scores <- predict(model, wdi_df_norm_long[,-1], type="score")
wdi_df_norm_long <- cbind(wdi_df_norm_long,scores)
wdi_df_norm_long$norm_isoforest_rank <- NA 
wdi_df_norm_long <- roworder(wdi_df_norm_long,-scores)
wdi_df_norm_long[1:sum(!is.na(wdi_df_norm_long$scores)),"norm_isoforest_rank"] <- 1:sum(!is.na(wdi_df_norm_long$scores))
saveRDS(wdi_df_norm_long[,c("Country.Name","Year","Indicator.Code","Zscore_rank","norm_isoforest_rank")],"norm_zscore_isoforest")

write.csv(wdi_df_norm_long, file = "WDI_outliers_zscore_isoforest.csv", na = "", row.names = F)                         



# Now try using a outlier tree on the standardized values 

if(!file.exists("wdi_outliers_tree_norm_object")) {
wdi_outliers_tree_norm  <- outlier.tree(wdi_df_norm, save_outliers = TRUE, cols_ignore="Year", nthreads = 1)
saveRDS(wdi_outliers_tree_norm, file = "wdi_outliers_tree_norm_object")
} 


# Look at detected outliers
# -------------------------
wdi_outliers_tree_norm <- readRDS(file = "wdi_outliers_tree_norm_object")
lst <- outliertree:::list.to.outliers(wdi_outliers_tree_norm$outliers_data)
df_outlierness <- data.frame(WDI_order = 1:nrow(wdi_df), 
                             uses_NA_branch = lst$uses_NA_branch, tree_depth = lst$tree_depth, 
                             outlier_score = lst$outlier_score, Country.Name=wdi_df$Country.Name,Year=wdi_df$Year
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




