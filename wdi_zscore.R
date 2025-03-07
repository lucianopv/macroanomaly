# ------------------------------------------------------------------------------
# Anomaly detection based on outlier trees 
# ------------------------------------------------------------------------------


library(reshape2)
library(rlist)
library(ggplot2)
library(outliertree)
library(isotree)


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

# Normalize wdi 
# ---------------------
#wdi_df_backup <- wdi_df 

wdi_df_norm <- ave(wdi_df[,4:1499],wdi_df[,2],FUN=scale)
saveRDS(wdi_df_norm,"wdi_df_norm")


wdi_df_norm <- readRDS("wdi_df_norm")
 
wdi_df_norm <- data.frame(wdi_df[,1:3],wdi_df_norm)
wdi_df_norm_long <- melt(wdi_df_norm, 
     id.vars = c("Country.Name", "Country.Code",
                 "Year"), variable.name = "Indicator.Code")
colnames(wdi_df_norm_long)[5] <- "Zscore"

wdi_df_norm_long$Zscore_rank <- NA 
wdi_df_norm_long <- wdi_df_norm_long[order(abs(wdi_df_norm_long$"Zscore"),decreasing=T),]
wdi_df_norm_long[1:sum(!is.na(wdi_df_norm_long$"Zscore")),"Zscore_rank"] <- 1:sum(!is.na(wdi_df_norm_long$"Zscore"))



#head(wdi_df_norm_long,40)
#write.csv(wdi_df_norm_long_sort, file = "WDI_outliers_zscore.csv", na = "", row.names = F)                         

# Try isolation  forest on the standardized values 
wdi_df_norm_long$Year <- as.numeric(wdi_df_norm_long$Year)
model <- isolation.forest(wdi_df_norm_long[,-1], ndim=1, ntrees=10, nthreads=1)
scores <- predict(model, wdi_df_norm_long[,-1], type="score")
wdi_df_norm_long <- data.frame(wdi_df_norm_long,scores)
wdi_df_norm_long$WDI_order <- 1:nrow(wdi_df_norm_long)
wdi_df_norm_long_sort <- wdi_df_norm_long[order(wdi_df_norm_long$scores,decreasing=T),]
wdi_df_norm_long_sort$norm_isoforest_rank <- NA 
wdi_df_norm_long_sort[1:sum(!is.na(wdi_df_norm_long$scores)),"norm_isoforest_rank"] <- 1:sum(!is.na(wdi_df_norm_long$scores))
saveRDS(wdi_df_norm_long_sort[,c("WDI_order","Zscore_rank","norm_isoforest_rank")],"norm_zscore_isoforest")

write.csv(wdi_df_norm_long, file = "WDI_outliers_zscore_isoforest.csv", na = "", row.names = F)                         



# Now try using a outlier tree on the standardized values 

if(!file.exists("wdi_outliers_tree_norm_object")) {
wdi_outliers_tree_norm  <- outlier.tree(wdi_df_norm, save_outliers = TRUE, cols_ignore="Year", nthreads = 1)
saveRDS(wdi_outliers_tree_norm, file = "wdi_outliers_tree_norm_object")
} 


# Look at detected outliers
# -------------------------
wdi_outliers_trees_norm <- readRDS(file = "wdi_outliers_tree_norm_object")
lst <- outliertree:::list.to.outliers(wdi_outliers_tree_norm$outliers_data)
df_outlierness <- data.frame(WDI_order = 1:nrow(wdi_df), 
                             uses_NA_branch = lst$uses_NA_branch, tree_depth = lst$tree_depth, 
                             outlier_score = lst$outlier_score, Country.Name=wdi_df$Country.Name,year=wdi_df$Year
)

outliers <- as.data.frame(do.call(rbind, lst$suspicous_value))[,-3] ## drop decimals column 
outliers$value<-unlist(outliers$value)
outliers$Indicator.Code <- unlist(outliers$column)
outliers$column <- NULL 

outliers <- cbind(outliers,WDI_order=rownames(outliers)) ## add WDI index numbers which are stored in row names 

# outliers is a subset of the wide form 

outliers$WDI_order <- as.numeric(outliers$WDI_order)
wdi_df_outliers <- merge(df_outlierness,outliers,by="WDI_order",all=T) 
wdi_df_outliers$outlier_tree_rank <- NA 
wdi_df_outliers <- wdi_df_outliers[with(wdi_df_outliers, order(uses_NA_branch, 
                                                               tree_depth, outlier_score)), ]
wdi_df_outliers[1:sum(!is.na(wdi_df_outliers$outlier_score)),"outlier_tree_rank"]<-1:sum(!is.na(wdi_df_outliers$outlier_score))
wdi_df_outliers$year <- as.numeric(wdi_df_outliers$year)


wdi_df_long <- readRDS("wdi_df_long")
wdi_df_long$WDI_order <- 1:nrow(wdi_df_long)
wdi_df_long$year <- as.numeric(wdi_df_long$Year)
wdi_df_long$Year <- NULL 

wdi_df_outliers$WDI_order <- NULL 

wdi_df_outliers_long <- merge(wdi_df_long,wdi_df_outliers,by=c("Country.Name","year","Indicator.Code","value"),all.x=T)
saveRDS(wdi_df_outliers_long[,c("WDI_order","outlier_tree_rank")],"WDI_norm_outlier_tree_data")

write.csv(wdi_df_outliers, file = "WDI_outliers_trees.csv", na = "", row.names = F)      




