# ------------------------------------------------------------------------------
# Anomaly detection based on outlier trees 
# ------------------------------------------------------------------------------

library(outliertree)
library(reshape2)
library(rlist)
library(ggplot2)
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
wdi_df_long <- melt(wdi_df_ori, 
               id.vars = c("Country.Name", "Country.Code", "Indicator.Code",
                           "Indicator.Name"), variable.name = "Year")
wdi_df_long <- roworder(wdi_df_long,Country.Name,Year,Indicator.Code)
saveRDS(wdi_df_long,"wdi_df_long")
wdi_df[, "Year"] <- substring(wdi_df[, "Year"], 2, 6)
wdi_df <- dcast(wdi_df_long, Country.Name + Country.Code + Year ~ Indicator.Code)

saveRDS(wdi_df, "WDI_df")
}

# Estimate outlier tree 
# ---------------------
rm(list=ls())
wdi_df <- readRDS("WDI_df")
wdi_df$Year <- as.numeric(wdi_df$Year)
if(!file.exists("wdi_outliers_tree_object")) {
wdi_outliers_tree <- outlier.tree(wdi_df, save_outliers = TRUE, cols_ignore="Year",nthreads = 1)
saveRDS(wdi_outliers_tree, file = "wdi_outliers_tree_object")
}

# Look at detected outliers
# -------------------------
wdi_outliers_tree <- readRDS(file = "wdi_outliers_tree_object")
lst <- outliertree:::list.to.outliers(wdi_outliers_tree$outliers_data)
# This needs to go from 1 to 17024 
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
#wdi_df_outliers <- merge(df_outlierness,outliers,by="WDI_order",all.y=T)  #12535 rows, one per outlier found 
wdi_df_outliers$outlier_tree_rank <- NA 

wdi_df_outliers = roworder(wdi_df_outliers,uses_NA_branch,tree_depth, outlier_score)

#wdi_df_outliers <- wdi_df_outliers[with(wdi_df_outliers, order(uses_NA_branch, 
#                                                            tree_depth, outlier_score)), ]

wdi_df_outliers[1:sum(!is.na(wdi_df_outliers$outlier_score)),"outlier_tree_rank"]<-1:sum(!is.na(wdi_df_outliers$outlier_score))
 


wdi_df_long <- readRDS("wdi_df_long")
wdi_df_long$Year <- as.numeric(substr(wdi_df_long$Year,2,5))

wdi_df_outliers$WDI_order <- NULL 


wdi_df_outliers_long <- join(wdi_df_long,wdi_df_outliers,on=c("Country.Name","Year","Indicator.Code"),how="full",validate="1:1",column="merge",overid=2)




saveRDS(wdi_df_outliers_long[,c("Country.Name","Year","Indicator.Code","outlier_tree_rank")],"WDI_outliers_trees_data")

write.csv(wdi_df_outliers, file = "WDI_outliers_trees.csv", na = "", row.names = F)      




