# ------------------------------------------------------------------------------
# Anomaly detection based on outlier trees 
# ------------------------------------------------------------------------------

library(isotree)
library(reshape2)
library(rlist)
library(ggplot2)

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
wdi_df <- melt(wdi_df_ori, 
               id.vars = c("Country.Name", "Country.Code", "Indicator.Code",
                           "Indicator.Name"), variable.name = "Year")

wdi_df <- dcast(wdi_df, Country.Name + Country.Code + Year ~ Indicator.Code)
wdi_df[, "Year"] <- substring(wdi_df[, "Year"], 2, 6)
saveRDS(wdi_df, "WDI_df")
}


wdi_df <- readRDS("wdi_df_long") # wdi_df_long created in wdi_anomaly_trees script 

# Estimate isolation forests  
# ---------------------
#wdi_df$indicator_country <- paste0(wdi_df$Country.Code,".",wdi_df$Indicator.Code)
#wdi_df <- merge(x=wdi_df,y=wdi_df[wdi_df$Indicator.Name=="GDP per capita (constant 2015 US$)",c("value","Country.Code","Year")],by=c("Country.Code","Year"))
wdi_df$Year=as.numeric(substr(wdi_df$Year,2,6))



#wdi_df$Country.Name <- NULL
#wdi_df$Indicator.Name <- NULL



wdi_isoforest_cells <- isolation.forest(wdi_df[,-c(2,3)],    
                                    ndim=1, sample_size=32,
                                    prob_pick_pooled_gain=1,
                                    ntrees=100,
                                output_dist = F
  )
  




# Look at detected outliers
# -------------------------
# for format where observation is country-year
outlier_scores <- predict(object=wdi_isoforest_cells, wdi_df[-c(2,3)],type="score")
wdi_df_outliers_cells <- data.frame(WDI_order = 1:length(outlier_scores),outlier_scores,wdi_df)

wdi_df_outliers_cells <- wdi_df_outliers_cells[with(wdi_df_outliers_cells, order(outlier_scores,decreasing=T)), ]
wdi_df_outliers_cells$isoforest_rank[!is.na(wdi_df_outliers_cells$outlier_score)]<-1:nrow(wdi_df_outliers_cells)
saveRDS(wdi_df_outliers_cells[,c("WDI_order","isoforest_rank")],"WDI_isoforest_data")
write.csv(wdi_df_outliers_cells, file = "WDI_isoforest_cells.csv", na = "", row.names = F) 
rm(outlier_scores)
#for format where observation is country-indicator 

