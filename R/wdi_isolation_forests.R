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
saveRDS(wdi_df_ori,"WDI_df_countryindicator")
wdi_df <- melt(wdi_df_ori, 
               id.vars = c("Country.Name", "Country.Code", "Indicator.Code",
                           "Indicator.Name"), variable.name = "Year")
wdi_df <- dcast(wdi_df, Country.Name + Country.Code + Year ~ Indicator.Name)
wdi_df[, "Year"] <- substring(wdi_df[, "Year"], 2, 6)
saveRDS(wdi_df, "WDI_df")
}

# Estimate isolation forests  
# ---------------------
wdi_df <- readRDS("WDI_df")
if(!file.exists("wdi_isolation_forest")) {
  wdi_isoforest_countryyear <- isolation.forest(wdi_df,    
                                    ndim=1, sample_size=32,
                                    prob_pick_pooled_gain=1,
                                    ntrees=100,
                                #output_dist = TRUE
  )
}


wdi_df_countryindicator <- readRDS("WDI_df_countryindicator")
wdi_isoforest_countryindicator <- isolation.forest(wdi_df_countryindicator,    
                                                   ndim=1, sample_size=32,
                                                   prob_pick_pooled_gain=1,
                                                   ntrees=100,
                                                   #output_dist = TRUE
)


# Look at detected outliers
# -------------------------
# for format where observation is country-year
outlier_scores <- predict(object=wdi_isoforest_countryyear, wdi_df,type="score")
wdi_df_outliers_countryyear <- data.frame(outlier_scores,wdi_df)
wdi_df_outliers_countryyear <- wdi_df_outliers_countryyear[with(wdi_df_outliers_countryyear, order(outlier_scores,decreasing=T)), ]
write.csv(wdi_df_outliers_countryyear, file = "WDI_isoforest_countryyear.csv", na = "", row.names = F) 
rm(outlier_scores)
#for format where observation is country-indicator 

# Now repeat when unit of observation is country-indicator 

outlier_scores <- predict(object=wdi_isoforest_countryindicator, wdi_df_countryindicator,type="score")
wdi_df_outliers_countryindicator <- data.frame(outlier_scores,wdi_df_countryindicator)
wdi_df_outliers_countryindicator <- wdi_df_outliers_countryindicator[with(wdi_df_outliers_countryindicator, order(outlier_scores,decreasing=T)), ]


write.csv(wdi_df_outliers_countryindicator, file = "WDI_isoforest_countryindicator.csv", na = "", row.names = F)






