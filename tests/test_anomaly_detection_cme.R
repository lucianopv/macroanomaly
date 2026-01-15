
# library(devtools)
# install_github("unicef-drp/unicefData")

library(macroanomaly)
library(collapse)
library(tidyr)
library(dplyr)
library(unicefData)

setwd("C:/Users/WB147665/OneDrive - WBG/_OD/ANOMALY/UNICEF")

mor <- search_indicators("mortality")

df <- data.frame()

for(i in 1:nrow(mor)) {
  dfi <- unicefData(
    indicator = mor$code[i],
    latest = F,
    dropna = TRUE
  )
  df <- rbind(df, dfi)
}

# If the indicator has dimensions, use them to create a unique indicator ID
if(length(table(df$sex)) > 1) df$indicator = paste0(df$indicator, df$sex)
if(length(table(df$age)) > 1) df$indicator = paste0(df$indicator, df$age)
if(length(table(df$wealth_quintile)) > 1) df$indicator = paste0(df$indicator, df$wealth_quintile)
if(length(table(df$residence)) > 1) df$indicator = paste0(df$indicator, df$residence)
if(length(table(df$maternal_edu_lvl)) > 1) df$indicator = paste0(df$indicator, df$maternal_edu_lvl)

print(paste("Observation in source dataset:", nrow(df)))

min_obs = 10
valid_obs <- df %>%
  group_by(iso3, indicator) %>%
  summarise(non_missing = sum(!is.na(value)), .groups = "drop")
not_enough_obs <- valid_obs[valid_obs$non_missing < min_obs, ]
df <- anti_join(df, not_enough_obs, by = c("iso3", "indicator"))

# Number of observations, geographies, and indicators in the selected dataset
print(paste("Nb of indicators in dataset:", length(unique(df$indicator))))
print(paste("Nb of geographies in dataset:", length(unique(df$iso3))))
print(paste("Nb of observations:", nrow(df)))

# Normalize the dataset (detrend, impute missing values)
df |>
  normalize(.value_col= "value",
            .country_col = c("iso3"),
            .indicator_col = "indicator",
            .time_col = "period",
            .detrend = TRUE, 
            .impute = TRUE) -> df_normalized

# Detect anomalies (5 methods)  

#"tsoutlier" --> Error message

df_normalized |>
  detect(.method = c("zscore", "capa", "isotree"), 
         .args = list(capa = c(.min_seg_len=3), isotree = c(.threshold=0.7)),
         .additional_cols = TRUE) -> cme_anomalies

cme_anomalies <- cme_anomalies[
  order(-cme_anomalies$outlier_indicator_total,
        -cme_anomalies$absZscore_zscore), ]

# Number of anomalies detected
summary(cme_anomalies)
table(cme_anomalies$outlier_indicator_total)

# Save output to CSV 
fn <- paste0("UNICEF_CME_ANOMALIES", "_", format(Sys.Date(), "%Y-%m-%d"), ".CSV")
write.csv(wdi_SE_anomalies, file = fn, row.names = FALSE)

# Plot a sample of anomalies 
for(i in 1:20) {
  if(cme_anomalies$Imputed[i] == FALSE) {
    subttl <- paste0(cme_anomalies$country[i], " - ", 
                     cme_anomalies$indicator_name[i])
    print(plot(cme_anomalies, 
               country = cme_anomalies$iso3[i], 
               indicator = cme_anomalies$indicator[i],
               .total_threshold = 2, x.lab = subttl))
  }
}

# Code to plot one selected anomaly (provide country code and indicator code)
plot(cme_anomalies, 
     country = "UKR",
     indicator = "CME_MRY1T4",   
     .total_threshold = 2, x.lab = "Year")
