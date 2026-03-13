
library(macroanomaly)
library(collapse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(pracma)
library(tictoc)

setwd("C:/Users/WB147665/OneDrive - WBG/_OD/ANOMALY/WDI")

# Download the WDI dataset, format, and filter observations
if(!file.exists("WDICSV.CSV")) {
  download.file("https://databank.worldbank.org/data/download/WDI_CSV.zip",
                destfile = "WDI.zip")
  unzip("WDI_CSV.zip")
}

df_wdi <- read.csv("WDICSV.csv")

df_wdi <- df_wdi %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year", 
    values_to = "Value" 
  ) %>%
  mutate(Year = as.integer(sub("X", "", Year))) 

df_wdi <- as.data.frame(df_wdi)

wdi_indics <- unique(df_wdi[c("Indicator.Code", "Indicator.Name")])
write.csv(wdi_indics, file = "indicators_codes_names.csv")

wdi_countries <- unique(df_wdi[c("Country.Code", "Country.Name")])
write.csv(wdi_countries, file = "countries_codes_names.csv")

df_wdi <- df_wdi[df_wdi$Year >= 1980, ]

# Drop series with insufficient number of non-missing values
min_obs = 10
nb_valid_values <- df_wdi %>%
  group_by(Country.Code, Indicator.Code) %>%
  summarise(non_missing_values = sum(!is.na(Value)), .groups = "drop")
not_enough_values <- nb_valid_values[nb_valid_values$non_missing_values < min_obs, ]
df_wdi <- anti_join(df_wdi, 
                    not_enough_values, 
                    by = c("Country.Code", "Indicator.Code"))

# Normalize the dataset
df_wdi |>
  normalize(.value_col= "Value",
            .country_col = c("Country.Code", "Country.Name"),
            .indicator_col = "Indicator.Code",
            .time_col = "Year",
            .detrend = TRUE, .impute = TRUE) -> wdi_normalized

saveRDS(wdi_normalized, "wdi_normalized.rds")
#wdi_normalized <- readRDS("wdi_normalized.rds")

tic("Hampel filter on WDI")

wdi_hampel <- wdi_normalized %>%
  group_by(Indicator.Code, Country.Code) %>%
  arrange(Year, .by_group = TRUE) %>%
  group_modify(~{
    h <- pracma::hampel(.x$Value, k = 5, t0 = 5)
      .x %>%
      mutate(
        Value_hampel = h$y,
        hampel_outlier = seq_along(Value) %in% h$ind
      )
  }) %>%
  ungroup()

toc()

table(wdi_hampel$hampel_outlier)
table(wdi_hampel$hampel_outlier & wdi_hampel$Imputed == FALSE)

df_anomalies <- wdi_hampel %>%
  group_by(Country.Code, Indicator.Code) %>%
  summarise(n_outliers = sum(hampel_outlier, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(n_outliers), Country.Code, Indicator.Code)

for(i in 1:10) {
  
  df_plot <- wdi_hampel %>%
    filter(Country.Code == df_anomalies$Country.Code[i],
           Indicator.Code == df_anomalies$Indicator.Code[i])
  
  print(ggplot(df_plot, aes(x = Year, y = Value)) +
          geom_point(aes(color = hampel_outlier), size = 3) +
          scale_color_manual(
            values = c(`FALSE` = "black", `TRUE` = "red"),
            labels = c(`FALSE` = "Non outlier", `TRUE` = "Outlier"),
            name = NULL
          ) +
          labs(
            title = paste0(unique(df_plot$Country.Name), " - ", unique(df_plot$Indicator.Name)),
            subtitle = paste0(unique(df_plot$Country.Code), " - ", unique(df_plot$Indicator.Code)),
            x = "Year", y = "Value"
          ) +
          theme_minimal())
  
}

# Remove some false positive outliers - This is a basic approach that would have to 
# be tested and improved.

wdi_hampel_2 <- wdi_hampel %>%
  group_by(Indicator.Code, Country.Code) %>%
  mutate(
    mean_non_out = mean(Value[!hampel_outlier], na.rm = TRUE),
    sd_non_out   = sd(Value[!hampel_outlier], na.rm = TRUE),
    hampel_outlier = ifelse(
      Value >= (mean_non_out - sd_non_out) &
        Value <= (mean_non_out + sd_non_out),
      FALSE,
      hampel_outlier
    )
  ) %>%
  select(-mean_non_out, -sd_non_out) %>%
  ungroup()

table(wdi_hampel_2$hampel_outlier)
table(wdi_hampel_2$hampel_outlier & wdi_hampel$Imputed == FALSE)

# Plot for the same series as original Hampel filter results

for(i in 1:10) {
    
  df_plot <- wdi_hampel_2 %>%
    filter(Country.Code == df_anomalies$Country.Code[i],
           Indicator.Code == df_anomalies$Indicator.Code[i])
  
  print(ggplot(df_plot, aes(x = Year, y = Value)) +
    geom_point(aes(color = hampel_outlier), size = 3) +
    scale_color_manual(
      values = c(`FALSE` = "black", `TRUE` = "red"),
      labels = c(`FALSE` = "Non outlier", `TRUE` = "Outlier"),
      name = NULL
    ) +
    labs(
      title = paste0(unique(df_plot$Country.Name), " - ", unique(df_plot$Indicator.Name)),
      subtitle = paste0("Trimmed -", unique(df_plot$Country.Code), " - ", unique(df_plot$Indicator.Code)),
      x = "Year", y = "Value"
    ) +
    theme_minimal())

}
