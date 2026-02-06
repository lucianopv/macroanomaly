
# Anomaly detection in WHO health expenditure GHED dataset

# Dataset in xlsx downloaded from:
# https://apps.who.int/nha/database/Select/Indicators/en

library(macroanomaly)
library(readxl)
library(collapse)
library(dplyr)
library(tidyr)
library(ggplot2)

# setwd("C:/Users/WB147665/OneDrive - WBG/_OD/ANOMALY/WHO")

df_ind <- read_xlsx("inst/extdata/GHED_data.XLSX", sheet = "Codebook")
df_ind <- df_ind[, 1:2]
names(df_ind)[1] <- "indicator"
names(df_ind)[2] <- "indicator_name"

df_who <- read_xlsx("inst/extdata/GHED_data.XLSX", sheet = "Data", na = "")
df_who <- df_who |>
  pivot_longer(
    cols = 6:last_col(),
    names_to = "variable code",
    values_to = "value"
  )
names(df_who)[6] <- "indicator"
df_who <- df_who[order(df_who$location, df_who$year), ]

# Drop series with insufficient number of non-missing values
min_obs = 8
nb_valid_values <- df_who %>%
  group_by(code, indicator) %>%
  summarise(non_missing_values = sum(!is.na(value)), .groups = "drop")
not_enough_values <- nb_valid_values[nb_valid_values$non_missing_values < min_obs, ]
df_who <- anti_join(df_who, not_enough_values, by = c("code", "indicator"))

# df_who <- merge(df_who, df_ind, by = "indicator", all.x = TRUE)
# df_who <- df_who[, c(1, ncol(df_who), 2:(ncol(df_who) - 1))]

df_who <- df_who %>%
  left_join(df_ind, by = "indicator")

# Normalize the dataset
df_who |>
  normalize(.value_col= "value",
            .country_col = c("code"),
            .indicator_col = "indicator",
            .time_col = "year",
            .detrend = TRUE, .impute = TRUE) -> who_normalized

df_who |>
  normalize(.value_col= "value",
            .country_col = c("code", "region", "location"),
            .indicator_col = c("indicator", "indicator_name"),
            .time_col = "year",
            .detrend = TRUE, .impute = TRUE) -> who_normalized_alt

# Detect anomalies
who_normalized |>
  detect(.method = c("tsoutlier", "isotree", "capa", "outliertree", "zscore"),
         .args = list(capa = c(.min_seg_len=3), isotree = c(.threshold=0.7),
                      outliertree = list(.cols = c("Zscore", "location", "indicator", "year", "region"))),
         .additional_cols = TRUE) -> who_anomalies

# ---- run without capa and outliertree which return error messages
# who_normalized |>
#   detect(.method = c("tsoutlier", "isotree", "zscore"),
#          .args = list(isotree = c(.threshold=0.7)),
#          .additional_cols = TRUE) -> who_anomalies

who_anomalies <- who_anomalies[
  order(-who_anomalies$outlier_indicator_total,
        -who_anomalies$absZscore_zscore), ]

summary(who_anomalies)
table(who_anomalies$outlier_indicator_total)     # All methods
table(who_anomalies$outlier_indicator_tsoutlier) # tsoutlier
table(who_anomalies$outlier_indicator_zscore)    # zscore
table(who_anomalies$outlier_indicator_isotree)   # isotree
# table(who_anomalies$)                            # capa
# table(who_anomalies$)                            # outliertree

# Save the results to CSV
# fn <- paste0("who_data_anomalies", "_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
# sel_vars <- c("location", "code", "indicator", "year", "value", "Zscore",
#               "Imputed", "outlier_indicator_total")
# who_anomalies_selected <- who_anomalies[, sel_vars]
# write.csv(who_anomalies_selected, file = fn, row.names = FALSE)

# Plot a sample of anomalies as an example
for(i in 1:20) {
  if(who_anomalies$Imputed[i] == FALSE) {
    iname <- who_anomalies$indicator_name[i]
    if (is.null(iname)) iname <- "Time"
    print(plot(who_anomalies,
               country = who_anomalies$code[i],
               indicator = who_anomalies$indicator[i],
               .total_threshold = 2, x.lab = iname, y.lab = "Value"))
  }
}

# Plot one selected anomaly
plot(who_anomalies,
     country = "IRN", indicator = "gdp",
     .total_threshold = 2, x.lab = "Time", y.lab = "Value")

