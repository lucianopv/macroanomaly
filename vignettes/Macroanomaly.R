## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(macroanomaly)
library(imf.data)
library(collapse)

## ----import-data--------------------------------------------------------------
imf_data_long <- read.csv(system.file("extdata", "imf_data.csv", package = "macroanomaly"),header =TRUE)
wdi_data_long_subset <- read.csv(system.file("extdata", "wdi_data_long_subset.csv", package = "macroanomaly"),header =TRUE)

## ----example_data-------------------------------------------------------------
#  # Load IMF data
#  imf.data::load_datasets("CPI")$get_series(freq = "M",
#                                            indicator = "PCPI_IX",
#                                            start_period = "2015-01-01",
#                                            end_period = "2023-12-31") |>
#    fmutate(TIME_PERIOD = lubridate::ym(TIME_PERIOD)) |>
#    pivot(
#      ids = "TIME_PERIOD",
#      how = "longer") |>
#    fmutate(
#      value = as.numeric(value),
#      Country = stringr::str_split_i(variable, "\\.", 2),
#      Frequency = stringr::str_split_i(variable, "\\.", 1),
#      variable = stringr::str_split_i(variable, "\\.", 3)) -> imf_data_long
#  
#  
#  # Load World Bank data
#  wdi_download(.path = tempdir()) |>
#    pivot(
#      ids = c("Year", "Country.Name", "Country.Code"),
#      how = "longer",
#      names = list("Indicator.Code", "Indicator.Value")
#      ) |>
#    fsubset(Indicator.Code %in% c("EG.CFT.ACCS.ZS",
#                                  "NY.GDP.MKTP.CN.AD") &
#              Year > 2005) -> wdi_data_long_subset

## ----filtering-data-----------------------------------------------------------
# Filter the data to remove countries with no data points
wdi_data_long_subset |>
  fsubset(!Country.Code %in% c("ABW", "ASM", "BGR", "BMU", "CHI", "CUW", "CYM",
                               "FRO", "GIB", "GRL", "GUM", "HKG", "IMN", "INX",
                               "LBN", "LBY", "LIE", "MAC", "MAF", "MNP", "NCL",
                               "PRI", "PSE", "PYF", "SXM", "TCA", "VGB", "VIR", 
                               "XKX", 
                               # No data in these countries or aggregates 
                               "AFE", "AFW", "ARB", "CEB", "CSS", "EAP", "EAR", 
                               "EAS", "ECA", "ECS", "EMU", "EUU", "FCS", "HIC", 
                               "HPC", "IBD", "IBT", "IDA", "IDB", "IDX", "LAC", 
                               "LCN", "LDC", "LIC", "LMC", "LMY", "LTE", "MEA", 
                               "MIC", "MNA", "NAC", "OED", "OSS", "PRE", "PRK", 
                               "PSS", "PST", "SAS", "SSA", "SSF", "SST", "TEA", 
                               "TEC", "TLA", "TMN", "TSA", "TSS", "UMC", "WLD" 
                               
                               )) -> wdi_data_long_subset_filtered

imf_data_long |>
  fsubset(!Country %in% c("YE")) -> imf_data_long_filtered


## ----normalization------------------------------------------------------------
# Normalize the IMF data
imf_data_long_filtered |>
  normalize(.indicator_col = "variable",
            .frequency = "monthly",
            .value_col = "value",
            .country_col = "Country",
            .detrend = TRUE,
            .impute = TRUE,
            .time_col = "TIME_PERIOD")  -> imf_data_long_normalized

# Normalize the World Bank data
wdi_data_long_subset_filtered |>
  normalize(.value_col= "Indicator.Value",
            .country_col = c("Country.Code", "Country.Name"),
            .indicator_col = "Indicator.Code",
            .time_col = "Year",
            .detrend = TRUE,
            .impute = TRUE
            ) -> wdi_data_long_subset_normalized

## ----summary-normalized-data--------------------------------------------------
# Summary of the normalized IMF data
summary(imf_data_long_normalized)

## ----plot-normalized-data-----------------------------------------------------
# Plot the normalized IMF data
plot(imf_data_long_normalized)

## ----plot-normalized-data-sudan-----------------------------------------------
# Plot the normalized IMF data for Sudan
plot(imf_data_long_normalized, country = c("SD", "CO"),
     x.lab = "Time", y.lab = "CPI")

## ----detect-imf---------------------------------------------------------------
# Detect anomalies in the IMF data using tsoutlier
imf_data_long_normalized |>
  detect(.method = "tsoutlier") -> imf_data_long_tsoutlier

# Detect anomalies in the IMF data using multiple methods
imf_data_long_normalized |>
  detect(.method = c("zscore", "isotree", "capa"), 
         .args = list(zscore = c(.min_seg_len = 3))
         ) -> imf_data_long_multiple_methods

## ----summary-detected-anomalies-imf-------------------------------------------
# Summary of the detected anomalies in the IMF data
summary(imf_data_long_multiple_methods)

## -----------------------------------------------------------------------------
# Detect anomalies in the IMF data using outliertree
imf_data_long |> 
  collapse::fsubset(Country %in% c("AR", "ZW")) |> 
  normalize(.indicator_col = "variable",
            .frequency = "monthly",
            .value_col = "value",
            .country_col = "Country",
            .detrend = TRUE,
            .impute = TRUE,
            .time_col = "TIME_PERIOD") |>
    detect(.method = "outliertree", 
           .args = list(outliertree = list(.cols = c("Zscore", "Country", "TIME_PERIOD")))
           ) -> imf_data_long_outliertree

## ----plot-detected-anomalies-imf----------------------------------------------
# Plot the detected anomalies in the IMF data
plot(imf_data_long_multiple_methods, country = "IT",
     x.lab = "Time", y.lab = "CPI")

## ----detect-wdi---------------------------------------------------------------
# Detect anomalies in the World Bank data using zscore
wdi_data_long_subset_normalized |>
  detect(.method = "zscore") -> wdi_data_long_zscore

# Detect anomalies in the World Bank data using multiple methods
wdi_data_long_subset_normalized |>
  detect(.method = c("tsoutlier", "isotree", "capa"), 
         .args = list(capa = c(.min_seg_len = 3), 
                      isotree = c(.threshold = 0.7))
         ) -> wdi_data_long_multiple_methods

## ----summary-detected-anomalies-wdi-------------------------------------------
# Summary of the detected anomalies in the World Bank data
summary(wdi_data_long_multiple_methods)

## ----plot-detected-anomalies-wdi----------------------------------------------
# Plot the detected anomalies in the World Bank data
plot(wdi_data_long_multiple_methods, 
     country = "BGD", 
     indicator = "EG.CFT.ACCS.ZS",
     x.lab = "Time", 
     y.lab = "Access to clean fuels for cooking (% of pop.)")

## ----save-results-------------------------------------------------------------
#  # Save the results to a CSV file only the first 30 outliers
#  write_to_csv(imf_data_long_multiple_methods,
#               file = "imf_data_anomalies.csv",
#               top_outliers = 30,
#               additional_cols = FALSE)
#  
#  # Save the results to a CSV file with all outliers
#  write_to_csv(wdi_data_long_multiple_methods, file = "wdi_data_anomalies.csv")

## ----session-info-------------------------------------------------------------
sessionInfo()

