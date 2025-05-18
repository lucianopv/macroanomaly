## File with examples
# Using the WDI data
wdi_data <- wdi_download(.path = "data-raw")

wdi_data |>
  pivot(
    ids = c("Year", "Country.Name", "Country.Code"),
    how = "longer",
    names = list("Indicator.Code", "Indicator.Value")
    ) -> wdi_data_long

wdi_data_long |>
  fsubset(Indicator.Code %in% c("EG.CFT.ACCS.ZS", "NY.GDP.MKTP.CN.AD") & Year > 2005) -> wdi_data_long_subset

wdi_data_long_subset |>
  convert_to_tsibble(.country_col = c("Country.Code", "Country.Name"),
                      .year_col = "Year",
                      .indicator_col = "Indicator.Code",
                      .frequency = "yearly") -> wdi_data_tsibble

# Will produce error due to lack of data points in some countries
wdi_data_tsibble |>
  impute_missing(.value_col = "Indicator.Value",
                 .country_col = c("Country.Code", "Country.Name"),
                 .year_col = "Year",
                 .indicator_col = "Indicator.Code",
                 .method = "na_kalman") -> wdi_data_long_subset_imputed

# Will not produce error
wdi_data_tsibble |>
  dplyr::filter(!Country.Code %in% c("ABW", "ASM", "BGR", "BMU", "CHI", "CUW", "CYM",
                                     "FRO", "GIB", "GRL", "GUM", "HKG", "IMN", "INX",
                                     "LBN", "LBY", "LIE", "MAC", "MAF", "MNP", "NCL",
                                     "PRI", "PSE", "PYF", "SXM", "TCA", "VGB", "VIR", "XKX")) |>
  impute_missing(.value_col = "Indicator.Value",
                 .country_col = c("Country.Code", "Country.Name"),
                 .year_col = "Year",
                 .indicator_col = "Indicator.Code",
                 .method = "na_kalman") -> wdi_data_long_subset_imputed

wdi_data_long_subset_imputed |>
  decompose_tsibble(
    .value_col = "Indicator.Value",
    .frequency = "yearly") -> wdi_data_long_subset_imputed_decomposed

wdi_data_long_subset |>
  fsubset(!Country.Code %in% c("ABW", "ASM", "BGR", "BMU", "CHI", "CUW", "CYM",
                               "FRO", "GIB", "GRL", "GUM", "HKG", "IMN", "INX",
                               "LBN", "LBY", "LIE", "MAC", "MAF", "MNP", "NCL",
                               "PRI", "PSE", "PYF", "SXM", "TCA", "VGB", "VIR", "XKX", # No data in these countries
                                 "AFE", "AFW", "ARB", "CEB", "CSS", "EAP", "EAR", "EAS", "ECA", "ECS", "EMU", "EUU",
                                 "FCS", "HIC", "HPC", "IBD", "IBT", "IDA", "IDB", "IDX", "LAC", "LCN", "LDC", "LIC",
                                 "LMC", "LMY", "LTE", "MEA", "MIC", "MNA", "NAC", "OED", "OSS", "PRE", "PRK", "PSS",
                                 "PST", "SAS", "SSA", "SSF", "SST", "TEA", "TEC", "TLA", "TMN", "TSA", "TSS", "UMC", "WLD" # Aggregates without data
                               )) |>
  normalize(.value_col= "Indicator.Value",
            .country_col = c("Country.Code", "Country.Name"),
            .year_col = "Year",
            ## .keep_decomp = TRUE,
            .detrend = TRUE,
            .impute = TRUE
            ) -> wdi_data_long_subset_norm
# Need to test all possibilities here

zscore_detection(wdi_data_long_subset_norm)

isotree_detection(wdi_data_long_subset_norm)

tsoutliers_detection(wdi_data_long_subset_norm) |> fungroup() |> fsubset(Country.Code == "ARM")

wdi_data_long_subset_norm |>
  tsoutliers_detection() |>
  fungroup() |>
  as.data.frame() |>
  fmutate(Year = as.Date(Year)) |>
  fsubset(Country.Code == "AFG") |>
  ggplot(aes(x = Year, y = Indicator.Value, color = outlier_indicator, group = Country.Code, shape = Imputed)) +
  geom_point() + geom_line() + facet_wrap(~Indicator.Code, scales = "free") -> tmp_value

wdi_data_long_subset_norm |>
  tsoutliers_detection() |>
  fungroup() |>
  as.data.frame() |>
  fmutate(Year = as.Date(Year)) |>
  fsubset(Country.Code == "AFG") |>
  ggplot(aes(x = Year, y = Zscore, color = outlier_indicator, group = Country.Code, shape = Imputed)) +
  geom_point() + facet_wrap(~Indicator.Code, scales = "free") -> tmp_zscore

tmp_value / tmp_zscore + plot_layout(guides = "collect")

# Using the IMF data
CPI <- imf.data::load_datasets("CPI")
imf_data <- CPI$get_series(freq = "M",
               indicator = "PCPI_IX",
               start_period = "2015-01-01",
               end_period = "2023-12-31")

imf_data |>
  fmutate(TIME_PERIOD = lubridate::ym(TIME_PERIOD)) |>
  pivot(
    ids = "TIME_PERIOD",
    how = "longer") |>
  fmutate(
    value = as.numeric(value),
    Country = stringr::str_split_i(variable, "\\.", 2),
    Frequency = stringr::str_split_i(variable, "\\.", 1),
    variable = stringr::str_split_i(variable, "\\.", 3)) -> imf_data_long

imf_data_long |>
  fsubset(Country %!=% "YE", TIME_PERIOD:Frequency) |>
  convert_to_tsibble(.country_col = "Country",
                     .year_col = "TIME_PERIOD",
                     .indicator_col = "variable",
                     .frequency = "monthly") |>
  impute_missing(.value_col = "value",
                 .country_col = "Country",
                 .year_col = "TIME_PERIOD",
                 .indicator_col = "variable",
                 .method = "na_kalman") -> imf_data_long_imputed

imf_data_long_imputed %>%
  fmutate(TIME_PERIOD = yearmonth(TIME_PERIOD)) %>%
  as_tsibble(key = Country, index = TIME_PERIOD, regular = TRUE) %>%
  model(STL(value ~ trend(), na.action = na.omit)) %>%
  components()

# Will cause error due to lack of data points in some countries
imf_data_long |>
  normalize(.indicator_col = "variable",
            .frequency = "monthly",
          .value_col = "value",
          .country_col = "Country",
          .year_col = "TIME_PERIOD") -> imf_data_long_subset_norm

# Will cause error due to lack of data points in some countries
imf_data_long |>
  fsubset(!Country %in% c("YE")) |>
  normalize(.indicator_col = "variable",
            .frequency = "monthly",
            .value_col = "value",
            .country_col = "Country",
            .year_col = "TIME_PERIOD") -> imf_data_long_subset_norm

zscore_detection(imf_data_long_subset_norm)

imf_data_long_subset_norm |>
  fselect(Country, TIME_PERIOD, value, Zscore, variable, Imputed) |>
  isotree_detection()

imf_data_long_subset_norm |>
  tsoutliers_detection()

imf_data_long_subset_norm |>
  tsoutliers_detection() |>
  fungroup() |>
  as.data.frame() |>
  fmutate(TIME_PERIOD = as.Date(TIME_PERIOD)) |>
  fsubset(Country == "SD") |>
  ggplot(aes(x = TIME_PERIOD, y = value, color = outlier_indicator, group = Country, shape = Imputed)) +
  geom_line() +
  geom_point() -> tmp_value
imf_data_long_subset_norm |>
  tsoutliers_detection() |>
  fungroup() |>
  as.data.frame() |>
  fmutate(TIME_PERIOD = as.Date(TIME_PERIOD)) |>
  fsubset(Country == "SD") |>
  ggplot(aes(x = TIME_PERIOD, y = Zscore, color = outlier_indicator, group = Country, shape = Imputed)) +
  geom_point() -> tmp_zscore
tmp_value / tmp_zscore + plot_layout(guides = "collect")


imf_data_long_subset_norm |>
  isotree_detection() |>
  fungroup() |>
  as.data.frame() |>
  fmutate(TIME_PERIOD = as.Date(TIME_PERIOD)) |>
  fsubset(Country == "SD") |>
  ggplot(aes(x = TIME_PERIOD, y = value, color = outlier_indicator, group = Country, shape = Imputed)) +
  geom_line() +
  geom_point() -> tmp_value
imf_data_long_subset_norm |>
  isotree_detection() |>
  fungroup() |>
  as.data.frame() |>
  fmutate(TIME_PERIOD = as.Date(TIME_PERIOD)) |>
  fsubset(Country == "SD") |>
  ggplot(aes(x = TIME_PERIOD, y = Zscore, color = outlier_indicator, group = Country, shape = Imputed)) +
  geom_point() -> tmp_zscore
tmp_value / tmp_zscore + plot_layout(guides = "collect")
