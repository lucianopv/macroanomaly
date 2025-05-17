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
  fsubset(Indicator.Code == "EG.CFT.ACCS.ZS" & Year > 2005) -> wdi_data_long_subset

wdi_data_tsibble |>
  impute_missing(.value_col = "Indicator.Value",
                 .country_col = c("Country.Code", "Country.Name"),
                 .year_col = "Year",
                 .indicator_col = "Indicator.Code",
                 .method = "na_mean") -> wdi_data_long_subset_imputed

wdi_data_tsibble |>
  dplyr::filter(!Country.Code %in% c("ABW", "ASM", "BGR", "BMU", "CHI", "CUW", "CYM",
                                     "FRO", "GIB", "GRL", "GUM", "HKG", "IMN", "INX",
                                     "LBN", "LBY", "LIE", "MAC", "MAF", "MNP", "NCL",
                                     "PRI", "PSE", "PYF", "SXM", "TCA", "VGB", "VIR", "XKX")) |>
  impute_missing(.value_col = "Indicator.Value",
                 .country_col = c("Country.Code", "Country.Name"),
                 .year_col = "Year",
                 .indicator_col = "Indicator.Code",
                 .method = "na_mean") -> wdi_data_long_subset_imputed

wdi_data_long_subset_imputed |>
  decompose_tsibble(
    .value_col = "Indicator.Value",
    .frequency = "yearly") -> wdi_data_long_subset_imputed_decomposed

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

normalize(imf_data_long,
          .cols = "value",
          .country_col = "Country",
          .year_col = "TIME_PERIOD") -> imf_data_long_norm
