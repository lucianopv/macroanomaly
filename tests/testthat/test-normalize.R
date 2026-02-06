test_that("Test the arguments of normalize using WDI", {
  ## Loading data WDI
  wdi_data <- wdi_download(.path = tempdir())

  wdi_data |>
    pivot(
      ids = c("Year", "Country.Name", "Country.Code"),
      how = "longer",
      names = list("Indicator.Code", "Indicator.Value")
    ) -> wdi_data_long

  wdi_data_long |>
    fsubset(Indicator.Code %in% c("GC.DOD.TOTL.GD.ZS", "NE.CON.TOTL.ZS") & Year > 2005) -> wdi_data_long_subset

  # Test lack of arguments:
  ## .value_col
  expect_error(
      wdi_data_long_subset |>
        normalize(.country_col = c("Country.Code", "Country.Name"),
                  .time_col = "Year",
                  .indicator_col = "Indicator.Code"
      ),
      regexp = 'argument ".value_col" is missing, with no default'
    )

  ## .country_col
  expect_error(
    wdi_data_long_subset |>
      normalize(.value_col = "Indicator.Value",
                .time_col = "Year",
                .indicator_col = "Indicator.Code"
      ),
    regexp = 'argument ".country_col" is missing, with no default'
  )

  ## .time_col
  expect_error(
    wdi_data_long_subset |>
      normalize(.value_col = "Indicator.Value",
                .country_col = c("Country.Code", "Country.Name"),
                .indicator_col = "Indicator.Code"
      ),
    regexp = 'argument ".time_col" is missing, with no default'
  )

  ## .indicator_col
  expect_error(
    wdi_data_long_subset |>
      normalize(.value_col = "Indicator.Value",
                .country_col = c("Country.Code", "Country.Name"),
                .time_col = "Year"
      ),
    regexp = 'argument ".indicator_col" is missing, with no default'
  )
})

test_that("Problems with data in normalize (WDI)", {
  wdi_data <- wdi_download(.path = tempdir())

  wdi_data |>
    pivot(
      ids = c("Year", "Country.Name", "Country.Code"),
      how = "longer",
      names = list("Indicator.Code", "Indicator.Value")
    ) -> wdi_data_long

  wdi_data_long |>
    fsubset(Indicator.Code %in% c("GC.DOD.TOTL.GD.ZS", "NE.CON.TOTL.ZS") & Year > 2005) -> wdi_data_long_subset

  expect_error(
    wdi_data_long_subset |>
      normalize(.value_col = "Indicator.Value",
                .country_col = c("Country.Code", "Country.Name"),
                .time_col = "Year",
                .indicator_col = "Indicator.Code",
                .detrend = TRUE,
                .impute = TRUE
      ),
    regexp = "The following countries do not have enough data"
  )

  expect_error(
    wdi_data_long_subset |>
      normalize(.value_col = "Indicator.Value",
                .country_col = c("Country.Code", "Country.Name"),
                .time_col = "Year",
                .indicator_col = "Indicator.Code",
                .detrend = TRUE,
                .impute = FALSE
      ),
    regexp = "The data contains missing values. Consider filling them before decomposition or using .impute = TRUE with prefered method"
  )

  expect_error(
    wdi_data_long_subset |>
      normalize(.value_col = "Indicator.Value",
                .country_col = c("Country.Code", "Country.Name"),
                .time_col = "Year",
                .method = "na_nonsense",
                .indicator_col = "Indicator.Code",
                .detrend = TRUE,
                .impute = TRUE
      ),
    regexp = "Invalid method. Choose"
  )

  expect_error(
    wdi_data_long_subset |>
      normalize(.value_col = "Indicator.Value",
                .country_col = c("Country.Code", "Country.Name"),
                .time_col = "Year",
                .indicator_col = "Indicator.Code",
                .detrend = TRUE,
                .impute = TRUE,
                .frequency = "weekly"
      ),
    regexp = "Invalid frequency. Choose"
  )

  expect_warning(
    wdi_data_long_subset |>
      fsubset(!Country.Code %in% c("ABW", "AFE", "AFE", "AFG", "AFW", "AFW", "AGO", "AND", "AND",
                                  "ARB", "ARB", "ARE", "ARG", "ASM", "ASM", "ATG", "ATG", "AUT",
                                  "BDI", "BEL", "BEN", "BGD", "BGR", "BMU", "BOL", "BRB", "BRN",
                                  "CAF", "CEB", "CEB", "CHI", "CHI", "CHL", "CHN", "CIV", "CMR",
                                  "COD", "COM", "CPV", "CRI", "CSS", "CSS", "CUB", "CUW", "CYM",
                                  "CYM", "CYP", "CZE", "DEU", "DJI", "DMA", "DNK", "DOM", "DZA",
                                  "EAP", "EAP", "EAR", "EAR", "EAS", "EAS", "ECA", "ECS", "ECS",
                                  "ECU", "EGY", "EMU", "EMU", "ERI", "EST", "EUU", "EUU", "FCS",
                                  "FCS", "FIN", "FJI", "FRA", "FRO", "FSM", "GAB", "GHA", "GIB",
                                  "GIB", "GIN", "GMB", "GNB", "GNQ", "GRC", "GRD", "GRD", "GRL",
                                  "GUM", "GUM", "GUY", "GUY", "HIC", "HKG", "HND", "HPC", "HPC",
                                  "HRV", "HTI", "IBD", "IBD", "IBT", "IBT", "IDA", "IDA", "IDB",
                                  "IDB", "IDX", "IDX", "IMN", "IMN", "INX", "INX", "IRL", "IRN",
                                  "ISR", "ITA", "JAM", "JPN", "KEN", "KHM", "KIR", "KNA", "KWT",
                                  "LAC", "LAO", "LBN", "LBR", "LBY", "LCA", "LCN", "LDC", "LDC",
                                  "LIC", "LIC", "LIE", "LIE", "LMC", "LMC", "LMY", "LMY", "LTE",
                                  "LTE", "LTU", "LUX", "LVA", "MAC", "MAF", "MAF", "MCO", "MCO",
                                  "MDG", "MEA", "MEA", "MIC", "MIC", "MKD", "MLI", "MLT", "MMR",
                                  "MMR", "MNA", "MNA", "MNE", "MNP", "MNP", "MRT", "MWI", "NAC",
                                  "NAM", "NCL", "NER", "NGA", "NGA", "NIC", "NLD", "NOR", "NRU",
                                  "NRU", "OED", "OMN", "OSS", "OSS", "PAK", "PAN", "PNG", "POL",
                                  "PRE", "PRE", "PRI", "PRK", "PRK", "PRT", "PRY", "PSE", "PSS",
                                  "PSS", "PST", "PYF", "QAT", "ROU", "RWA", "SAS", "SAU", "SDN",
                                  "SEN", "SLE", "SOM", "SRB", "SSA", "SSA", "SSD", "SSF", "SSF",
                                  "SST", "SST", "STP", "STP", "SUR", "SVK", "SVN", "SWE", "SWZ",
                                  "SXM", "SXM", "SYC", "SYR", "TCA", "TCA", "TCD", "TEA", "TEA",
                                  "TEC", "TGO", "TJK", "TKM", "TLA", "TLS", "TMN", "TMN", "TSA",
                                  "TSS", "TSS", "TTO", "TUV", "TUV", "TZA", "UMC", "UMC", "UZB",
                                  "VCT", "VEN", "VGB", "VGB", "VIR", "VNM", "WLD", "WLD", "WSM",
                                  "XKX", "YEM", "ZWE")) |>
      normalize(.value_col = c("Indicator.Value", "Indicator.Code"),
                .country_col = c("Country.Code", "Country.Name"),
                .time_col = "Year",
                .indicator_col = "Indicator.Code",
                .detrend = TRUE,
                .impute = TRUE,
                .frequency = "yearly",
                .keep_decomp = TRUE
      ),
    regexp = "Multiple columns specified to normalize"
  )

})

test_that("Class of nomalize object is maly_norm", {
  wdi_data <- wdi_download(.path = tempdir())

  wdi_data |>
    pivot(
      ids = c("Year", "Country.Name", "Country.Code"),
      how = "longer",
      names = list("Indicator.Code", "Indicator.Value")
    ) -> wdi_data_long

  wdi_data_long |>
    fsubset(Indicator.Code %in% c("GC.DOD.TOTL.GD.ZS", "NE.CON.TOTL.ZS") & Year > 2005) |>
    fsubset(!Country.Code %in% c("ABW", "AFE", "AFE", "AFG", "AFW", "AFW", "AGO", "AND", "AND",
                                 "ARB", "ARB", "ARE", "ARG", "ASM", "ASM", "ATG", "ATG", "AUT",
                                 "BDI", "BEL", "BEN", "BGD", "BGR", "BMU", "BOL", "BRB", "BRN",
                                 "CAF", "CEB", "CEB", "CHI", "CHI", "CHL", "CHN", "CIV", "CMR",
                                 "COD", "COM", "CPV", "CRI", "CSS", "CSS", "CUB", "CUW", "CYM",
                                 "CYM", "CYP", "CZE", "DEU", "DJI", "DMA", "DNK", "DOM", "DZA",
                                 "EAP", "EAP", "EAR", "EAR", "EAS", "EAS", "ECA", "ECS", "ECS",
                                 "ECU", "EGY", "EMU", "EMU", "ERI", "EST", "EUU", "EUU", "FCS",
                                 "FCS", "FIN", "FJI", "FRA", "FRO", "FSM", "GAB", "GHA", "GIB",
                                 "GIB", "GIN", "GMB", "GNB", "GNQ", "GRC", "GRD", "GRD", "GRL",
                                 "GUM", "GUM", "GUY", "GUY", "HIC", "HKG", "HND", "HPC", "HPC",
                                 "HRV", "HTI", "IBD", "IBD", "IBT", "IBT", "IDA", "IDA", "IDB",
                                 "IDB", "IDX", "IDX", "IMN", "IMN", "INX", "INX", "IRL", "IRN",
                                 "ISR", "ITA", "JAM", "JPN", "KEN", "KHM", "KIR", "KNA", "KWT",
                                 "LAC", "LAO", "LBN", "LBR", "LBY", "LCA", "LCN", "LDC", "LDC",
                                 "LIC", "LIC", "LIE", "LIE", "LMC", "LMC", "LMY", "LMY", "LTE",
                                 "LTE", "LTU", "LUX", "LVA", "MAC", "MAF", "MAF", "MCO", "MCO",
                                 "MDG", "MEA", "MEA", "MIC", "MIC", "MKD", "MLI", "MLT", "MMR",
                                 "MMR", "MNA", "MNA", "MNE", "MNP", "MNP", "MRT", "MWI", "NAC",
                                 "NAM", "NCL", "NER", "NGA", "NGA", "NIC", "NLD", "NOR", "NRU",
                                 "NRU", "OED", "OMN", "OSS", "OSS", "PAK", "PAN", "PNG", "POL",
                                 "PRE", "PRE", "PRI", "PRK", "PRK", "PRT", "PRY", "PSE", "PSS",
                                 "PSS", "PST", "PYF", "QAT", "ROU", "RWA", "SAS", "SAU", "SDN",
                                 "SEN", "SLE", "SOM", "SRB", "SSA", "SSA", "SSD", "SSF", "SSF",
                                 "SST", "SST", "STP", "STP", "SUR", "SVK", "SVN", "SWE", "SWZ",
                                 "SXM", "SXM", "SYC", "SYR", "TCA", "TCA", "TCD", "TEA", "TEA",
                                 "TEC", "TGO", "TJK", "TKM", "TLA", "TLS", "TMN", "TMN", "TSA",
                                 "TSS", "TSS", "TTO", "TUV", "TUV", "TZA", "UMC", "UMC", "UZB",
                                 "VCT", "VEN", "VGB", "VGB", "VIR", "VNM", "WLD", "WLD", "WSM",
                                 "XKX", "YEM", "ZWE")) -> wdi_data_long_subset

  expect_s3_class(
    wdi_data_long_subset |>
      normalize(.value_col = "Indicator.Value",
                .country_col = c("Country.Code", "Country.Name"),
                .time_col = "Year",
                .indicator_col = "Indicator.Code",
                .detrend = TRUE,
                .impute = TRUE
      ),
    "maly_norm"
  )
})

test_that("Result has main columns in it", {
  wdi_data <- wdi_download(.path = tempdir())

  wdi_data |>
    pivot(
      ids = c("Year", "Country.Name", "Country.Code"),
      how = "longer",
      names = list("Indicator.Code", "Indicator.Value")
    ) -> wdi_data_long

  wdi_data_long |>
    fsubset(Indicator.Code %in% c("GC.DOD.TOTL.GD.ZS", "NE.CON.TOTL.ZS") & Year > 2005) |>
    fsubset(!Country.Code %in% c("ABW", "AFE", "AFE", "AFG", "AFW", "AFW", "AGO", "AND", "AND",
                                 "ARB", "ARB", "ARE", "ARG", "ASM", "ASM", "ATG", "ATG", "AUT",
                                 "BDI", "BEL", "BEN", "BGD", "BGR", "BMU", "BOL", "BRB", "BRN",
                                 "CAF", "CEB", "CEB", "CHI", "CHI", "CHL", "CHN", "CIV", "CMR",
                                 "COD", "COM", "CPV", "CRI", "CSS", "CSS", "CUB", "CUW", "CYM",
                                 "CYM", "CYP", "CZE", "DEU", "DJI", "DMA", "DNK", "DOM", "DZA",
                                 "EAP", "EAP", "EAR", "EAR", "EAS", "EAS", "ECA", "ECS", "ECS",
                                 "ECU", "EGY", "EMU", "EMU", "ERI", "EST", "EUU", "EUU", "FCS",
                                 "FCS", "FIN", "FJI", "FRA", "FRO", "FSM", "GAB", "GHA", "GIB",
                                 "GIB", "GIN", "GMB", "GNB", "GNQ", "GRC", "GRD", "GRD", "GRL",
                                 "GUM", "GUM", "GUY", "GUY", "HIC", "HKG", "HND", "HPC", "HPC",
                                 "HRV", "HTI", "IBD", "IBD", "IBT", "IBT", "IDA", "IDA", "IDB",
                                 "IDB", "IDX", "IDX", "IMN", "IMN", "INX", "INX", "IRL", "IRN",
                                 "ISR", "ITA", "JAM", "JPN", "KEN", "KHM", "KIR", "KNA", "KWT",
                                 "LAC", "LAO", "LBN", "LBR", "LBY", "LCA", "LCN", "LDC", "LDC",
                                 "LIC", "LIC", "LIE", "LIE", "LMC", "LMC", "LMY", "LMY", "LTE",
                                 "LTE", "LTU", "LUX", "LVA", "MAC", "MAF", "MAF", "MCO", "MCO",
                                 "MDG", "MEA", "MEA", "MIC", "MIC", "MKD", "MLI", "MLT", "MMR",
                                 "MMR", "MNA", "MNA", "MNE", "MNP", "MNP", "MRT", "MWI", "NAC",
                                 "NAM", "NCL", "NER", "NGA", "NGA", "NIC", "NLD", "NOR", "NRU",
                                 "NRU", "OED", "OMN", "OSS", "OSS", "PAK", "PAN", "PNG", "POL",
                                 "PRE", "PRE", "PRI", "PRK", "PRK", "PRT", "PRY", "PSE", "PSS",
                                 "PSS", "PST", "PYF", "QAT", "ROU", "RWA", "SAS", "SAU", "SDN",
                                 "SEN", "SLE", "SOM", "SRB", "SSA", "SSA", "SSD", "SSF", "SSF",
                                 "SST", "SST", "STP", "STP", "SUR", "SVK", "SVN", "SWE", "SWZ",
                                 "SXM", "SXM", "SYC", "SYR", "TCA", "TCA", "TCD", "TEA", "TEA",
                                 "TEC", "TGO", "TJK", "TKM", "TLA", "TLS", "TMN", "TMN", "TSA",
                                 "TSS", "TSS", "TTO", "TUV", "TUV", "TZA", "UMC", "UMC", "UZB",
                                 "VCT", "VEN", "VGB", "VGB", "VIR", "VNM", "WLD", "WLD", "WSM",
                                 "XKX", "YEM", "ZWE")) -> wdi_data_long_subset

  wdi_data_long_subset |>
    normalize(.value_col = c("Indicator.Value"),
              .country_col = c("Country.Code"),
              .time_col = c("Year"),
              .indicator_col = c("Indicator.Code"),
              .detrend = TRUE,
              .impute = TRUE
    ) -> normalized_data

  wdi_data_long_subset |>
    normalize(.value_col = c("Indicator.Value"),
              .country_col = c("Country.Code"),
              .time_col = c("Year"),
              .indicator_col = c("Indicator.Code"),
              .detrend = TRUE,
              .impute = TRUE,
              .keep_decomp = TRUE
    ) -> normalized_data_decomp

  # Original columns
  expect_true(
    all(c("Country.Name",
          "Country.Code",
          "Indicator.Code",
          "Year",
          "Indicator.Value") %in% colnames(
      normalized_data
          ))
  )

  # Normalized columns
  expect_true(
    all(c("Zscore",
          "Imputed") %in% colnames(normalized_data)))

  # Original columns when .keep_decomp = TRUE
  expect_true(
    all(c("Country.Name",
          "Country.Code",
          "Indicator.Code",
          "Year",
          "Indicator.Value") %in% colnames(
      normalized_data_decomp
    ))
  )

  # Normalized columns when .keep_decomp = TRUE
  expect_true(
    all(c("Zscore",
          "Imputed",
          "trend",
          "season_adjust",
          ".model") %in% colnames(normalized_data_decomp))
  )

  expect_s3_class(normalized_data, "maly_norm")

  expect_s3_class(normalized_data_decomp, "maly_norm")

  expect_type(normalized_data$Imputed, "logical")

  expect_type(normalized_data_decomp$Imputed, "logical")

})
