test_that("Check inputs and arguments", {
  # Load IMF unemployment data using imfapi
  imf_data <- imfapi::imf_get("LS",
                               dimensions = list(INDICATOR = c("U")),
                               start_period = "2015-Q1",
                               end_period = "2023-Q4")

  # Filter to quarterly frequency and PT transformation, rename columns
  imf_data |>
    fsubset(FREQUENCY == "Q" & TYPE_OF_TRANSFORMATION == "PT") |>
    fmutate(TIME_PERIOD = lubridate::yq(TIME_PERIOD)) |>
    frename(Country = COUNTRY, variable = INDICATOR, value = OBS_VALUE) -> imf_data_long

  # Should cause error due to missing data points in some countries
  expect_error(
    imf_data_long |>
     normalize(.indicator_col = "variable",
                .frequency = "quarterly",
                .value_col = "value",
                .country_col = "Country",
                .time_col = "TIME_PERIOD"), "The following countries do not have")

  # Exclude countries with insufficient data for STL decomposition
  imf_data_long |>
    fsubset(!Country %in% c("AGO", "BLR", "BRB", "BWA", "CHN", "DZA", "GHA", "SGP", "SMR")) |>
    normalize(.indicator_col = "variable",
              .frequency = "quarterly",
              .value_col = "value",
              .country_col = "Country",
              .time_col = "TIME_PERIOD") -> imf_data_long_subset_norm

  # Error with data type
  expect_error(detect(imf_data_long), regexp = "Input must be a data frame and")

  # Error with method
  expect_error(detect(imf_data_long_subset_norm, .method = "misterious_method"),
               regexp = "Invalid method")

  # Error with column arguments
  expect_error(detect(imf_data_long_subset_norm, .method = "zscore",
                      .country_col = "NOT_country_col"),
               regexp = "One or more specified columns do not exist in the data frame*")

})

# Load IMF unemployment data using imfapi
imf_data <- imfapi::imf_get("LS",
                             dimensions = list(INDICATOR = c("U")),
                             start_period = "2015-Q1",
                             end_period = "2023-Q4")

# Filter to quarterly frequency and PT transformation, rename columns
imf_data |>
  fsubset(FREQUENCY == "Q" & TYPE_OF_TRANSFORMATION == "PT") |>
  fmutate(TIME_PERIOD = lubridate::yq(TIME_PERIOD)) |>
  frename(Country = COUNTRY, variable = INDICATOR, value = OBS_VALUE) -> imf_data_long

# Exclude countries with insufficient data for STL decomposition
imf_data_long |>
  fsubset(!Country %in% c("AGO", "BLR", "BRB", "BWA", "CHN", "DZA", "GHA", "SGP", "SMR")) |>
  normalize(.indicator_col = "variable",
            .frequency = "quarterly",
            .value_col = "value",
            .country_col = "Country",
            .time_col = "TIME_PERIOD") -> imf_data_long_subset_norm

test_that("Check zscore detection", {


  imf_detect_zscore <- detect(imf_data_long_subset_norm, .method = "zscore")

  expect_s3_class(imf_detect_zscore, "maly_detect")
  expect_true(all(c("Country", "TIME_PERIOD", "variable", "Zscore") %in% colnames(imf_detect_zscore)))
  expect_true(all((abs(imf_detect_zscore$Zscore) > 3) == imf_detect_zscore$outlier_indicator))
})


test_that("Check isotree detection", {

  imf_detect_isotree <- detect(imf_data_long_subset_norm, .method = "isotree", .additional_cols = TRUE)

  expect_s3_class(imf_detect_isotree, "maly_detect")
  expect_true(all(c("Country", "TIME_PERIOD", "variable", "outlier_score") %in% colnames(imf_detect_isotree)))
  expect_true(all((imf_detect_isotree$outlier_score > 0.5) == imf_detect_isotree$outlier_indicator))

  imf_detect_isotree_arg <- detect(imf_data_long_subset_norm, .method = "isotree", .args = list(isotree = c(.threshold = 0.7)), .additional_cols = TRUE)

  expect_true(all((imf_detect_isotree_arg$outlier_score > 0.7) == imf_detect_isotree_arg$outlier_indicator))

})


test_that("Check capa detection", {

  imf_detect_capa <- detect(imf_data_long_subset_norm, .method = "capa", .additional_cols = TRUE)

  expect_s3_class(imf_detect_capa, "maly_detect")
  expect_true(all(c("Country", "TIME_PERIOD", "variable", "capa_strength", "type") %in% colnames(imf_detect_capa)))
  
  # Check that outlier_indicator == 1 when type is point or collective
  expect_true(all((imf_detect_capa$outlier_indicator == 1) == (imf_detect_capa$type %in% c("collective", "point")), na.rm = TRUE))
  
  # Check that point anomalies have capa_strength, collective anomalies don't
  point_rows <- imf_detect_capa$type == "point" & !is.na(imf_detect_capa$type)
  expect_true(all(!is.na(imf_detect_capa$capa_strength[point_rows])))

})

test_that("Check multiple methods detection", {

  imf_detect_all <- detect(imf_data_long_subset_norm, .method = c("zscore", "isotree", "capa"),
                           .additional_cols = TRUE,
                           .country_col = "Country", .time_col = "TIME_PERIOD", .indicator_col = "variable")

  expect_s3_class(imf_detect_all, "maly_detect")
  expect_true(all(c("Country", "TIME_PERIOD", "variable", "rankZscore_zscore", "outlier_score_isotree", "capa_strength_capa") %in% colnames(imf_detect_all)))

  expect_true(all((abs(imf_detect_all$Zscore) > 3) == imf_detect_all$outlier_indicator_zscore))
  expect_true(all((imf_detect_all$outlier_score_isotree > 0.5) == imf_detect_all$outlier_indicator_isotree))
  
  # Check CAPA outlier_indicator matches type being point or collective
  expect_true(all((imf_detect_all$outlier_indicator_capa == 1) == (imf_detect_all$type_capa %in% c("collective", "point")), na.rm = TRUE))

})
