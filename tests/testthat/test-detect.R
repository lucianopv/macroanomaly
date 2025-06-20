test_that("Check inputs and arguments", {
  IFS <- imf.data::load_datasets("IFS")
  imf_data <- IFS$get_series(freq = "Q",
                             indicator = "LUR_PT",
                             start_period = "2015-01-01",
                             end_period = "2023-12-31")


  imf_data |>
    fmutate(TIME_PERIOD = lubridate::yq(TIME_PERIOD)) |>
    pivot(
      ids = "TIME_PERIOD",
      how = "longer") |>
    fmutate(
      value = as.numeric(value),
      Country = stringr::str_split_i(variable, "\\.", 2),
      Frequency = stringr::str_split_i(variable, "\\.", 1),
      variable = stringr::str_split_i(variable, "\\.", 3)) -> imf_data_long

  # Should cause error due to missing data points in some countries
  expect_error(
    imf_data_long |>
     normalize(.indicator_col = "variable",
                .frequency = "quarterly",
                .value_col = "value",
                .country_col = "Country",
                .time_col = "TIME_PERIOD"), "The following countries do not have")

  imf_data_long |>
    fsubset(!Country %in% c("AO", "SM")) |>
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

IFS <- imf.data::load_datasets("IFS")
imf_data <- IFS$get_series(freq = "Q",
                           indicator = "LUR_PT",
                           start_period = "2015-01-01",
                           end_period = "2023-12-31")


imf_data |>
  fmutate(TIME_PERIOD = lubridate::yq(TIME_PERIOD)) |>
  pivot(
    ids = "TIME_PERIOD",
    how = "longer") |>
  fmutate(
    value = as.numeric(value),
    Country = stringr::str_split_i(variable, "\\.", 2),
    Frequency = stringr::str_split_i(variable, "\\.", 1),
    variable = stringr::str_split_i(variable, "\\.", 3)) -> imf_data_long

imf_data_long |>
  fsubset(!Country %in% c("AO", "SM")) |>
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
  expect_true(all(c("Country", "TIME_PERIOD", "variable", "capa_strength") %in% colnames(imf_detect_capa)))
  expect_true(all((imf_detect_capa$capa_score > 0.5) == imf_detect_capa$outlier_indicator))

  imf_detect_capa_arg <- detect(imf_data_long_subset_norm, .method = "capa", .args = list(capa = c(.threshold = 0.7)), .additional_cols = TRUE)

  expect_true(all((imf_detect_capa_arg$capa_score > 0.7) == imf_detect_capa_arg$outlier_indicator))

  expect_true(all((imf_detect_capa_arg$type == "point") == (!is.na(imf_detect_capa_arg$strength))))

  expect_true(all((imf_detect_capa_arg$outlier_indicator == 1) == (imf_detect_capa_arg$type %in% c("collective", "point"))))

})

test_that("Check multiple methods detection", {

  imf_detect_all <- detect(imf_data_long_subset_norm, .method = c("zscore", "isotree", "capa"),
                           .additional_cols = TRUE,
                           .country_col = "Country", .time_col = "TIME_PERIOD", .indicator_col = "variable")

  expect_s3_class(imf_detect_all, "maly_detect")
  expect_true(all(c("Country", "TIME_PERIOD", "variable", "rankZscore_zscore", "outlier_score_isotree", "capa_strength_capa") %in% colnames(imf_detect_all)))

  expect_true(all((abs(imf_detect_all$Zscore) > 3) == imf_detect_all$outlier_indicator_zscore))
  expect_true(all((imf_detect_all$outlier_score_isotree > 0.5) == imf_detect_all$outlier_indicator_isotree))
  expect_true(all(!is.na(imf_detect_all$type_capa) == imf_detect_all$capa_outlier_indicator))

})
