# Setup: Load IMF data and run through normalize -> detect pipeline
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
            .time_col = "TIME_PERIOD") -> imf_normalized

imf_detected <- detect(imf_normalized, .method = "zscore")
imf_detected_multi <- detect(imf_normalized, .method = c("zscore", "isotree"))


test_that("Check inputs and arguments for validate", {

  # Error: input must be maly_detect class
  expect_error(
    validate(imf_normalized),
    regexp = "Input must be a data frame of class 'maly_detect'"
  )

  # Error: input must be data frame
  expect_error(
    validate(list(a = 1, b = 2)),
    regexp = "Input must be a data frame of class 'maly_detect'"
  )

  # Error: invalid method
  expect_error(
    validate(imf_detected, .method = "invalid_method"),
    regexp = "Invalid method"
  )
})


test_that("Check validate returns maly_validate class", {

  imf_validated <- validate(imf_detected, .method = "hampel")

  expect_s3_class(imf_validated, "maly_validate")
  expect_s3_class(imf_validated, "maly_detect")
  expect_s3_class(imf_validated, "maly_norm")
})


test_that("Check validate output has required columns", {

  imf_validated <- validate(imf_detected, .method = "hampel")

  # Check new columns exist
  expect_true(all(c("hampel_outlier", "validated_outlier") %in% colnames(imf_validated)))

  # Check original columns preserved
  expect_true(all(c("Country", "TIME_PERIOD", "variable", "Zscore", "outlier_indicator") %in% colnames(imf_validated)))

  # Check column types
  expect_type(imf_validated$hampel_outlier, "logical")
  expect_type(imf_validated$validated_outlier, "logical")
})


test_that("Check validated_outlier logic is correct", {

  imf_validated <- validate(imf_detected, .method = "hampel")

  # validated_outlier should only be TRUE when BOTH hampel and detect flag it
  expect_true(all(
    imf_validated$validated_outlier == (imf_validated$hampel_outlier & imf_validated$outlier_indicator == 1)
  ))

  # validated_outlier count should be <= detected outliers
  expect_lte(
    sum(imf_validated$validated_outlier),
    sum(imf_validated$outlier_indicator == 1)
  )

  # validated_outlier count should be <= hampel outliers
  expect_lte(
    sum(imf_validated$validated_outlier),
    sum(imf_validated$hampel_outlier)
  )
})


test_that("Check validate works with multiple detection methods", {

  imf_validated_multi <- validate(imf_detected_multi, .method = "hampel")

  expect_s3_class(imf_validated_multi, "maly_validate")
  expect_true("outlier_indicator_total" %in% colnames(imf_validated_multi))

  # validated_outlier should use outlier_indicator_total
  expect_true(all(
    imf_validated_multi$validated_outlier ==
      (imf_validated_multi$hampel_outlier & imf_validated_multi$outlier_indicator_total > 0)
  ))
})


test_that("Check validate preserves attributes", {

  imf_validated <- validate(imf_detected, .method = "hampel")

  # Check attributes from detect are preserved
  expect_equal(attr(imf_validated, "country_columns"), attr(imf_detected, "country_columns"))
  expect_equal(attr(imf_validated, "time_columns"), attr(imf_detected, "time_columns"))
  expect_equal(attr(imf_validated, "indicator_columns"), attr(imf_detected, "indicator_columns"))
  expect_equal(attr(imf_validated, "value_column"), attr(imf_detected, "value_column"))
  expect_equal(attr(imf_validated, "maly_detect_attr"), attr(imf_detected, "maly_detect_attr"))
  expect_equal(attr(imf_validated, "maly_norm_attrs"), attr(imf_detected, "maly_norm_attrs"))

  # Check new validate attribute
  expect_equal(attr(imf_validated, "maly_validate_attr")$method, "hampel")
})


test_that("Check Hampel filter arguments work", {

  # With different k and t0 values
  imf_validated_strict <- validate(imf_detected, .method = "hampel",
                                    .args = list(hampel = c(.k = 3, .t0 = 3)))
  imf_validated_lenient <- validate(imf_detected, .method = "hampel",
                                     .args = list(hampel = c(.k = 7, .t0 = 7)))

  expect_s3_class(imf_validated_strict, "maly_validate")
  expect_s3_class(imf_validated_lenient, "maly_validate")

  # Stricter parameters should generally find more Hampel outliers
  # (smaller window, lower threshold)
  expect_gte(
    sum(imf_validated_strict$hampel_outlier),
    sum(imf_validated_lenient$hampel_outlier)
  )
})


test_that("Check summary.maly_validate works", {

  imf_validated <- validate(imf_detected, .method = "hampel")

  # Should not error
  expect_output(summary(imf_validated), "Summary of Macroanomaly validation")
  expect_output(summary(imf_validated), "Detection method")
  expect_output(summary(imf_validated), "Validation method")
  expect_output(summary(imf_validated), "Validated outliers")

  # Should return object invisibly
  expect_invisible(summary(imf_validated))
})


test_that("Check plot.maly_validate works", {

  imf_validated <- validate(imf_detected, .method = "hampel")

  # Should return ggplot object
  p <- plot(imf_validated)
  expect_s3_class(p, "ggplot")
  expect_s3_class(p, "patchwork")

  # Should work with specific country/indicator (USA and U for new imfapi data)
  p2 <- plot(imf_validated, country = "USA", indicator = "U")
  expect_s3_class(p2, "ggplot")

  # Should work with show_all_outliers = FALSE
  p3 <- plot(imf_validated, show_all_outliers = FALSE)
  expect_s3_class(p3, "ggplot")
})


test_that("Check summary.maly_validate errors with wrong class", {

  expect_error(
    summary.maly_validate(imf_detected),
    regexp = "object must be of class 'maly_validate'"
  )
})


test_that("Check plot.maly_validate errors with invalid country/indicator", {

  imf_validated <- validate(imf_detected, .method = "hampel")

  expect_error(
    plot(imf_validated, country = "INVALID", indicator = "LUR_PT"),
    regexp = "No data found"
  )
})


test_that("Check plot.maly_validate includes caption with outlier counts", {

  imf_validated <- validate(imf_detected, .method = "hampel")

  p <- plot(imf_validated)

  # Check that the plot has a caption annotation
 expect_true("caption" %in% names(p$patches$annotation))
  
  # Caption should contain information about outlier counts
  caption_text <- p$patches$annotation$caption
  expect_true(grepl("validated|detected|Hampel", caption_text, ignore.case = TRUE))
})
