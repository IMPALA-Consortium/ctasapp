test_that("prepare_measures returns expected structure", {
  m <- prepare_measures(sample_ctas_data, sample_ctas_results)

  expect_s3_class(m, "data.frame")
  expect_true(nrow(m) > 0)

  expected_cols <- c(
    "subject_id", "timepoint_rank", "timepoint_1_name", "result",
    "parameter_id", "site", "parameter_name", "max_score", "n_subjects"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(m), info = paste("missing column:", col))
  }

  expect_true(all(!is.na(m$max_score)))
  expect_true(all(m$max_score >= 0))
  expect_true(all(m$n_subjects > 0))
})

test_that("prepare_measures covers all parameters", {
  m <- prepare_measures(sample_ctas_data, sample_ctas_results)
  input_params <- unique(sample_ctas_data$data$parameter_id)
  output_params <- unique(m$parameter_id)
  expect_equal(sort(output_params), sort(input_params))
})

test_that("prepare_measures works with SDTM sample data", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)

  expect_s3_class(m, "data.frame")
  expect_true(nrow(m) > 0)
  expect_true("parameter_category_3" %in% names(m))
  expect_true("parameter_category_2" %in% names(m))
})

test_that("prepare_score_table returns wide format with max_score", {
  st <- prepare_score_table(sample_ctas_results, "param1")

  expect_s3_class(st, "data.frame")
  expect_true(nrow(st) > 0)
  expect_true("site" %in% names(st))
  expect_true("max_score" %in% names(st))

  feature_cols <- setdiff(names(st), c("site", "max_score"))
  expect_true(length(feature_cols) > 0)

  expect_true(all(!is.na(st$max_score)))
  expect_equal(st$max_score, sort(st$max_score, decreasing = TRUE))
})

test_that("prepare_score_table values are rounded to 2 decimals", {
  st <- prepare_score_table(sample_ctas_results, "param1")
  feature_cols <- setdiff(names(st), c("site", "max_score"))

  for (col in feature_cols) {
    vals <- st[[col]]
    expect_equal(vals, round(vals, 2), info = paste("column not rounded:", col))
  }
  expect_equal(st$max_score, round(st$max_score, 2))
})

test_that("prepare_score_table_multi returns same as prepare_score_table for single param", {
  st_single <- prepare_score_table(sample_ctas_results, "param1")
  st_multi <- prepare_score_table_multi(sample_ctas_results, "param1")
  expect_equal(st_single, st_multi)
})

test_that("prepare_score_table_multi handles multiple parameter_ids", {
  params <- unique(sample_sdtm_results$timeseries$parameter_id)
  cat_params <- params[grepl("^RS_OVRLRESP=", params)]
  if (length(cat_params) > 0) {
    st <- prepare_score_table_multi(sample_sdtm_results, cat_params)
    expect_s3_class(st, "data.frame")
    expect_true("site" %in% names(st))
    expect_true("max_score" %in% names(st))
  }
})

test_that("prepare_score_table_multi returns empty for non-matching params", {
  st <- prepare_score_table_multi(sample_ctas_results, "nonexistent_param")
  expect_equal(nrow(st), 0)
})

test_that("prepare_ts_data filters by parameter and threshold", {
  m <- prepare_measures(sample_ctas_data, sample_ctas_results)

  td <- prepare_ts_data(m, "param1", thresh = 1.3)

  expect_s3_class(td, "data.frame")
  expect_true(all(td$max_score > 1.3))

  expected_cols <- c(
    "site", "subject_id", "timepoint_rank", "timepoint_1_name",
    "result", "parameter_name", "max_score"
  )
  expect_equal(names(td), expected_cols)
})

test_that("prepare_ts_data returns empty data frame with high threshold", {
  m <- prepare_measures(sample_ctas_data, sample_ctas_results)
  td <- prepare_ts_data(m, "param1", thresh = 99999)

  expect_s3_class(td, "data.frame")
  expect_equal(nrow(td), 0)
})

test_that("prepare_ts_data is sorted by site, subject, timepoint", {
  m <- prepare_measures(sample_ctas_data, sample_ctas_results)
  td <- prepare_ts_data(m, "param1", thresh = 0)

  if (nrow(td) > 1) {
    sorted <- td[order(td$site, td$subject_id, td$timepoint_rank), ]
    expect_equal(td, sorted)
  }
})

test_that("prepare_ts_data_multi handles multiple params", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  cat_params <- unique(m$parameter_id[grepl("^RS_OVRLRESP=", m$parameter_id)])

  if (length(cat_params) > 0) {
    td <- prepare_ts_data_multi(m, cat_params, thresh = 0)
    expect_s3_class(td, "data.frame")
    expect_true("parameter_id" %in% names(td))
  }
})

test_that("prepare_ts_data_multi returns empty with high threshold", {
  m <- prepare_measures(sample_ctas_data, sample_ctas_results)
  td <- prepare_ts_data_multi(m, "param1", thresh = 99999)
  expect_equal(nrow(td), 0)
})

test_that("VS_HEIGHT has scores in SDTM sample results", {
  ts <- sample_sdtm_results$timeseries
  expect_true("VS_HEIGHT" %in% ts$parameter_id)

  st <- prepare_score_table_multi(sample_sdtm_results, "VS_HEIGHT")
  expect_true(nrow(st) > 0)
  expect_true(any(st$max_score > 0))
})

test_that("VS_WEIGHT_CAT has scores in SDTM sample results", {
  ts <- sample_sdtm_results$timeseries
  wt_ids <- ts$parameter_id[grepl("^VS_WEIGHT_CAT=", ts$parameter_id)]
  expect_true(length(wt_ids) > 0)

  st <- prepare_score_table_multi(sample_sdtm_results, unique(wt_ids))
  expect_true(nrow(st) > 0)
  expect_true(any(st$max_score > 0))
})
