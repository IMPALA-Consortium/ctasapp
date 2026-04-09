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

test_that("prepare_ts_data_multi joins untransformed for numeric labs", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  ut <- sample_sdtm_data$untransformed

  lab_params <- unique(m$parameter_id[grepl("^LB_NORM_ALT|^LB_MISS_ALT", m$parameter_id)])
  td <- prepare_ts_data_multi(m, lab_params, thresh = 0, untransformed = ut)

  expect_s3_class(td, "data.frame")
  expect_true("original_value" %in% names(td))
  expect_true("lower" %in% names(td))
  expect_true("upper" %in% names(td))
  expect_false("original_category" %in% names(td))
})

test_that("prepare_ts_data_multi joins untransformed for categorical", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  ut <- sample_sdtm_data$untransformed

  cat_params <- unique(m$parameter_id[grepl("^RS_OVRLRESP=", m$parameter_id)])
  td <- prepare_ts_data_multi(m, cat_params, thresh = 0, untransformed = ut)

  expect_s3_class(td, "data.frame")
  expect_true("original_category" %in% names(td))
  expect_false("original_value" %in% names(td))
  expect_false("lower" %in% names(td))
})

test_that("prepare_ts_data_multi joins untransformed for VS (no ranges)", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  ut <- sample_sdtm_data$untransformed

  vs_params <- unique(m$parameter_id[grepl("^VS_SYSBP", m$parameter_id)])
  td <- prepare_ts_data_multi(m, vs_params, thresh = 0, untransformed = ut)

  expect_s3_class(td, "data.frame")
  expect_true("original_value" %in% names(td))
  expect_false("lower" %in% names(td))
  expect_false("upper" %in% names(td))
  expect_false("original_category" %in% names(td))
})

test_that("prepare_ts_data_multi with NULL untransformed shows result only", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)

  lab_params <- unique(m$parameter_id[grepl("^LB_NORM_ALT", m$parameter_id)])
  td <- prepare_ts_data_multi(m, lab_params, thresh = 0, untransformed = NULL)

  expect_true("result" %in% names(td))
  expect_false("original_value" %in% names(td))
  expect_false("lower" %in% names(td))
})

test_that("prepare_ts_data_multi preserves row count with untransformed", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  ut <- sample_sdtm_data$untransformed

  lab_params <- unique(m$parameter_id[grepl("^LB_NORM_ALT|^LB_MISS_ALT", m$parameter_id)])
  td_with <- prepare_ts_data_multi(m, lab_params, thresh = 0, untransformed = ut)
  td_without <- prepare_ts_data_multi(m, lab_params, thresh = 0, untransformed = NULL)

  expect_equal(nrow(td_with), nrow(td_without))
})

test_that("recompute_max_score with NULL features returns same max_score", {
  m <- prepare_measures(sample_ctas_data, sample_ctas_results)
  m2 <- recompute_max_score(m, sample_ctas_results, features = NULL)
  expect_equal(m2$max_score, m$max_score)
})

test_that("recompute_max_score with subset features changes max_score", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  all_feats <- unique(sample_sdtm_results$site_scores$feature)
  one_feat <- all_feats[1]

  m2 <- recompute_max_score(m, sample_sdtm_results, features = one_feat)
  expect_true(all(m2$max_score <= m$max_score))
  expect_true(all(m2$max_score >= 0))
})

test_that("recompute_max_score with non-existent feature zeroes scores", {
  m <- prepare_measures(sample_ctas_data, sample_ctas_results)
  m2 <- recompute_max_score(m, sample_ctas_results, features = "nonexistent_feat")
  expect_true(all(m2$max_score == 0))
})

test_that("prepare_score_table_multi features param filters columns", {
  all_feats <- unique(sample_sdtm_results$site_scores$feature)
  one_feat <- all_feats[1]

  params <- unique(sample_sdtm_results$timeseries$parameter_id)[1]
  st_all <- prepare_score_table_multi(sample_sdtm_results, params)
  st_one <- prepare_score_table_multi(sample_sdtm_results, params,
                                      features = one_feat)

  all_feat_cols <- setdiff(names(st_all), c("site", "max_score"))
  one_feat_cols <- setdiff(names(st_one), c("site", "max_score"))

  expect_true(length(all_feat_cols) >= length(one_feat_cols))
  expect_true(all(one_feat_cols %in% all_feat_cols))
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


# -- validate_upload_results ---------------------------------------------------

test_that("validate_upload_results passes for valid data", {
  df <- data.frame(
    site = "A", timeseries_id = "ts1", parameter_id = "p1",
    feature = "autocorr", fdr_corrected_pvalue_logp = 2.5,
    stringsAsFactors = FALSE
  )
  expect_length(validate_upload_results(df), 0)
})

test_that("validate_upload_results catches non-dataframe", {
  errs <- validate_upload_results("not a df")
  expect_length(errs, 1)
  expect_true(grepl("data frame", errs))
})

test_that("validate_upload_results catches missing columns", {
  df <- data.frame(site = "A", feature = "sd")
  errs <- validate_upload_results(df)
  expect_true(any(grepl("missing columns", errs)))
})

test_that("validate_upload_results catches non-numeric score", {
  df <- data.frame(
    site = "A", timeseries_id = "ts1", parameter_id = "p1",
    feature = "sd", fdr_corrected_pvalue_logp = "bad",
    stringsAsFactors = FALSE
  )
  errs <- validate_upload_results(df)
  expect_true(any(grepl("numeric", errs)))
})

# -- validate_upload_input -----------------------------------------------------

test_that("validate_upload_input passes for valid data", {
  df <- data.frame(
    subject_id = "S1", site = "A", parameter_id = "p1",
    parameter_name = "Param1", parameter_category_3 = "numeric",
    timepoint_1_name = "V1", timepoint_rank = 1, result = 5.0,
    stringsAsFactors = FALSE
  )
  expect_length(validate_upload_input(df), 0)
})

test_that("validate_upload_input catches non-dataframe", {
  errs <- validate_upload_input(42)
  expect_length(errs, 1)
})

test_that("validate_upload_input catches missing columns", {
  df <- data.frame(subject_id = "S1")
  errs <- validate_upload_input(df)
  expect_true(any(grepl("missing columns", errs)))
})

test_that("validate_upload_input catches non-numeric result", {
  df <- data.frame(
    subject_id = "S1", site = "A", parameter_id = "p1",
    parameter_name = "Param1", parameter_category_3 = "numeric",
    timepoint_1_name = "V1", timepoint_rank = 1, result = "bad",
    stringsAsFactors = FALSE
  )
  errs <- validate_upload_input(df)
  expect_true(any(grepl("result.*numeric", errs)))
})

test_that("validate_upload_input catches non-numeric timepoint_rank", {
  df <- data.frame(
    subject_id = "S1", site = "A", parameter_id = "p1",
    parameter_name = "Param1", parameter_category_3 = "numeric",
    timepoint_1_name = "V1", timepoint_rank = "bad", result = 5.0,
    stringsAsFactors = FALSE
  )
  errs <- validate_upload_input(df)
  expect_true(any(grepl("timepoint_rank.*numeric", errs)))
})

test_that("validate_upload_input accepts any parameter_category_3 value", {
  df <- data.frame(
    subject_id = "S1", site = "A", parameter_id = "p1",
    parameter_name = "Param1", parameter_category_3 = "category 3",
    timepoint_1_name = "V1", timepoint_rank = 1, result = 5.0,
    stringsAsFactors = FALSE
  )
  expect_length(validate_upload_input(df), 0)
})

# -- validate_upload_untransformed ---------------------------------------------

test_that("validate_upload_untransformed passes for valid data", {
  df <- data.frame(
    subject_id = "S1", parameter_category_2 = "ALT",
    timepoint_1_name = "V1", original_value = 10.0
  )
  expect_length(validate_upload_untransformed(df), 0)
})

test_that("validate_upload_untransformed catches non-dataframe", {
  errs <- validate_upload_untransformed(list(a = 1))
  expect_length(errs, 1)
})

test_that("validate_upload_untransformed catches missing columns", {
  df <- data.frame(subject_id = "S1")
  errs <- validate_upload_untransformed(df)
  expect_true(any(grepl("missing columns", errs)))
})

# -- validate_upload_queries ---------------------------------------------------

test_that("validate_upload_queries passes for valid data", {
  df <- data.frame(
    subject_id = "S1", parameter_id = "p1", visit = "V1",
    data_change = TRUE, stringsAsFactors = FALSE
  )
  expect_length(validate_upload_queries(df), 0)
})

test_that("validate_upload_queries catches non-dataframe", {
  errs <- validate_upload_queries(NULL)
  expect_length(errs, 1)
})

test_that("validate_upload_queries catches missing columns", {
  df <- data.frame(subject_id = "S1")
  errs <- validate_upload_queries(df)
  expect_true(any(grepl("missing columns", errs)))
})

test_that("validate_upload_queries catches non-logical data_change", {
  df <- data.frame(
    subject_id = "S1", parameter_id = "p1", visit = "V1",
    data_change = "yes", stringsAsFactors = FALSE
  )
  errs <- validate_upload_queries(df)
  expect_true(any(grepl("logical", errs)))
})

# -- validate_upload_crossfile -------------------------------------------------

test_that("validate_upload_crossfile returns empty for clean data", {
  results_df <- data.frame(
    site = "A", parameter_id = "p1",
    stringsAsFactors = FALSE
  )
  input_df <- data.frame(
    site = "A", parameter_id = "p1",
    stringsAsFactors = FALSE
  )
  warns <- validate_upload_crossfile(input_df, results_df)
  expect_length(warns, 0)
})

test_that("validate_upload_crossfile catches orphan parameter_ids in results", {
  results_df <- data.frame(
    site = "A", parameter_id = c("p1", "p_orphan"),
    stringsAsFactors = FALSE
  )
  input_df <- data.frame(
    site = "A", parameter_id = "p1",
    stringsAsFactors = FALSE
  )
  warns <- validate_upload_crossfile(input_df, results_df)
  expect_true(any(grepl("parameter_id", warns)))
})

test_that("validate_upload_crossfile catches orphan sites in results", {
  results_df <- data.frame(
    site = c("A", "B"), parameter_id = "p1",
    stringsAsFactors = FALSE
  )
  input_df <- data.frame(
    site = "A", parameter_id = "p1",
    stringsAsFactors = FALSE
  )
  warns <- validate_upload_crossfile(input_df, results_df)
  expect_true(any(grepl("site", warns)))
})

# -- reconstruct_from_upload ---------------------------------------------------

test_that("reconstruct_from_upload builds valid ctas structures", {
  input_df <- data.frame(
    subject_id = "S1", site = "A", parameter_id = "p1",
    parameter_name = "Param1", parameter_category_1 = "cat1",
    parameter_category_2 = "ALT", parameter_category_3 = "numeric",
    timepoint_1_name = "V1", timepoint_rank = 1, result = 5.0,
    timepoint_2_name = NA_character_, baseline = NA_real_,
    country = "US", region = "North", study = "STUDY-001",
    stringsAsFactors = FALSE
  )
  results_df <- data.frame(
    site = "A", timeseries_id = "ts1", parameter_id = "p1",
    feature = "sd", fdr_corrected_pvalue_logp = 3.0,
    stringsAsFactors = FALSE
  )

  out <- reconstruct_from_upload(input_df, results_df)

  expect_type(out, "list")
  expect_true("ctas_data" %in% names(out))
  expect_true("ctas_results" %in% names(out))

  cd <- out$ctas_data
  expect_true(all(c("data", "subjects", "parameters") %in% names(cd)))
  expect_true("study" %in% names(cd$subjects))
  expect_true("site" %in% names(cd$subjects))
  expect_null(cd$untransformed)
  expect_null(cd$queries)

  cr <- out$ctas_results
  expect_true("site_scores" %in% names(cr))
  expect_true("parameter_id" %in% names(cr$site_scores))
})

test_that("reconstruct_from_upload passes through optional files", {
  input_df <- data.frame(
    subject_id = "S1", site = "A", parameter_id = "p1",
    parameter_name = "Param1", parameter_category_3 = "numeric",
    timepoint_1_name = "V1", timepoint_rank = 1, result = 5.0,
    stringsAsFactors = FALSE
  )
  results_df <- data.frame(
    site = "A", timeseries_id = "ts1", parameter_id = "p1",
    feature = "sd", fdr_corrected_pvalue_logp = 3.0,
    stringsAsFactors = FALSE
  )
  ut_df <- data.frame(
    subject_id = "S1", parameter_category_2 = "ALT",
    timepoint_1_name = "V1", original_value = 50.0,
    stringsAsFactors = FALSE
  )
  q_df <- data.frame(
    subject_id = "S1", parameter_id = "p1", visit = "V1",
    data_change = FALSE, stringsAsFactors = FALSE
  )

  out <- reconstruct_from_upload(input_df, results_df, ut_df, q_df)
  expect_s3_class(out$ctas_data$untransformed, "data.frame")
  expect_s3_class(out$ctas_data$queries, "data.frame")
})

# -- aggregate_results ---------------------------------------------------------

test_that("aggregate_results collapses rows by max logp", {
  df <- data.frame(
    timeseries_id = c("ts1", "ts2", "ts3"),
    site = c("A", "A", "B"),
    feature = c("sd", "sd", "sd"),
    fdr_corrected_pvalue_logp = c(3.0, 5.0, 2.0),
    stringsAsFactors = FALSE
  )
  out <- aggregate_results(df)
  expect_false("timeseries_id" %in% names(out))
  expect_equal(nrow(out), 2)
  expect_equal(out$fdr_corrected_pvalue_logp[out$site == "A"], 5.0)
})

test_that("aggregate_results returns NA for all-NA groups", {
  df <- data.frame(
    timeseries_id = c("ts1", "ts2"),
    site = c("A", "A"),
    feature = c("sd", "sd"),
    fdr_corrected_pvalue_logp = c(NA_real_, NA_real_),
    stringsAsFactors = FALSE
  )
  out <- aggregate_results(df)
  expect_equal(nrow(out), 1)
  expect_true(is.na(out$fdr_corrected_pvalue_logp))
})

# -- prepare_ts_data_multi categorical branches --------------------------------

test_that("prepare_ts_data_multi categorical with untransformed selects original_category", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  ut <- sample_sdtm_data$untransformed
  cat_params <- unique(m$parameter_id[grepl("^RS_OVRLRESP=", m$parameter_id)])

  td <- prepare_ts_data_multi(m, cat_params, thresh = 0,
                              untransformed = ut, plot_type = "categorical")
  expect_s3_class(td, "data.frame")
  expect_true(nrow(td) > 0)
  # Categorical branch selects distinct rows without numeric columns
  expect_false("result" %in% names(td))
  expect_false("parameter_id" %in% names(td))
})

test_that("prepare_ts_data_multi categorical without untransformed returns minimal cols", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  cat_params <- unique(m$parameter_id[grepl("^RS_OVRLRESP=", m$parameter_id)])

  td <- prepare_ts_data_multi(m, cat_params, thresh = 0,
                              untransformed = NULL, plot_type = "categorical")
  expect_s3_class(td, "data.frame")
  expect_true(nrow(td) > 0)
  expect_true("site" %in% names(td))
  expect_false("result" %in% names(td))
})

# -- generate_sample_csv with NULL sdtm_categories ----------------------------

test_that("generate_sample_csv with NULL sdtm_categories exports all params", {
  tmp_dir <- file.path(tempdir(), "csv_all")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  files <- generate_sample_csv(tmp_dir, sdtm_categories = NULL)
  expect_true(file.exists(file.path(tmp_dir, "input.csv")))
  expect_true(file.exists(file.path(tmp_dir, "results.csv")))

  input <- utils::read.csv(file.path(tmp_dir, "input.csv"))
  # With NULL categories, all SDTM parameters should be included
  sdtm_params <- unique(sample_sdtm_data$parameters$parameter_id)
  input_params <- unique(input$parameter_id[input$study == "STUDY-002"])
  expect_true(all(sdtm_params %in% input_params))
})
