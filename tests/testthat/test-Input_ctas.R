make_dm <- function(n = 10) {
  data.frame(
    USUBJID = paste0("SUBJ-", seq_len(n)),
    SITEID = paste0("SITE-", rep(1:2, length.out = n)),
    COUNTRY = "USA",
    stringsAsFactors = FALSE
  )
}

make_lb <- function(dm, n_visits = 5) {
  expand.grid(
    USUBJID = dm$USUBJID,
    VISITNUM = seq_len(n_visits),
    LBTESTCD = c("ALT", "AST"),
    stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(
      VISIT = paste("WEEK", .data$VISITNUM * 2),
      LBTEST = ifelse(.data$LBTESTCD == "ALT", "Alanine Aminotransferase", "Aspartate Aminotransferase"),
      LBSTRESN = stats::rnorm(dplyr::n(), 30, 10),
      LBSTNRLO = 10,
      LBSTNRHI = 50
    )
}

make_vs <- function(dm, n_visits = 5) {
  expand.grid(
    USUBJID = dm$USUBJID,
    VISITNUM = seq_len(n_visits),
    VSTESTCD = c("SYSBP", "WEIGHT"),
    stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(
      VISIT = paste("WEEK", .data$VISITNUM * 2),
      VSTEST = ifelse(.data$VSTESTCD == "SYSBP", "Systolic Blood Pressure", "Weight"),
      VSSTRESN = ifelse(.data$VSTESTCD == "SYSBP",
                        stats::rnorm(dplyr::n(), 120, 15),
                        stats::rnorm(dplyr::n(), 75, 15)),
      VSBLFL = ifelse(.data$VISITNUM == 1, "Y", "")
    )
}

make_rs <- function(dm, n_visits = 3) {
  expand.grid(
    USUBJID = dm$USUBJID,
    VISITNUM = seq_len(n_visits),
    stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(
      VISIT = paste("WEEK", .data$VISITNUM * 2),
      RSTESTCD = "OVRLRESP",
      RSTEST = "Overall Response",
      RSORRES = sample(c("CR", "PR", "SD"), dplyr::n(), replace = TRUE)
    )
}


test_that("build_subjects extracts correct columns", {
  dm <- make_dm(5)
  subj <- build_subjects(dm)

  expect_s3_class(subj, "data.frame")
  expect_equal(sort(names(subj)), c("country", "region", "site", "subject_id"))
  expect_equal(nrow(subj), 5)
})


test_that("filter_unscheduled removes unscheduled visits", {
  df <- data.frame(
    timepoint_1_name = c("WEEK 2", "UNSCHEDULED 3.1", "BASELINE", "Unscheduled 1"),
    value = 1:4,
    stringsAsFactors = FALSE
  )
  result <- filter_unscheduled(df)
  expect_equal(nrow(result), 2)
  expect_equal(result$timepoint_1_name, c("WEEK 2", "BASELINE"))
})


test_that("Input_Labs returns correct structure with norm and miss params sharing category_2", {
  dm <- make_dm()
  lb <- make_lb(dm)
  result <- Input_Labs(dm, lb)

  expect_type(result, "list")
  expect_true(all(c("data", "subjects", "parameters", "untransformed") %in% names(result)))
  expect_s3_class(result$data, "data.frame")

  expect_true(any(grepl("^LB_NORM_", result$parameters$parameter_id)))
  expect_true(any(grepl("^LB_MISS_", result$parameters$parameter_id)))

  # Norm and miss for same test share parameter_category_2
  alt_params <- result$parameters[grepl("ALT", result$parameters$parameter_id), ]
  expect_equal(length(unique(alt_params$parameter_category_2)), 1)
  expect_equal(unique(alt_params$parameter_category_2), "ALT")
})


test_that("Input_Labs untransformed has original values and ranges", {
  dm <- make_dm()
  lb <- make_lb(dm)
  result <- Input_Labs(dm, lb)

  ut <- result$untransformed
  expect_s3_class(ut, "data.frame")
  expect_true(nrow(ut) > 0)
  expect_true(all(c("subject_id", "parameter_category_2", "timepoint_1_name",
                     "original_value", "lower", "upper", "original_category") %in% names(ut)))
  expect_true(all(!is.na(ut$original_value)))
  expect_true(all(!is.na(ut$lower)))
  expect_true(all(!is.na(ut$upper)))
  expect_true(all(is.na(ut$original_category)))
  expect_true(all(ut$parameter_category_2 %in% c("ALT", "AST")))
})


test_that("Input_Labs recalculates timepoint_rank", {
  dm <- make_dm(3)
  lb <- make_lb(dm, n_visits = 3)
  result <- Input_Labs(dm, lb)

  subj1_norm <- result$data |>
    dplyr::filter(
      .data$subject_id == "SUBJ-1",
      grepl("^LB_NORM_ALT", .data$parameter_id)
    )
  expect_equal(subj1_norm$timepoint_rank, seq_len(nrow(subj1_norm)))
})


test_that("Input_Labs filters unscheduled visits", {
  dm <- make_dm(3)
  lb <- make_lb(dm, n_visits = 2)
  lb$VISIT[1] <- "UNSCHEDULED 1.1"
  result <- Input_Labs(dm, lb)

  expect_false(any(grepl("UNSCH", result$data$timepoint_1_name, ignore.case = TRUE)))
})


test_that("Input_VS returns correct structure with numeric params", {
  dm <- make_dm()
  vs <- make_vs(dm)
  result <- Input_VS(dm, vs)

  expect_type(result, "list")
  expect_true("untransformed" %in% names(result))
  expect_true(all(result$parameters$parameter_category_3 == "numeric"))
  expect_true(any(grepl("^VS_", result$parameters$parameter_id)))
})


test_that("Input_VS untransformed has original values without ranges", {
  dm <- make_dm()
  vs <- make_vs(dm)
  result <- Input_VS(dm, vs)

  ut <- result$untransformed
  expect_s3_class(ut, "data.frame")
  expect_true(nrow(ut) > 0)
  expect_true(all(!is.na(ut$original_value)))
  expect_true(all(is.na(ut$lower)))
  expect_true(all(is.na(ut$upper)))
  expect_true(all(is.na(ut$original_category)))
})


test_that("Input_VS recalculates timepoint_rank and filters unscheduled", {
  dm <- make_dm(3)
  vs <- make_vs(dm, n_visits = 3)
  vs$VISIT[1] <- "UNSCHEDULED 3.1"
  result <- Input_VS(dm, vs)

  expect_false(any(grepl("UNSCH", result$data$timepoint_1_name, ignore.case = TRUE)))
})


test_that("Input_RS returns categorical one-hot encoded parameters", {
  dm <- make_dm()
  rs <- make_rs(dm)
  result <- Input_RS(dm, rs)

  expect_type(result, "list")
  expect_true(all(result$parameters$parameter_category_3 == "categorical"))
  expect_true(all(grepl("^RS_OVRLRESP=", result$parameters$parameter_id)))
  expect_true(all(result$data$result %in% c(0, 1)))
  expect_equal(unique(result$parameters$parameter_category_2), "RS_OVRLRESP")
})


test_that("Input_RS untransformed has original categories", {
  dm <- make_dm()
  rs <- make_rs(dm)
  result <- Input_RS(dm, rs)

  ut <- result$untransformed
  expect_s3_class(ut, "data.frame")
  expect_true(nrow(ut) > 0)
  expect_true(all(is.na(ut$original_value)))
  expect_true(all(!is.na(ut$original_category)))
  expect_true(all(ut$original_category %in% c("CR", "PR", "SD")))
  expect_equal(unique(ut$parameter_category_2), "RS_OVRLRESP")
})


test_that("Input_RS returns empty structure for missing test code", {
  dm <- make_dm()
  rs <- make_rs(dm)
  result <- Input_RS(dm, rs, strTestCD = "NONEXIST")

  expect_equal(nrow(result$data), 0)
  expect_equal(nrow(result$parameters), 0)
  expect_true("untransformed" %in% names(result))
  expect_equal(nrow(result$untransformed), 0)
})


test_that("Input_BMI returns bar-type parameters", {
  dm <- make_dm()
  vs <- make_vs(dm)
  result <- Input_BMI(dm, vs)

  expect_type(result, "list")
  expect_true("untransformed" %in% names(result))
  expect_true(all(result$parameters$parameter_category_3 == "bar"))
  expect_true(all(grepl("^VS_WEIGHT_CAT=", result$parameters$parameter_id)))
})


test_that("Input_BMI untransformed has weight values and categories", {
  dm <- make_dm()
  vs <- make_vs(dm)
  result <- Input_BMI(dm, vs)

  ut <- result$untransformed
  expect_s3_class(ut, "data.frame")
  expect_true(nrow(ut) > 0)
  expect_true(all(!is.na(ut$original_value)))
  expect_true(all(!is.na(ut$original_category)))
  expect_equal(unique(ut$parameter_category_2), "VS_WEIGHT_CAT")
})


test_that("Input_BMI returns empty structure when no screening weights", {
  dm <- make_dm()
  vs_empty <- make_vs(dm) |>
    dplyr::filter(.data$VSTESTCD != "WEIGHT")
  result <- Input_BMI(dm, vs_empty)

  expect_equal(nrow(result$data), 0)
  expect_equal(nrow(result$parameters), 0)
  expect_true("untransformed" %in% names(result))
  expect_equal(nrow(result$untransformed), 0)
})


test_that("combine_ctas_input merges multiple inputs", {
  dm <- make_dm()
  lb <- make_lb(dm)
  vs <- make_vs(dm)

  input_labs <- Input_Labs(dm, lb)
  input_vs <- Input_VS(dm, vs)

  combined <- combine_ctas_input(input_labs, input_vs)

  expect_type(combined, "list")
  expect_true(all(c("data", "subjects", "parameters",
                     "custom_timeseries", "custom_reference_groups",
                     "untransformed") %in% names(combined)))

  expect_equal(
    nrow(combined$data),
    nrow(input_labs$data) + nrow(input_vs$data)
  )

  expect_true("time_point_count_min" %in% names(combined$parameters))

  expect_s3_class(combined$untransformed, "data.frame")
  expect_equal(
    nrow(combined$untransformed),
    nrow(input_labs$untransformed) + nrow(input_vs$untransformed)
  )
})


test_that("combine_ctas_input deduplicates subjects", {
  dm <- make_dm(5)
  lb <- make_lb(dm)
  vs <- make_vs(dm)

  input_labs <- Input_Labs(dm, lb)
  input_vs <- Input_VS(dm, vs)

  combined <- combine_ctas_input(input_labs, input_vs)
  expect_equal(nrow(combined$subjects), 5)
})

test_that("inject_missingness sets NA values and renames site IDs", {
  dm <- make_dm(10)
  lb <- make_lb(dm, n_visits = 5)

  injections <- data.frame(
    site = "SITE-1",
    lbtestcd = "ALT",
    frac = 0.5,
    stringsAsFactors = FALSE
  )

  result <- inject_missingness(dm, lb, injections, seed = 123)

  expect_true("SITE-1_MISS_ALT" %in% result$dm$SITEID)
  expect_false("SITE-1" %in% result$dm$SITEID)

  subj_at_site <- unique(result$dm$USUBJID[result$dm$SITEID == "SITE-1_MISS_ALT"])
  alt_rows <- result$lb$USUBJID %in% subj_at_site & result$lb$LBTESTCD == "ALT"
  na_count <- sum(is.na(result$lb$LBSTRESN[alt_rows]))
  expect_true(na_count > 0)

  ast_rows <- result$lb$USUBJID %in% subj_at_site & result$lb$LBTESTCD == "AST"
  expect_equal(sum(is.na(result$lb$LBSTRESN[ast_rows])), 0)
})


test_that("inject_missingness handles zero-fraction gracefully", {
  dm <- make_dm(4)
  lb <- make_lb(dm, n_visits = 3)

  injections <- data.frame(
    site = "SITE-1",
    lbtestcd = "ALT",
    frac = 0,
    stringsAsFactors = FALSE
  )

  result <- inject_missingness(dm, lb, injections, seed = 42)
  expect_true("SITE-1_MISS_ALT" %in% result$dm$SITEID)
  expect_equal(sum(is.na(result$lb$LBSTRESN)), 0)
})


test_that("combine_ctas_input returns NULL untransformed when inputs lack it", {
  dm <- make_dm(3)
  lb <- make_lb(dm)
  input_labs <- Input_Labs(dm, lb)
  input_labs$untransformed <- NULL

  vs <- make_vs(dm)
  input_vs <- Input_VS(dm, vs)
  input_vs$untransformed <- NULL

  combined <- combine_ctas_input(input_labs, input_vs)
  expect_null(combined$untransformed)
})


# --- simulate_query_data tests -----------------------------------------------

test_that("simulate_query_data returns correct shape and columns", {
  dm <- make_dm(10)
  lb <- make_lb(dm, n_visits = 5)
  input_labs <- Input_Labs(dm, lb)
  vs <- make_vs(dm)
  input_vs <- Input_VS(dm, vs)
  ctas_input <- combine_ctas_input(input_labs, input_vs)

  qd <- simulate_query_data(ctas_input, seed = 1)

  expect_s3_class(qd, "data.frame")
  expected_cols <- c("study", "domain", "field", "subject_id", "visit",
                     "parameter_id", "query_text", "query_answer",
                     "query_status", "query_type", "data_change",
                     "value_first_entry", "value_at_query_open",
                     "value_at_query_close", "value_now")
  expect_true(all(expected_cols %in% names(qd)))
})

test_that("simulate_query_data samples approx 4% of rows", {
  dm <- make_dm(20)
  lb <- make_lb(dm, n_visits = 10)
  input_labs <- Input_Labs(dm, lb)
  ctas_input <- combine_ctas_input(input_labs)

  n_data <- nrow(ctas_input$data)
  qd <- simulate_query_data(ctas_input, seed = 42)

  expected <- round(n_data * 0.04)
  expect_equal(nrow(qd), expected)
})

test_that("simulate_query_data data_change rate is approx 20%", {
  dm <- make_dm(20)
  lb <- make_lb(dm, n_visits = 10)
  input_labs <- Input_Labs(dm, lb)
  ctas_input <- combine_ctas_input(input_labs)

  qd <- simulate_query_data(ctas_input, seed = 99)
  change_rate <- mean(qd$data_change)
  expect_true(change_rate > 0.05 && change_rate < 0.45)
})

test_that("simulate_query_data value_now matches actual data", {
  dm <- make_dm(10)
  lb <- make_lb(dm, n_visits = 5)
  input_labs <- Input_Labs(dm, lb)
  ctas_input <- combine_ctas_input(input_labs)

  qd <- simulate_query_data(ctas_input, seed = 7)

  for (i in sample.int(nrow(qd), min(5, nrow(qd)))) {
    row_match <- ctas_input$data$subject_id == qd$subject_id[i] &
      ctas_input$data$parameter_id == qd$parameter_id[i] &
      ctas_input$data$timepoint_1_name == qd$visit[i]
    if (any(row_match)) {
      actual <- round(ctas_input$data$result[which(row_match)[1]], 4)
      expect_equal(as.numeric(qd$value_now[i]), actual)
    }
  }
})

test_that("simulate_query_data handles categorical data_change with NA", {
  dm <- make_dm(10)
  rs <- make_rs(dm)
  input_rs <- Input_RS(dm, rs)
  ctas_input <- combine_ctas_input(input_rs)

  qd <- simulate_query_data(ctas_input, seed = 11, change_frac = 1.0)
  expect_true(any(is.na(qd$value_first_entry)))
})

test_that("simulate_query_data returns empty frame for zero-row data", {
  empty_input <- list(
    data = data.frame(
      subject_id = character(0), parameter_id = character(0),
      timepoint_1_name = character(0), timepoint_rank = numeric(0),
      result = numeric(0), timepoint_2_name = character(0),
      baseline = numeric(0), stringsAsFactors = FALSE
    ),
    parameters = data.frame(
      parameter_id = character(0), parameter_name = character(0),
      parameter_category_1 = character(0), parameter_category_2 = character(0),
      parameter_category_3 = character(0), stringsAsFactors = FALSE
    )
  )

  qd <- simulate_query_data(empty_input, seed = 1)
  expect_equal(nrow(qd), 0)
  expect_true("data_change" %in% names(qd))
})

test_that("simulate_query_data query_answer is NA for open queries", {
  dm <- make_dm(20)
  lb <- make_lb(dm, n_visits = 10)
  input_labs <- Input_Labs(dm, lb)
  ctas_input <- combine_ctas_input(input_labs)

  qd <- simulate_query_data(ctas_input, seed = 55)
  open_queries <- qd[qd$query_status == "Open", ]
  if (nrow(open_queries) > 0) {
    expect_true(all(is.na(open_queries$query_answer)))
  }
})

test_that("simulate_query_data seed is reproducible", {
  dm <- make_dm(10)
  lb <- make_lb(dm, n_visits = 5)
  input_labs <- Input_Labs(dm, lb)
  ctas_input <- combine_ctas_input(input_labs)

  qd1 <- simulate_query_data(ctas_input, seed = 42)
  qd2 <- simulate_query_data(ctas_input, seed = 42)
  expect_identical(qd1, qd2)
})
