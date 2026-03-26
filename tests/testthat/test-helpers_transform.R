test_that("normalize_by_range computes correct normalization", {
  expect_equal(normalize_by_range(5, 0, 10), 0.5)
  expect_equal(normalize_by_range(0, 0, 10), 0)
  expect_equal(normalize_by_range(10, 0, 10), 1)
  expect_equal(normalize_by_range(15, 0, 10), 1.5)
  expect_equal(normalize_by_range(-5, 0, 10), -0.5)
})

test_that("normalize_by_range handles NA and invalid ranges", {
  expect_true(is.na(normalize_by_range(5, NA, 10)))
  expect_true(is.na(normalize_by_range(5, 0, NA)))
  expect_true(is.na(normalize_by_range(5, 10, 10)))
  expect_true(is.na(normalize_by_range(5, 10, 5)))
})

test_that("normalize_by_range is vectorized", {
  values <- c(0, 5, 10, NA)
  result <- normalize_by_range(values, 0, 10)
  expect_equal(result, c(0, 0.5, 1, NA))
})

test_that("ratio_missing_over_time computes running ratio", {
  value <- c(1, NA, 3, NA, NA)
  subject_id <- rep("s1", 5)
  parameter_id <- rep("p1", 5)

  result <- ratio_missing_over_time(value, subject_id, parameter_id)
  expect_equal(result, c(0, 1/2, 1/3, 2/4, 3/5))
})

test_that("ratio_missing_over_time groups by subject and parameter", {
  value <- c(1, NA, 1, NA)
  subject_id <- c("s1", "s1", "s2", "s2")
  parameter_id <- c("p1", "p1", "p1", "p1")

  result <- ratio_missing_over_time(value, subject_id, parameter_id)
  expect_equal(result, c(0, 1/2, 0, 1/2))
})

test_that("ratio_missing_over_time handles all non-missing", {
  value <- c(1, 2, 3)
  result <- ratio_missing_over_time(value, rep("s1", 3), rep("p1", 3))
  expect_equal(result, c(0, 0, 0))
})

test_that("ratio_missing_over_time handles all missing", {
  value <- c(NA, NA, NA)
  result <- ratio_missing_over_time(value, rep("s1", 3), rep("p1", 3))
  expect_equal(result, c(1, 1, 1))
})

test_that("recalculate_timepoint_rank computes sequential ranks", {
  df <- data.frame(
    subject_id = c("s1", "s1", "s1", "s2", "s2"),
    parameter_id = c("p1", "p1", "p1", "p1", "p1"),
    timepoint_1_name = c("WEEK 2", "WEEK 4", "BASELINE", "WEEK 2", "BASELINE"),
    timepoint_rank = c(3, 4, 1, 3, 1),
    result = 1:5,
    stringsAsFactors = FALSE
  )

  result <- recalculate_timepoint_rank(df)
  s1 <- result[result$subject_id == "s1", ]
  expect_equal(s1$timepoint_rank, 1:3)
  expect_equal(s1$timepoint_1_name, c("BASELINE", "WEEK 2", "WEEK 4"))

  s2 <- result[result$subject_id == "s2", ]
  expect_equal(s2$timepoint_rank, 1:2)
})

test_that("arrange_timepoints orders screening first, discontinuation last", {
  visits <- c("WEEK 4", "SCREENING 1", "DISCONTINUATION", "WEEK 2", "SCREENING 2")
  result <- arrange_timepoints(visits)

  expect_s3_class(result, "factor")
  lvls <- levels(result)
  expect_equal(lvls[1:2], c("SCREENING 1", "SCREENING 2"))
  expect_equal(lvls[length(lvls)], "DISCONTINUATION")
})

test_that("arrange_timepoints handles no screening or discontinuation", {
  visits <- c("WEEK 4", "WEEK 2", "BASELINE")
  result <- arrange_timepoints(visits)
  expect_equal(levels(result), c("BASELINE", "WEEK 2", "WEEK 4"))
})

test_that("encode_categorical returns correct structure", {
  result <- encode_categorical(c("A", "B", "A"), prefix = "TEST")

  expect_s3_class(result, "data.frame")
  expect_equal(sort(names(result)), c("encoded", "level", "orig_row"))
  expect_true(all(result$encoded %in% c(0, 1)))
})

test_that("encode_categorical one-hot encodes correctly", {
  result <- encode_categorical(c("A", "B", "A"), prefix = "X")

  a_rows <- result[result$level == "X=A", ]
  expect_equal(a_rows$encoded, c(1, 0, 1))
  expect_equal(a_rows$orig_row, c(1, 2, 3))

  b_rows <- result[result$level == "X=B", ]
  expect_equal(b_rows$encoded, c(0, 1, 0))
})

test_that("encode_categorical skips NAs", {
  result <- encode_categorical(c("A", NA, "B"), prefix = "T")

  expect_true(all(result$orig_row %in% c(1, 3)))
  expect_equal(nrow(result), 4)
})

test_that("encode_categorical returns empty data frame for all NA", {
  result <- encode_categorical(c(NA, NA), prefix = "T")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(sort(names(result)), c("encoded", "level", "orig_row"))
})
