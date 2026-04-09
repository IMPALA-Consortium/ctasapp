test_that("mod_DataInput_ui returns a shiny tag", {
  ui <- mod_DataInput_ui("test")
  expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"))
})

test_that("mod_DataInput_server returns full list of reactives", {
  shiny::testServer(mod_DataInput_server, {
    returned <- session$getReturned()
    expect_type(returned, "list")
    expected_names <- c("measures", "ctas_results", "untransformed",
                        "queries", "dataset_label", "studies")
    for (nm in expected_names) {
      expect_true(nm %in% names(returned), info = paste("missing:", nm))
    }
    expect_null(returned$measures())
    expect_null(returned$ctas_results())
    expect_null(returned$untransformed())
    expect_null(returned$queries())
    expect_null(returned$dataset_label())
    expect_null(returned$studies())
  })
})

test_that("mod_DataInput_server loads ctas sample data on button click", {
  shiny::testServer(mod_DataInput_server, {
    session$setInputs(data_source = "ctas", load_data = 1)

    returned <- session$getReturned()
    m <- returned$measures()
    res <- returned$ctas_results()

    expect_s3_class(m, "data.frame")
    expect_true(nrow(m) > 0)
    expect_true("parameter_id" %in% names(m))

    expect_type(res, "list")
    expect_true("site_scores" %in% names(res))
    expect_true("timeseries" %in% names(res))

    expect_null(returned$untransformed())
    expect_equal(returned$dataset_label(), "ctas sample")
    expect_null(returned$studies())
  })
})

test_that("mod_DataInput_server loads SDTM sample data on button click", {
  shiny::testServer(mod_DataInput_server, {
    session$setInputs(data_source = "sdtm", load_data = 1)

    returned <- session$getReturned()
    m <- returned$measures()
    res <- returned$ctas_results()
    ut <- returned$untransformed()

    expect_s3_class(m, "data.frame")
    expect_true(nrow(m) > 0)

    cats <- unique(m$parameter_category_3)
    expect_true("categorical" %in% cats || "bar" %in% cats ||
                  "range_normalized" %in% cats)

    expect_s3_class(ut, "data.frame")
    expect_true(nrow(ut) > 0)
    expect_true(all(c("subject_id", "parameter_category_2", "timepoint_1_name",
                       "original_value") %in% names(ut)))
    expect_equal(returned$dataset_label(), "SDTM sample")
    expect_null(returned$studies())
  })
})

test_that("mod_DataInput_server renders status after loading", {
  shiny::testServer(mod_DataInput_server, {
    session$setInputs(data_source = "ctas", load_data = 1)

    status <- output$status
    expect_true(inherits(status, "list") || is.character(status$html))
  })
})

# -- read_upload_file ----------------------------------------------------------

test_that("read_upload_file reads CSV", {
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(a = 1:3, b = letters[1:3]), tmp,
                   row.names = FALSE)
  df <- read_upload_file(tmp, "test.csv")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 3)
  expect_true(all(c("a", "b") %in% names(df)))
  unlink(tmp)
})

test_that("read_upload_file reads RDA", {
  tmp <- tempfile(fileext = ".rda")
  test_df <- data.frame(x = 1:5)
  save(test_df, file = tmp)
  df <- read_upload_file(tmp, "test.rda")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 5)
  unlink(tmp)
})

test_that("read_upload_file errors for RDA with no data frame", {
  tmp <- tempfile(fileext = ".rda")
  test_vec <- 1:10
  save(test_vec, file = tmp)
  expect_error(read_upload_file(tmp, "test.rda"), "No data frame")
  unlink(tmp)
})

test_that("read_upload_file errors for unsupported format", {
  tmp <- tempfile(fileext = ".xyz")
  writeLines("hello", tmp)
  expect_error(read_upload_file(tmp, "test.xyz"), "Unsupported")
  unlink(tmp)
})

test_that("read_upload_file errors for parquet without arrow", {
  skip_if(requireNamespace("arrow", quietly = TRUE),
          "arrow is installed; cannot test missing-arrow path")
  tmp <- tempfile(fileext = ".parquet")
  writeLines("dummy", tmp)
  expect_error(read_upload_file(tmp, "test.parquet"), "arrow")
  unlink(tmp)
})

# -- Upload with generated CSV fixtures ----------------------------------------

test_that("generated CSV fixtures load and validate", {
  tmp_dir <- tempfile("csv_fixtures")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  generate_sample_csv(tmp_dir)

  results_df <- read_upload_file(file.path(tmp_dir, "results.csv"), "results.csv")
  input_df <- read_upload_file(file.path(tmp_dir, "input.csv"), "input.csv")

  expect_length(validate_upload_results(results_df), 0)
  expect_length(validate_upload_input(input_df), 0)

  out <- reconstruct_from_upload(input_df, results_df)
  measures <- prepare_measures(out$ctas_data, out$ctas_results)
  expect_s3_class(measures, "data.frame")
  expect_true(nrow(measures) > 0)
  expect_true("study" %in% names(out$ctas_data$subjects))
  expect_true(length(unique(out$ctas_data$subjects$study)) > 1)
})

test_that("generated untransformed and queries CSVs validate", {
  tmp_dir <- tempfile("csv_fixtures")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  generate_sample_csv(tmp_dir)

  ut_df <- read_upload_file(file.path(tmp_dir, "untransformed.csv"), "untransformed.csv")
  q_df <- read_upload_file(file.path(tmp_dir, "queries.csv"), "queries.csv")

  expect_length(validate_upload_untransformed(ut_df), 0)

  if ("data_change" %in% names(q_df)) {
    q_df$data_change <- as.logical(q_df$data_change)
  }
  expect_length(validate_upload_queries(q_df), 0)
})
