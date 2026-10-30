test_that("mod_DataInput_ui returns a shiny tag", {
  ui <- mod_DataInput_ui("test")
  expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"))
})

test_that("mod_DataInput_server returns full list of reactives", {
  shiny::testServer(mod_DataInput_server, {
    returned <- session$getReturned()
    expect_type(returned, "list")
    expected_names <- c("measures", "ctas_results", "untransformed",
                        "queries", "pd", "dataset_label", "studies")
    for (nm in expected_names) {
      expect_true(nm %in% names(returned), info = paste("missing:", nm))
    }
    expect_null(returned$measures())
    expect_null(returned$ctas_results())
    expect_null(returned$untransformed())
    expect_null(returned$queries())
    expect_null(returned$pd())
    expect_null(returned$dataset_label())
    expect_null(returned$studies())
  })
})

test_that("mod_DataInput_server loads gsm.ctas sample data on button click", {
  shiny::testServer(mod_DataInput_server, {
    session$setInputs(data_source = "sample", load_data = 1)

    returned <- session$getReturned()
    m <- returned$measures()
    res <- returned$ctas_results()
    ut <- returned$untransformed()

    expect_s3_class(m, "data.frame")
    expect_true(nrow(m) > 0)
    expect_true(all(c("categorical", "numeric", "range_normalized",
                      "ratio_missing") %in% m$parameter_category_3))

    expect_type(res, "list")
    expect_true("site_scores" %in% names(res))

    expect_s3_class(ut, "data.frame")
    expect_true(nrow(ut) > 0)
    expect_true(all(c("subject_id", "parameter_category_2", "timepoint_1_name",
                       "original_value") %in% names(ut)))
    expect_s3_class(returned$queries(), "data.frame")
    expect_s3_class(returned$pd(), "data.frame")
    expect_equal(returned$dataset_label(), "Sample data (gsm.ctas)")
    expect_null(returned$studies())
  })
})

test_that("mod_DataInput_server renders status after loading", {
  shiny::testServer(mod_DataInput_server, {
    session$setInputs(data_source = "sample", load_data = 1)

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

test_that("read_upload_file reads parquet", {
  tmp <- tempfile(fileext = ".parquet")
  on.exit(unlink(tmp), add = TRUE)
  arrow::write_parquet(data.frame(a = 1:3, b = letters[1:3]), tmp)
  df <- read_upload_file(tmp, "test.parquet")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 3)
  expect_true(all(c("a", "b") %in% names(df)))
})

# -- Shipped parquet sample files ----------------------------------------------

test_that("shipped parquet sample files load and validate", {
  ext <- system.file("extdata", package = "ctasapp")
  read_ext <- function(nm) {
    read_upload_file(file.path(ext, paste0(nm, ".parquet")), paste0(nm, ".parquet"))
  }
  results_df <- read_ext("results")
  input_df <- read_ext("input")

  expect_length(validate_upload_results(results_df), 0)
  expect_length(validate_upload_input(input_df), 0)
  expect_length(validate_upload_untransformed(read_ext("untransformed")), 0)
  expect_length(validate_upload_queries(read_ext("queries")), 0)
  expect_length(validate_upload_pd(read_ext("pd")), 0)

  out <- reconstruct_from_upload(input_df, aggregate_results(results_df))
  measures <- prepare_measures(out$ctas_data, out$ctas_results)
  expect_s3_class(measures, "data.frame")
  expect_true(nrow(measures) > 0)
})

# -- generate_sample_csv -------------------------------------------------------

test_that("generate_sample_csv copies parquet files", {
  tmp_dir <- tempfile("parquet_fixtures")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  files <- generate_sample_csv(tmp_dir, format = "parquet")
  expect_setequal(
    basename(files),
    paste0(c("results", "input", "untransformed", "queries", "pd"), ".parquet")
  )
  expect_true(all(file.exists(files)))
})

test_that("generate_sample_csv writes CSV files that validate", {
  tmp_dir <- tempfile("csv_fixtures")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  files <- generate_sample_csv(tmp_dir)
  expect_true(all(grepl("\\.csv$", files)))

  results_df <- read_upload_file(file.path(tmp_dir, "results.csv"), "results.csv")
  pd_df <- read_upload_file(file.path(tmp_dir, "pd.csv"), "pd.csv")
  expect_length(validate_upload_results(results_df), 0)
  expect_length(validate_upload_pd(pd_df), 0)
})


# -- Embedded data set ---------------------------------------------------------

setup_embedded_fixtures <- function() {
  tmp_dir <- tempfile("embedded_")
  dir.create(tmp_dir)
  generate_sample_csv(tmp_dir, format = "parquet")
  cfg <- default_config()
  cfg$embedded$results <- file.path(tmp_dir, "results.parquet")
  cfg$embedded$input <- file.path(tmp_dir, "input.parquet")
  cfg$embedded$untransformed <- file.path(tmp_dir, "untransformed.parquet")
  cfg$embedded$queries <- file.path(tmp_dir, "queries.parquet")
  apply_config(cfg)
  tmp_dir
}

test_that("mod_DataInput_server injects 'Embedded data set' option", {
  on.exit(apply_config(default_config()), add = TRUE)
  tmp_dir <- setup_embedded_fixtures()
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  shiny::testServer(mod_DataInput_server, {
    session$flushReact()
    expect_true(embedded_files_configured())
    # The inject observer ran during flushReact; option is added.
    # Then load via embedded source:
    session$setInputs(data_source = "embedded", load_data = 1)
    returned <- session$getReturned()
    m <- returned$measures()
    expect_s3_class(m, "data.frame")
    expect_true(nrow(m) > 0)
  })
})

test_that("mod_DataInput_server embedded mode errors when files missing", {
  on.exit(apply_config(default_config()), add = TRUE)
  cfg <- default_config()
  cfg$embedded$results <- "/nonexistent/results.csv"
  cfg$embedded$input <- "/nonexistent/input.csv"
  apply_config(cfg)

  shiny::testServer(mod_DataInput_server, {
    session$setInputs(data_source = "embedded", load_data = 1)
    returned <- session$getReturned()
    expect_null(returned$measures())
  })
})

test_that("embedded_panel renders existence markers", {
  on.exit(apply_config(default_config()), add = TRUE)
  tmp_dir <- setup_embedded_fixtures()
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  shiny::testServer(mod_DataInput_server, {
    panel <- output$embedded_panel
    expect_true(inherits(panel, "list") || is.character(panel$html))
  })
})
