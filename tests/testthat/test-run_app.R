test_that("run_ctas_app returns a shiny.appobj", {
  app <- run_ctas_app()
  expect_s3_class(app, "shiny.appobj")
})

test_that("run_ctas_app server wires modules correctly", {
  app <- run_ctas_app()
  shiny::testServer(app$serverFuncSource(), {
    expect_type(data, "list")
    expect_true("measures" %in% names(data))
    expect_true("ctas_results" %in% names(data))
  })
})

test_that("run_ctas_app accepts custom config path", {
  tmp <- tempfile(fileext = ".yml")
  writeLines(c(
    "colors:",
    "  query_no_change: '#ABCDEF'"
  ), tmp)
  app <- run_ctas_app(config = tmp)
  expect_s3_class(app, "shiny.appobj")
  expect_equal(get_query_colors()$no_change, "#ABCDEF")
  unlink(tmp)

  apply_config(default_config())
})

test_that("run_ctas_app works with NULL config", {
  app <- run_ctas_app(config = NULL)
  expect_s3_class(app, "shiny.appobj")
})
