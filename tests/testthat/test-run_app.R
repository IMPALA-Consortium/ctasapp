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
