test_that("mod_FieldDetail_ui returns a shiny tag", {
  ui <- mod_FieldDetail_ui("test")
  expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"))
})

test_that("mod_FieldDetail_server renders outputs", {
  m <- prepare_measures(sample_ctas_data, sample_ctas_results)

  shiny::testServer(
    mod_FieldDetail_server,
    args = list(
      rctv_measures = shiny::reactiveVal(m),
      rctv_ctas_results = shiny::reactiveVal(sample_ctas_results)
    ),
    {
      session$setInputs(thresh = 1.3)
      session$flushReact()

      stats <- param_outliers()
      expect_s3_class(stats, "data.frame")
      expect_true("parameter_id" %in% names(stats))
      expect_true("n_outlier_sites" %in% names(stats))
      expect_true(all(stats$n_outlier_sites >= 0))

      session$setInputs(selected_param = stats$parameter_id[1])

      expect_type(output$plot_title, "character")
      expect_true(grepl("Timeseries:", output$plot_title))
    }
  )
})

test_that("mod_FieldDetail_server param_outliers reacts to threshold changes", {
  m <- prepare_measures(sample_ctas_data, sample_ctas_results)

  shiny::testServer(
    mod_FieldDetail_server,
    args = list(
      rctv_measures = shiny::reactiveVal(m),
      rctv_ctas_results = shiny::reactiveVal(sample_ctas_results)
    ),
    {
      session$setInputs(thresh = 0)
      stats_low <- param_outliers()
      n_low <- sum(stats_low$n_outlier_sites)

      session$setInputs(thresh = 99999)
      stats_high <- param_outliers()
      n_high <- sum(stats_high$n_outlier_sites)

      expect_true(n_low >= n_high)
      expect_equal(n_high, 0)
    }
  )
})
