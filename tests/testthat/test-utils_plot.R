test_that("plot_timeseries returns a ggplot/patchwork object", {
  m <- prepare_measures(sample_ctas_data, sample_ctas_results)
  p <- plot_timeseries("param1", m, thresh = 1.3)

  expect_true(
    inherits(p, "ggplot") || inherits(p, "patchwork"),
    info = "Expected ggplot or patchwork object"
  )
})

test_that("plot_timeseries handles non-existent parameter", {
  m <- prepare_measures(sample_ctas_data, sample_ctas_results)
  p <- plot_timeseries("nonexistent_param", m, thresh = 1.3)

  expect_s3_class(p, "ggplot")
})

test_that("plot_timeseries with explicit sites argument works", {
  m <- prepare_measures(sample_ctas_data, sample_ctas_results)
  sites <- unique(m$site[m$parameter_id == "param1"])[1:2]
  p <- plot_timeseries("param1", m, thresh = 1.3, sites = sites)

  expect_true(
    inherits(p, "ggplot") || inherits(p, "patchwork")
  )
})

test_that("plot_timeseries with zero threshold shows all sites", {
  m <- prepare_measures(sample_ctas_data, sample_ctas_results)
  p <- plot_timeseries("param1", m, thresh = 0)

  expect_true(
    inherits(p, "ggplot") || inherits(p, "patchwork")
  )
})

test_that("plot_timeseries with very high threshold still works", {
  m <- prepare_measures(sample_ctas_data, sample_ctas_results)
  p <- plot_timeseries("param1", m, thresh = 99999)

  expect_true(
    inherits(p, "ggplot") || inherits(p, "patchwork")
  )
})

test_that("format_score handles boundary values", {
  expect_equal(format_score(0), "0")
  expect_equal(format_score(20000), "> 10000")
  expect_equal(format_score(0.0005), "< 0.001")
  expect_equal(format_score(1.55), "1.6")
  expect_equal(format_score(0.05), "0.05")
  expect_equal(format_score(0.005), "0.005")
})

test_that("format_score handles vectors", {
  result <- format_score(c(0, 1.5, 20000))
  expect_length(result, 3)
  expect_type(result, "character")
})
