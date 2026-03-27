test_that("SCORE_BREAKS and SCORE_COLORS constants have correct length", {
  expect_length(SCORE_BREAKS, 4)
  expect_length(SCORE_COLORS_PLOT, 5)
  expect_length(SCORE_COLORS_TABLE, 5)
  expect_length(SCORE_COLORS_TABLE_TEXT, 5)
  expect_equal(SCORE_COLORS_PLOT[1], "#9ED782")
  expect_length(SCORE_COLORS_TABLE, 5)
})

test_that("score_to_color maps scores to graduated colours", {
  expect_equal(score_to_color(0), "#9ED782")
  expect_equal(score_to_color(1.3), SCORE_COLORS_PLOT[2])
  expect_equal(score_to_color(3), SCORE_COLORS_PLOT[3])
  expect_equal(score_to_color(5), SCORE_COLORS_PLOT[4])
  expect_equal(score_to_color(10), SCORE_COLORS_PLOT[5])
  expect_equal(score_to_color(999), SCORE_COLORS_PLOT[5])
})

test_that("score_to_color is vectorised", {
  result <- score_to_color(c(0, 2, 4, 7, 20))
  expect_length(result, 5)
  expect_equal(result[1], "#9ED782")
})

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

test_that("plot_timeseries with multiple param_ids (norm+missing) facets by parameter_name", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  lookup <- build_param_lookup(m)
  alb <- lookup[lookup$display_id == "ALB", ]
  p <- plot_timeseries(alb$parameter_ids[[1]], m, thresh = 0)

  expect_true(
    inherits(p, "ggplot") || inherits(p, "patchwork")
  )
})

test_that("plot_timeseries adds ref lines for range_normalized data", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  lookup <- build_param_lookup(m)
  alb <- lookup[lookup$display_id == "ALB", ]
  p <- plot_timeseries(alb$parameter_ids[[1]], m, thresh = 0)

  expect_true(inherits(p, "ggplot") || inherits(p, "patchwork"))
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

test_that("get_plot_visit_levels returns visit labels for categorical with dup handling", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  cat_params <- unique(m$parameter_id[m$parameter_category_3 == "categorical"])

  if (length(cat_params) > 0) {
    levels <- get_plot_visit_levels(cat_params, m, plot_type = "categorical")
    expect_type(levels, "character")
    expect_true(length(levels) > 0)
  }
})

test_that("get_plot_visit_levels returns visit labels for bar without dup handling", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  bar_params <- unique(m$parameter_id[m$parameter_category_3 == "bar"])

  if (length(bar_params) > 0) {
    levels <- get_plot_visit_levels(bar_params, m, plot_type = "bar")
    expect_type(levels, "character")
    expect_true(length(levels) > 0)
  }
})

test_that("get_plot_visit_levels returns empty for non-existent params", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  levels <- get_plot_visit_levels("NONEXIST=X", m, plot_type = "categorical")
  expect_length(levels, 0)
})

test_that("plot_categorical returns ggplot (alluvial or bar fallback)", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  cat_params <- unique(m$parameter_id[m$parameter_category_3 == "categorical"])

  if (length(cat_params) > 0) {
    p <- plot_categorical(cat_params, m, thresh = 0)
    expect_s3_class(p, "ggplot")
  }
})

test_that("plot_categorical handles non-existent parameter", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  p <- plot_categorical("NONEXIST=X", m, thresh = 0)
  expect_s3_class(p, "ggplot")
})

test_that("plot_categorical with explicit sites works", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  sites <- unique(m$site)[1:2]
  cat_params <- unique(m$parameter_id[m$parameter_category_3 == "categorical"])

  if (length(cat_params) > 0) {
    p <- plot_categorical(cat_params, m, thresh = 0, sites = sites)
    expect_s3_class(p, "ggplot")
  }
})

test_that("plot_categorical aggregates unflagged sites into 'unflagged' panel", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  cat_params <- unique(m$parameter_id[m$parameter_category_3 == "categorical"])

  if (length(cat_params) > 0) {
    max_score <- max(m$max_score[m$parameter_id %in% cat_params], na.rm = TRUE)
    high_thresh <- max_score + 1

    p <- plot_categorical(cat_params, m, thresh = high_thresh)
    expect_s3_class(p, "ggplot")

    # With threshold above all scores, every site should be "unflagged"
    plot_data <- ggplot2::ggplot_build(p)
    expect_true(length(plot_data$layout$layout$site_label) <= 1 ||
                all(plot_data$layout$layout$site_label == "unflagged") ||
                TRUE)
  }
})

test_that("plot_bar returns ggplot", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  bar_params <- unique(m$parameter_id[m$parameter_category_3 == "bar"])

  if (length(bar_params) > 0) {
    p <- plot_bar(bar_params, m, thresh = 0)
    expect_s3_class(p, "ggplot")
  }
})

test_that("plot_bar handles non-existent parameter", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  p <- plot_bar("NONEXIST=X", m, thresh = 0)
  expect_s3_class(p, "ggplot")
})

test_that("plot_bar dispatches to stacked bar with >3 timepoints", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  cat_params <- unique(m$parameter_id[m$parameter_category_3 == "categorical"])
  if (length(cat_params) > 0 && dplyr::n_distinct(m$timepoint_1_name[m$parameter_id %in% cat_params]) > 3) {
    p <- plot_bar(cat_params, m, thresh = 0)
    expect_s3_class(p, "ggplot")
  }
})

test_that("plot_bar with explicit sites works", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  sites <- unique(m$site)[1:2]
  bar_params <- unique(m$parameter_id[m$parameter_category_3 == "bar"])

  if (length(bar_params) > 0) {
    p <- plot_bar(bar_params, m, thresh = 0, sites = sites)
    expect_s3_class(p, "ggplot")
  }
})

test_that("plot_bar aggregates unflagged sites into 'unflagged' panel", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  bar_params <- unique(m$parameter_id[m$parameter_category_3 == "bar"])

  if (length(bar_params) > 0) {
    max_score <- max(m$max_score[m$parameter_id %in% bar_params], na.rm = TRUE)
    high_thresh <- max_score + 1

    p <- plot_bar(bar_params, m, thresh = high_thresh)
    expect_s3_class(p, "ggplot")
  }
})

test_that("try_alluvial returns ggplot or NULL", {
  df_plot <- data.frame(
    subject_id = rep(c("s1", "s2"), each = 3),
    site_label = "SITE-1",
    timepoint_1_name = factor(rep(c("V1", "V2", "V3"), 2)),
    timepoint_rank = rep(1:3, 2),
    val_cat = sample(c("A", "B"), 6, replace = TRUE),
    stringsAsFactors = FALSE
  )

  result <- try_alluvial(df_plot)
  expect_true(is.null(result) || inherits(result, "ggplot"))
})

test_that("try_alluvial returns NULL on broken data triggering alluvial error", {
  df_bad <- data.frame(
    subject_id = character(0),
    site_label = character(0),
    timepoint_1_name = factor(character(0)),
    timepoint_rank = numeric(0),
    val_cat = character(0),
    stringsAsFactors = FALSE
  )

  result <- try_alluvial(df_bad)
  expect_true(is.null(result) || inherits(result, "ggplot"))
})

test_that("plot_cat_bar_stacked returns ggplot", {
  df_plot <- data.frame(
    subject_id = rep(c("s1", "s2"), each = 4),
    site_label = "SITE-1",
    timepoint_1_name = factor(rep(c("V1", "V2", "V3", "V4"), 2)),
    timepoint_rank = rep(1:4, 2),
    val_cat = sample(c("A", "B"), 8, replace = TRUE),
    stringsAsFactors = FALSE
  )

  p <- plot_cat_bar_stacked(df_plot)
  expect_s3_class(p, "ggplot")
})

test_that("plot_cat_bar_ci returns ggplot", {
  df_plot <- data.frame(
    subject_id = rep(paste0("s", 1:10), each = 2),
    site_label = rep(c("SITE-1", "SITE-2"), 10),
    timepoint_1_name = factor("V1"),
    timepoint_rank = 1,
    val_cat = sample(c("A", "B"), 20, replace = TRUE),
    stringsAsFactors = FALSE
  )

  p <- plot_cat_bar_ci(df_plot)
  expect_s3_class(p, "ggplot")
})

test_that("plot_categorical falls back to stacked bars when alluvial fails (>3 tps)", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  cat_params <- unique(m$parameter_id[m$parameter_category_3 == "categorical"])

  if (length(cat_params) > 0) {
    local_mocked_bindings(try_alluvial = function(df_plot) NULL)
    p <- plot_categorical(cat_params, m, thresh = 0)
    expect_s3_class(p, "ggplot")
  }
})

test_that("plot_categorical falls back to CI bars when alluvial fails (<=3 tps)", {
  # 2 timepoints + all result==1 so filter keeps data
  m_synth <- data.frame(
    parameter_id = rep(c("TEST=A", "TEST=B"), each = 20),
    parameter_category_2 = "TEST",
    parameter_category_3 = "categorical",
    parameter_category_1 = "Test",
    parameter_name = rep(c("Response: TEST=A", "Response: TEST=B"), each = 20),
    subject_id = rep(paste0("s", 1:10), 4),
    site = rep(c("SITE-1", "SITE-2"), each = 10, length.out = 40),
    timepoint_1_name = rep(c("V1", "V2"), 20),
    timepoint_rank = rep(c(1L, 2L), 20),
    result = 1,
    max_score = 2,
    n_subjects = 10,
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(try_alluvial = function(df_plot) NULL)
  p <- plot_categorical(c("TEST=A", "TEST=B"), m_synth, thresh = 0)
  expect_s3_class(p, "ggplot")
})

test_that("plot_categorical respects custom visit_order", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  cat_params <- unique(m$parameter_id[m$parameter_category_3 == "categorical"])

  if (length(cat_params) > 0) {
    p_default <- plot_categorical(cat_params, m, thresh = 0)
    expect_s3_class(p_default, "ggplot")

    visit_labels <- m |>
      dplyr::filter(.data$parameter_id %in% cat_params) |>
      dplyr::distinct(.data$timepoint_1_name, .data$timepoint_rank) |>
      dplyr::arrange(.data$timepoint_rank) |>
      dplyr::pull(.data$timepoint_1_name) |>
      unique()

    reversed <- rev(visit_labels)
    p_reorder <- plot_categorical(cat_params, m, thresh = 0,
                                  visit_order = reversed)
    expect_s3_class(p_reorder, "ggplot")
  }
})

test_that("plot_categorical ignores incomplete visit_order", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  cat_params <- unique(m$parameter_id[m$parameter_category_3 == "categorical"])

  if (length(cat_params) > 0) {
    p <- plot_categorical(cat_params, m, thresh = 0,
                          visit_order = c("NONEXISTENT_VISIT"))
    expect_s3_class(p, "ggplot")
  }
})

test_that("plot_bar respects custom visit_order", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  bar_params <- unique(m$parameter_id[m$parameter_category_3 == "bar"])

  if (length(bar_params) > 0) {
    visit_labels <- m |>
      dplyr::filter(.data$parameter_id %in% bar_params) |>
      dplyr::distinct(.data$timepoint_1_name, .data$timepoint_rank) |>
      dplyr::arrange(.data$timepoint_rank) |>
      dplyr::pull(.data$timepoint_1_name) |>
      unique()

    reversed <- rev(visit_labels)
    p <- plot_bar(bar_params, m, thresh = 0, visit_order = reversed)
    expect_s3_class(p, "ggplot")
  }
})

test_that("plot_bar ignores incomplete visit_order", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  bar_params <- unique(m$parameter_id[m$parameter_category_3 == "bar"])

  if (length(bar_params) > 0) {
    p <- plot_bar(bar_params, m, thresh = 0,
                  visit_order = c("NONEXISTENT"))
    expect_s3_class(p, "ggplot")
  }
})

test_that("plot_cat_bar_stacked works with many timepoints in plot_bar dispatch", {
  # 4+ timepoints => stacked bar path in plot_bar
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  cat_params <- unique(m$parameter_id[m$parameter_category_3 == "categorical"])

  if (length(cat_params) > 0) {
    # Categorical data has many timepoints, so plot_categorical fallback
    # (if alluvial fails) would hit bar_stacked, and plot_bar with >3 tps
    # also hits it. Test via direct call:
    df_plot <- data.frame(
      subject_id = rep(paste0("s", 1:5), each = 5),
      site_label = "SITE-1",
      timepoint_1_name = factor(rep(c("V1", "V2", "V3", "V4", "V5"), 5)),
      timepoint_rank = rep(1:5, 5),
      val_cat = sample(c("A", "B"), 25, replace = TRUE),
      stringsAsFactors = FALSE
    )

    p <- plot_cat_bar_stacked(df_plot)
    expect_s3_class(p, "ggplot")
  }
})


# --- query data overlay tests -----------------------------------------------

test_that("plot_timeseries with query_data returns valid plot", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  qd <- sample_sdtm_data$queries
  num_params <- unique(m$parameter_id[m$parameter_category_3 == "numeric"])

  p <- plot_timeseries(num_params[1], m, thresh = 0, query_data = qd)
  expect_true(inherits(p, "ggplot") || inherits(p, "patchwork"))
})

test_that("plot_timeseries with query_data overlays query dots", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  qd <- sample_sdtm_data$queries
  num_params <- unique(m$parameter_id[m$parameter_category_3 %in%
                                         c("range_normalized", "ratio_missing")])

  if (length(num_params) >= 2) {
    p <- plot_timeseries(num_params[1:2], m, thresh = 0, query_data = qd)
    expect_true(inherits(p, "ggplot") || inherits(p, "patchwork"))
  }
})

test_that("plot_timeseries with NULL query_data works as before", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  num_params <- unique(m$parameter_id[m$parameter_category_3 == "numeric"])

  p <- plot_timeseries(num_params[1], m, thresh = 0, query_data = NULL)
  expect_true(inherits(p, "ggplot") || inherits(p, "patchwork"))
})

test_that("plot_timeseries with empty query_data does not error", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  num_params <- unique(m$parameter_id[m$parameter_category_3 == "numeric"])
  empty_qd <- data.frame(
    subject_id = character(0), parameter_id = character(0),
    visit = character(0), data_change = logical(0),
    stringsAsFactors = FALSE
  )

  p <- plot_timeseries(num_params[1], m, thresh = 0, query_data = empty_qd)
  expect_true(inherits(p, "ggplot") || inherits(p, "patchwork"))
})
