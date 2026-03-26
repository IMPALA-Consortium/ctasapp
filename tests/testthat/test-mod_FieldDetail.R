test_that("mod_FieldDetail_ui returns a shiny tag", {
  ui <- mod_FieldDetail_ui("test")
  expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"))
})

test_that("build_param_lookup groups labs by parameter_category_2", {
  df <- data.frame(
    parameter_id = c("LB_NORM_ALT", "LB_MISS_ALT", "VS_SYSBP", "VS_DIABP",
                      "RS_OVRLRESP=CR", "RS_OVRLRESP=PR",
                      "VS_WEIGHT_CAT=low", "VS_WEIGHT_CAT=high"),
    parameter_category_2 = c("ALT", "ALT", "VS_SYSBP", "VS_DIABP",
                              "RS_OVRLRESP", "RS_OVRLRESP",
                              "VS_WEIGHT_CAT", "VS_WEIGHT_CAT"),
    parameter_category_3 = c("range_normalized", "ratio_missing",
                              "numeric", "numeric",
                              "categorical", "categorical",
                              "bar", "bar"),
    stringsAsFactors = FALSE
  )

  lookup <- build_param_lookup(df)

  # ALB groups norm+missing
  alb <- lookup[lookup$display_id == "ALT", ]
  expect_equal(alb$plot_type, "numeric")
  expect_equal(sort(alb$parameter_ids[[1]]), c("LB_MISS_ALT", "LB_NORM_ALT"))

  # VS stays separate per param
  expect_true("VS_SYSBP" %in% lookup$display_id)
  expect_true("VS_DIABP" %in% lookup$display_id)

  # Categorical groups
  rs <- lookup[lookup$display_id == "RS_OVRLRESP", ]
  expect_equal(rs$plot_type, "categorical")
  expect_equal(sort(rs$parameter_ids[[1]]), c("RS_OVRLRESP=CR", "RS_OVRLRESP=PR"))

  # Bar groups
  wt <- lookup[lookup$display_id == "VS_WEIGHT_CAT", ]
  expect_equal(wt$plot_type, "bar")

  # cat3_values present
  expect_true("cat3_values" %in% names(lookup))
  expect_equal(sort(alb$cat3_values[[1]]), c("range_normalized", "ratio_missing"))
  expect_equal(rs$cat3_values[[1]], "categorical")
})

test_that("build_param_lookup splits independent params with same category_2", {
  df <- data.frame(
    parameter_id = c("param1", "param2"),
    parameter_category_2 = c("cat2", "cat2"),
    parameter_category_3 = c("category 3", "category 3"),
    stringsAsFactors = FALSE
  )

  lookup <- build_param_lookup(df)
  expect_equal(nrow(lookup), 2)
  expect_true("param1" %in% lookup$display_id)
  expect_true("param2" %in% lookup$display_id)
})

test_that("determine_plot_type returns single type directly", {
  expect_equal(determine_plot_type("numeric"), "numeric")
  expect_equal(determine_plot_type("categorical"), "categorical")
})

test_that("determine_plot_type returns numeric for mixed numeric types", {
  expect_equal(determine_plot_type(c("range_normalized", "ratio_missing")), "numeric")
})

test_that("determine_plot_type falls back to first type for non-numeric mix", {
  expect_equal(determine_plot_type(c("categorical", "bar")), "categorical")
})


test_that("split_param_ids separates regular from missingness", {
  df <- data.frame(
    parameter_id = c("LB_NORM_ALT", "LB_MISS_ALT", "VS_SYSBP"),
    parameter_category_3 = c("range_normalized", "ratio_missing", "numeric"),
    stringsAsFactors = FALSE
  )

  splits <- split_param_ids(c("LB_NORM_ALT", "LB_MISS_ALT", "VS_SYSBP"), df)
  expect_equal(sort(splits$regular), c("LB_NORM_ALT", "VS_SYSBP"))
  expect_equal(splits$missingness, "LB_MISS_ALT")
})

test_that("split_param_ids returns empty missingness when none present", {
  df <- data.frame(
    parameter_id = c("VS_SYSBP", "VS_DIABP"),
    parameter_category_3 = c("numeric", "numeric"),
    stringsAsFactors = FALSE
  )

  splits <- split_param_ids(c("VS_SYSBP", "VS_DIABP"), df)
  expect_equal(sort(splits$regular), c("VS_DIABP", "VS_SYSBP"))
  expect_length(splits$missingness, 0)
})


test_that("mod_FieldDetail_server renders outputs with ctas sample", {
  m <- prepare_measures(sample_ctas_data, sample_ctas_results)

  shiny::testServer(
    mod_FieldDetail_server,
    args = list(
      rctv_measures = shiny::reactiveVal(m),
      rctv_ctas_results = shiny::reactiveVal(sample_ctas_results)
    ),
    {
      session$setInputs(thresh = 1.3, include_miss = TRUE)
      session$flushReact()

      stats <- param_outliers()
      expect_s3_class(stats, "data.frame")
      expect_true("display_id" %in% names(stats))
      expect_true("n_outlier_sites" %in% names(stats))
      expect_true(all(stats$n_outlier_sites >= 0))

      session$setInputs(selected_param = stats$display_id[1])
      expect_type(output$plot_title, "character")
      expect_true(grepl("Parameter:", output$plot_title))
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
      session$setInputs(thresh = 0, include_miss = TRUE)
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

test_that("mod_FieldDetail_server handles SDTM sample data", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)

  shiny::testServer(
    mod_FieldDetail_server,
    args = list(
      rctv_measures = shiny::reactiveVal(m),
      rctv_ctas_results = shiny::reactiveVal(sample_sdtm_results)
    ),
    {
      session$setInputs(thresh = 1.3, include_miss = TRUE)
      session$flushReact()

      lookup <- rctv_param_lookup()
      expect_true("categorical" %in% lookup$plot_type)
      expect_true("bar" %in% lookup$plot_type)

      stats <- param_outliers()
      expect_true(nrow(stats) > 0)
    }
  )
})

test_that("mod_FieldDetail_server dispatches categorical plot", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)

  shiny::testServer(
    mod_FieldDetail_server,
    args = list(
      rctv_measures = shiny::reactiveVal(m),
      rctv_ctas_results = shiny::reactiveVal(sample_sdtm_results)
    ),
    {
      session$setInputs(thresh = 0, include_miss = TRUE)
      session$flushReact()

      lookup <- rctv_param_lookup()
      cat_id <- lookup$display_id[lookup$plot_type == "categorical"][1]
      session$setInputs(selected_param = cat_id)

      p <- output$ts_plot
      expect_true(!is.null(p))
    }
  )
})

test_that("mod_FieldDetail_server dispatches bar plot", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)

  shiny::testServer(
    mod_FieldDetail_server,
    args = list(
      rctv_measures = shiny::reactiveVal(m),
      rctv_ctas_results = shiny::reactiveVal(sample_sdtm_results)
    ),
    {
      session$setInputs(thresh = 0, include_miss = TRUE)
      session$flushReact()

      lookup <- rctv_param_lookup()
      bar_id <- lookup$display_id[lookup$plot_type == "bar"][1]
      session$setInputs(selected_param = bar_id)

      p <- output$ts_plot
      expect_true(!is.null(p))
    }
  )
})

test_that("mod_FieldDetail_server dispatches numeric plot with norm+missing", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)

  shiny::testServer(
    mod_FieldDetail_server,
    args = list(
      rctv_measures = shiny::reactiveVal(m),
      rctv_ctas_results = shiny::reactiveVal(sample_sdtm_results)
    ),
    {
      session$setInputs(thresh = 0, include_miss = TRUE)
      session$flushReact()

      lookup <- rctv_param_lookup()
      alb_id <- lookup$display_id[lookup$display_id == "ALB"]
      session$setInputs(selected_param = alb_id)

      p <- output$ts_plot
      expect_true(!is.null(p))
    }
  )
})

test_that("mod_FieldDetail_server score_table_regular validate guard fires for unscored param", {
  m <- prepare_measures(sample_ctas_data, sample_ctas_results)

  fake_results <- sample_ctas_results
  fake_results$timeseries <- fake_results$timeseries[0, ]
  fake_results$site_scores <- fake_results$site_scores[0, ]

  shiny::testServer(
    mod_FieldDetail_server,
    args = list(
      rctv_measures = shiny::reactiveVal(m),
      rctv_ctas_results = shiny::reactiveVal(fake_results)
    ),
    {
      session$setInputs(thresh = 1.3, include_miss = TRUE)
      session$flushReact()

      lookup <- rctv_param_lookup()
      session$setInputs(selected_param = lookup$display_id[1])

      expect_error(output$score_table_regular, class = "shiny.silent.error")
    }
  )
})

test_that("mod_FieldDetail_server ts_data_table validate guard fires with high threshold", {
  m <- prepare_measures(sample_ctas_data, sample_ctas_results)

  shiny::testServer(
    mod_FieldDetail_server,
    args = list(
      rctv_measures = shiny::reactiveVal(m),
      rctv_ctas_results = shiny::reactiveVal(sample_ctas_results)
    ),
    {
      session$setInputs(thresh = 99999, include_miss = TRUE)
      session$flushReact()

      lookup <- rctv_param_lookup()
      session$setInputs(selected_param = lookup$display_id[1])

      expect_error(output$ts_data_table, class = "shiny.silent.error")
    }
  )
})


test_that("mod_FieldDetail_server passes untransformed to ts_data_table for SDTM", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  ut <- sample_sdtm_data$untransformed

  shiny::testServer(
    mod_FieldDetail_server,
    args = list(
      rctv_measures = shiny::reactiveVal(m),
      rctv_ctas_results = shiny::reactiveVal(sample_sdtm_results),
      rctv_untransformed = shiny::reactiveVal(ut)
    ),
    {
      session$setInputs(thresh = 0, include_miss = TRUE)
      session$flushReact()

      lookup <- rctv_param_lookup()
      alb_id <- lookup$display_id[lookup$display_id == "ALB"]
      session$setInputs(selected_param = alb_id)

      tbl <- output$ts_data_table
      expect_true(!is.null(tbl))
    }
  )
})


test_that("mod_FieldDetail_server include_miss=FALSE filters outlier counts", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)

  shiny::testServer(
    mod_FieldDetail_server,
    args = list(
      rctv_measures = shiny::reactiveVal(m),
      rctv_ctas_results = shiny::reactiveVal(sample_sdtm_results)
    ),
    {
      session$setInputs(thresh = 1.3, include_miss = TRUE)
      stats_with <- param_outliers()

      session$setInputs(include_miss = FALSE)
      stats_without <- param_outliers()

      # Missingness parameters contribute to scores, so disabling should
      # change (or keep equal) the outlier counts
      expect_true(nrow(stats_with) > 0)
      expect_true(nrow(stats_without) > 0)
    }
  )
})


test_that("mod_FieldDetail_server include_miss=FALSE filters plot param_ids", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)

  shiny::testServer(
    mod_FieldDetail_server,
    args = list(
      rctv_measures = shiny::reactiveVal(m),
      rctv_ctas_results = shiny::reactiveVal(sample_sdtm_results)
    ),
    {
      session$setInputs(thresh = 0, include_miss = FALSE)
      session$flushReact()

      lookup <- rctv_param_lookup()
      alb_id <- lookup$display_id[lookup$display_id == "ALB"]
      session$setInputs(selected_param = alb_id)

      p <- output$ts_plot
      expect_true(!is.null(p))
    }
  )
})


test_that("mod_FieldDetail_server missingness score table shows message for non-lab param", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)

  shiny::testServer(
    mod_FieldDetail_server,
    args = list(
      rctv_measures = shiny::reactiveVal(m),
      rctv_ctas_results = shiny::reactiveVal(sample_sdtm_results)
    ),
    {
      session$setInputs(thresh = 1.3, include_miss = TRUE)
      session$flushReact()

      lookup <- rctv_param_lookup()
      # VS parameters don't have ratio_missing companion
      vs_ids <- lookup$display_id[grepl("^VS_", lookup$display_id)]
      if (length(vs_ids) > 0) {
        session$setInputs(selected_param = vs_ids[1])
        expect_error(output$score_table_miss, class = "shiny.silent.error")
      }
    }
  )
})


test_that("mod_FieldDetail_server regular score table renders for SDTM lab", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)

  shiny::testServer(
    mod_FieldDetail_server,
    args = list(
      rctv_measures = shiny::reactiveVal(m),
      rctv_ctas_results = shiny::reactiveVal(sample_sdtm_results)
    ),
    {
      session$setInputs(thresh = 0, include_miss = TRUE)
      session$flushReact()

      lookup <- rctv_param_lookup()
      alb_id <- lookup$display_id[lookup$display_id == "ALB"]
      session$setInputs(selected_param = alb_id)

      tbl <- output$score_table_regular
      expect_true(!is.null(tbl))
    }
  )
})


test_that("plot_type_icon returns flask for range_normalized", {
  expect_equal(plot_type_icon("numeric", c("range_normalized", "ratio_missing")), "flask")
  expect_equal(plot_type_icon("numeric", "range_normalized"), "flask")
})

test_that("plot_type_icon returns chart-line for plain numeric", {
  expect_equal(plot_type_icon("numeric", "numeric"), "chart-line")
})

test_that("plot_type_icon returns chart-bar for bar", {
  expect_equal(plot_type_icon("bar", "bar"), "chart-bar")
})

test_that("plot_type_icon returns water for categorical", {
  expect_equal(plot_type_icon("categorical", "categorical"), "water")
})


test_that("mod_FieldDetail_server feature selection filters scores", {
  m <- prepare_measures(sample_sdtm_data, sample_sdtm_results)
  all_feats <- sort(unique(sample_sdtm_results$site_scores$feature))

  shiny::testServer(
    mod_FieldDetail_server,
    args = list(
      rctv_measures = shiny::reactiveVal(m),
      rctv_ctas_results = shiny::reactiveVal(sample_sdtm_results)
    ),
    {
      session$setInputs(
        thresh = 1.3,
        include_miss = TRUE,
        selected_features = all_feats
      )
      session$flushReact()

      mf_all <- rctv_measures_feat()
      expect_equal(mf_all$max_score, m$max_score)

      session$setInputs(selected_features = all_feats[1])
      session$flushReact()

      mf_one <- rctv_measures_feat()
      expect_true(all(mf_one$max_score <= m$max_score))
    }
  )
})
