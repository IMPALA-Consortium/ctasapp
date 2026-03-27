reset_config <- function() {
  apply_config(default_config())
}


# -- default_config() ---------------------------------------------------------

test_that("default_config returns correct structure", {
  cfg <- default_config()
  expect_type(cfg, "list")
  expect_named(cfg, c("colors", "icons", "features"))
  expect_named(cfg$colors, c("score_breaks", "plot", "table", "table_text",
                              "query_no_change", "query_data_change"))
  expect_named(cfg$icons, c("range_normalized", "numeric", "categorical", "bar"))
  expect_length(cfg$colors$table, 5)
  expect_length(cfg$colors$plot, 5)
  expect_length(cfg$colors$score_breaks, 4)
  expect_equal(cfg$features$default,
               c("autocorr", "average", "sd",
                 "unique_value_count_relative", "lof", "range"))
})


# -- load_config() -------------------------------------------------------------

test_that("load_config with NULL path returns defaults when inst file missing", {
  cfg <- load_config(NULL)
  expect_type(cfg, "list")
  expect_named(cfg, c("colors", "icons", "features"))
})


test_that("load_config warns and returns defaults for non-existent file", {
  expect_warning(
    cfg <- load_config("/nonexistent/config.yml"),
    "Config file not found"
  )
  expect_equal(cfg, default_config())
})

test_that("load_config reads a valid YAML file", {
  tmp <- tempfile(fileext = ".yml")
  writeLines(c(
    "colors:",
    "  score_breaks: [1, 2, 3, 4]",
    "  plot: ['#111111', '#222222', '#333333', '#444444', '#555555']",
    "  query_no_change: '#AAAAAA'",
    "  query_data_change: '#BBBBBB'",
    "features:",
    "  default:",
    "    - autocorr",
    "    - sd"
  ), tmp)

  cfg <- load_config(tmp)

  expect_equal(cfg$colors$score_breaks, c(1, 2, 3, 4))
  expect_equal(cfg$colors$plot, c("#111111", "#222222", "#333333",
                                  "#444444", "#555555"))
  expect_equal(cfg$colors$table, default_config()$colors$table)
  expect_equal(cfg$colors$query_no_change, "#AAAAAA")
  expect_equal(cfg$colors$query_data_change, "#BBBBBB")
  expect_equal(cfg$features$default, c("autocorr", "sd"))
  unlink(tmp)
})

test_that("load_config merges partial overrides with defaults", {
  tmp <- tempfile(fileext = ".yml")
  writeLines(c(
    "colors:",
    "  query_no_change: '#FFFFFF'"
  ), tmp)

  cfg <- load_config(tmp)

  expect_equal(cfg$colors$query_no_change, "#FFFFFF")
  expect_equal(cfg$colors$score_breaks, default_config()$colors$score_breaks)
  expect_equal(cfg$colors$plot, default_config()$colors$plot)
  expect_equal(cfg$features$default, default_config()$features$default)
  unlink(tmp)
})

test_that("load_config handles table override", {
  tmp <- tempfile(fileext = ".yml")
  writeLines(c(
    "colors:",
    "  table: ['#A1', '#A2', '#A3', '#A4', '#A5']"
  ), tmp)

  cfg <- load_config(tmp)
  expect_equal(cfg$colors$table, c("#A1", "#A2", "#A3", "#A4", "#A5"))
  unlink(tmp)
})


# -- apply_config() ------------------------------------------------------------

test_that("apply_config updates getter values", {
  on.exit(reset_config(), add = TRUE)

  cfg <- default_config()
  cfg$colors$score_breaks <- c(2, 4, 6, 8)
  cfg$colors$plot <- c("#AA0000", "#BB0000", "#CC0000", "#DD0000", "#EE0000")
  cfg$colors$table <- c("#A10000", "#B10000", "#C10000", "#D10000", "#E10000")
  cfg$colors$table_text <- c("#000001", "#000002", "#000003", "#000004", "#000005")
  cfg$colors$query_no_change <- "#CUSTOM1"
  cfg$colors$query_data_change <- "#CUSTOM2"
  cfg$features$default <- c("lof", "sd")

  apply_config(cfg)

  expect_equal(get_score_breaks(), c(2, 4, 6, 8))
  expect_equal(get_score_colors_plot(), c("#AA0000", "#BB0000", "#CC0000",
                                          "#DD0000", "#EE0000"))
  expect_equal(get_score_colors_table(), c("#A10000", "#B10000", "#C10000",
                                           "#D10000", "#E10000"))
  expect_equal(get_score_colors_table_text(), c("#000001", "#000002", "#000003",
                                                "#000004", "#000005"))
})

test_that("apply_config uses plot colors for table when table is NULL", {
  on.exit(reset_config(), add = TRUE)

  cfg <- default_config()
  cfg$colors$table <- NULL
  cfg$colors$plot <- c("#111111", "#222222", "#333333", "#444444", "#555555")

  apply_config(cfg)

  expect_equal(get_score_colors_table(), c("#111111", "#222222", "#333333",
                                           "#444444", "#555555"))
})

test_that("apply_config returns config invisibly", {
  on.exit(reset_config(), add = TRUE)

  cfg <- default_config()
  result <- apply_config(cfg)
  expect_equal(result, cfg)
})


# -- get_query_colors() --------------------------------------------------------

test_that("get_query_colors returns defaults", {
  on.exit(reset_config(), add = TRUE)

  cfg <- default_config()
  apply_config(cfg)

  qc <- get_query_colors()
  expect_named(qc, c("no_change", "data_change"))
  expect_equal(qc$no_change, "#a380e9")
  expect_equal(qc$data_change, "#2790e0")
})

test_that("get_query_colors reflects apply_config changes", {
  on.exit(reset_config(), add = TRUE)

  cfg <- default_config()
  cfg$colors$query_no_change <- "#AABBCC"
  cfg$colors$query_data_change <- "#DDEEFF"
  apply_config(cfg)

  qc <- get_query_colors()
  expect_equal(qc$no_change, "#AABBCC")
  expect_equal(qc$data_change, "#DDEEFF")
})


# -- get_default_features() ----------------------------------------------------

test_that("get_default_features returns NULL when not configured", {
  on.exit(reset_config(), add = TRUE)

  .cfg_env$default_features <- NULL
  expect_null(get_default_features())
})

test_that("get_default_features returns config value after apply_config", {
  on.exit(reset_config(), add = TRUE)

  cfg <- default_config()
  cfg$features$default <- c("autocorr", "range")
  apply_config(cfg)

  expect_equal(get_default_features(), c("autocorr", "range"))
})


# -- get_score_* fallback to constants -----------------------------------------

test_that("get_score_breaks falls back to SCORE_BREAKS when not configured", {
  on.exit(reset_config(), add = TRUE)

  .cfg_env$score_breaks <- NULL
  expect_equal(get_score_breaks(), SCORE_BREAKS)
})

test_that("get_score_colors_plot falls back to SCORE_COLORS_PLOT", {
  on.exit(reset_config(), add = TRUE)

  .cfg_env$score_colors_plot <- NULL
  expect_equal(get_score_colors_plot(), SCORE_COLORS_PLOT)
})

test_that("get_score_colors_table falls back to SCORE_COLORS_TABLE", {
  on.exit(reset_config(), add = TRUE)

  .cfg_env$score_colors_table <- NULL
  expect_equal(get_score_colors_table(), SCORE_COLORS_TABLE)
})

test_that("get_score_colors_table_text falls back to SCORE_COLORS_TABLE_TEXT", {
  on.exit(reset_config(), add = TRUE)

  .cfg_env$score_colors_table_text <- NULL
  expect_equal(get_score_colors_table_text(), SCORE_COLORS_TABLE_TEXT)
})


# -- get_param_icons() ---------------------------------------------------------

test_that("get_param_icons returns NULL when not configured", {
  on.exit(reset_config(), add = TRUE)

  .cfg_env$icons <- NULL
  expect_null(get_param_icons())
})

test_that("get_param_icons returns default icons after apply_config", {
  on.exit(reset_config(), add = TRUE)

  apply_config(default_config())

  icons <- get_param_icons()
  expect_type(icons, "list")
  expect_equal(icons$range_normalized, "flask")
  expect_equal(icons$numeric, "chart-line")
  expect_equal(icons$categorical, "water")
  expect_equal(icons$bar, "chart-bar")
})

test_that("apply_config stores custom icons", {
  on.exit(reset_config(), add = TRUE)

  cfg <- default_config()
  cfg$icons$range_normalized <- "vial"
  cfg$icons$numeric <- "wave-square"
  apply_config(cfg)

  icons <- get_param_icons()
  expect_equal(icons$range_normalized, "vial")
  expect_equal(icons$numeric, "wave-square")
  expect_equal(icons$categorical, "water")
})

test_that("load_config merges icon overrides", {
  tmp <- tempfile(fileext = ".yml")
  writeLines(c(
    "icons:",
    "  bar: chart-column",
    "  numeric: signal"
  ), tmp)

  cfg <- load_config(tmp)
  expect_equal(cfg$icons$bar, "chart-column")
  expect_equal(cfg$icons$numeric, "signal")
  expect_equal(cfg$icons$range_normalized, "flask")
  expect_equal(cfg$icons$categorical, "water")
  unlink(tmp)
})
