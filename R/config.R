# Package-level environment for runtime configuration (mutable state)
.cfg_env <- new.env(parent = emptyenv())

#' Conditional app logger
#'
#' Prints a message to the console when the `ctasapp.verbose` option is `TRUE`
#' (the default). Set `options(ctasapp.verbose = FALSE)` or pass
#' `verbose = FALSE` to [run_ctas_app()] to silence all log output.
#'
#' @param ... Passed to [base::message()].
#' @keywords internal
ctas_log <- function(...) {
  if (isTRUE(getOption("ctasapp.verbose", TRUE))) {
    message("[ctasapp] ", ...)
  }
}
.cfg_env$query_no_change <- "#a380e9"
.cfg_env$query_data_change <- "#2790e0"
.cfg_env$default_features <- NULL
.cfg_env$score_breaks <- NULL
.cfg_env$score_colors_plot <- NULL
.cfg_env$score_colors_table <- NULL
.cfg_env$score_colors_table_text <- NULL
.cfg_env$icons <- NULL
.cfg_env$embedded_paths <- NULL


#' Get built-in default configuration
#'
#' Returns the hardcoded defaults that match the shipped `inst/ctasapp.yml`.
#' Used as fallback when keys are missing from a user-supplied config.
#'
#' @return A nested list.
#' @keywords internal
default_config <- function() {
  list(
    colors = list(
      score_breaks = c(1.3, 3, 5, 10),
      plot = c("#9ED782", "#fed8019c", "#fed801", "#FEAA01", "#FF5858"),
      table = c("#FFFFFF", "#feed01", "#fed801", "#FEAA01", "#FF5858"),
      table_text = c("#1A1A1A", "#1A1A1A", "#1A1A1A", "#FFFFFF", "#FFFFFF"),
      query_no_change = "#a380e9",
      query_data_change = "#2790e0"
    ),
    icons = list(
      range_normalized = "flask",
      numeric = "chart-line",
      categorical = "water",
      bar = "chart-bar"
    ),
    features = list(
      default = c("autocorr", "average", "sd",
                   "unique_value_count_relative", "lof", "range")
    ),
    embedded = list(
      results = NULL,
      input = NULL,
      untransformed = NULL,
      queries = NULL,
      pd = NULL
    )
  )
}


#' Load app configuration from a YAML file
#'
#' Reads a YAML config file and merges with built-in defaults so that
#' missing keys fall back gracefully. When `path` is `NULL` the file is
#' resolved using a discovery chain:
#' \enumerate{
#'   \item `ctasapp.yml` in the current working directory (i.e. the
#'     folder containing `app.R` when launched by Posit Connect or
#'     `shiny::runApp()`).
#'   \item `system.file("ctasapp.yml", package = "ctasapp")` (the copy
#'     shipped with the installed package).
#' }
#' If neither is found, the built-in [default_config()] is returned.
#'
#' @param path Path to a YAML file. When `NULL` (default), the discovery
#'   chain above is used.
#'
#' @return A nested list with elements `colors`, `icons`, `features`,
#'   and `embedded`.
#' @export
load_config <- function(path = NULL) {
  defaults <- default_config()

  if (is.null(path)) {
    cwd_path <- file.path(getwd(), "ctasapp.yml")
    if (file.exists(cwd_path)) {
      ctas_log("Loading config from working directory: ", cwd_path)
      path <- cwd_path
    } else {
      pkg_path <- system.file("ctasapp.yml", package = "ctasapp")
      if (nzchar(pkg_path)) {
        ctas_log("Loading config from installed package: ", pkg_path)
        path <- pkg_path
      } else {
        ctas_log("No ctasapp.yml found; using built-in defaults") # nocov
        return(defaults) # nocov
      }
    }
  } else {
    ctas_log("Loading config from explicit path: ", path)
  }

  if (!file.exists(path)) {
    warning("Config file not found: ", path, ". Using defaults.")
    return(defaults)
  }

  user <- yaml::read_yaml(path)
  cfg_dir <- dirname(normalizePath(path, mustWork = TRUE))

  cfg <- defaults
  if (!is.null(user$colors)) {
    uc <- user$colors
    if (!is.null(uc$score_breaks)) cfg$colors$score_breaks <- as.numeric(uc$score_breaks)
    if (!is.null(uc$plot))         cfg$colors$plot <- as.character(uc$plot)
    if (!is.null(uc$table))        cfg$colors$table <- as.character(uc$table)
    if (!is.null(uc$table_text))   cfg$colors$table_text <- as.character(uc$table_text)
    if (!is.null(uc$query_no_change))   cfg$colors$query_no_change <- uc$query_no_change
    if (!is.null(uc$query_data_change)) cfg$colors$query_data_change <- uc$query_data_change
  }
  if (!is.null(user$icons)) {
    for (nm in names(user$icons)) {
      cfg$icons[[nm]] <- as.character(user$icons[[nm]])
    }
  }
  if (!is.null(user$features) && !is.null(user$features$default)) {
    cfg$features$default <- as.character(user$features$default)
  }
  if (!is.null(user$embedded)) {
    for (key in c("results", "input", "untransformed", "queries", "pd")) {
      val <- user$embedded[[key]]
      if (!is.null(val) && nzchar(val)) {
        cfg$embedded[[key]] <- resolve_config_path(as.character(val), cfg_dir)
      }
    }
  }

  cfg
}


#' Resolve a path from a config file
#'
#' Expands `~`, then resolves relative paths against `base_dir` (the
#' directory containing the active config file). Absolute paths are
#' returned as-is (after expansion).
#'
#' @param path Character scalar path from the config file.
#' @param base_dir Directory of the active config file.
#' @return Character scalar resolved path.
#' @keywords internal
resolve_config_path <- function(path, base_dir) {
  path <- path.expand(path)
  if (!nzchar(path)) return(path) # nocov
  if (substr(path, 1, 1) == "/" || grepl("^[A-Za-z]:[/\\\\]", path)) {
    return(path)
  }
  file.path(base_dir, path)
}


#' Apply configuration to package runtime state
#'
#' Overwrites the package-level colour constants and stores query colours
#' and default features in the internal config environment.
#'
#' @param cfg A configuration list as returned by [load_config()].
#' @export
apply_config <- function(cfg) {
  cc <- cfg[["colors"]]

  .cfg_env$score_breaks <- cc[["score_breaks"]]
  .cfg_env$score_colors_plot <- cc[["plot"]]

  tbl <- if (is.null(cc[["table"]])) cc[["plot"]] else cc[["table"]]
  .cfg_env$score_colors_table <- tbl
  .cfg_env$score_colors_table_text <- cc[["table_text"]]

  .cfg_env$query_no_change <- cc[["query_no_change"]]
  .cfg_env$query_data_change <- cc[["query_data_change"]]
  .cfg_env$icons <- cfg[["icons"]]
  .cfg_env$default_features <- cfg[["features"]][["default"]]
  .cfg_env$embedded_paths <- cfg[["embedded"]]

  invisible(cfg)
}


#' Get embedded dataset file paths from configuration
#'
#' Returns the list of resolved file paths configured in the `embedded:`
#' block of `ctasapp.yml`, or `NULL` when the feature is not configured
#' (i.e. neither `results` nor `input` was set).
#'
#' @return A named list with elements `results`, `input`, `untransformed`,
#'   `queries`, `pd` (each a character path or `NULL`), or `NULL` if
#'   disabled.
#' @export
get_embedded_paths <- function() {
  paths <- .cfg_env$embedded_paths
  if (is.null(paths)) return(NULL)
  if (is.null(paths$results) && is.null(paths$input)) return(NULL)
  paths
}


#' Check whether the embedded dataset feature is configured
#'
#' Returns `TRUE` when [get_embedded_paths()] is non-`NULL`, i.e. the
#' operator has set the `embedded:` block in `ctasapp.yml`. File
#' existence on disk is not checked here; missing files are surfaced
#' as errors when the user clicks Load.
#'
#' @return Logical scalar.
#' @export
embedded_files_configured <- function() {
  !is.null(get_embedded_paths())
}


#' Get query dot colours from configuration
#'
#' @return A named list with `no_change` and `data_change` colour strings.
#' @export
get_query_colors <- function() {
  list(
    no_change = .cfg_env$query_no_change,
    data_change = .cfg_env$query_data_change
  )
}


#' Get default features from configuration
#'
#' @return Character vector of default feature names, or NULL if all
#'   features should be selected.
#' @export
get_default_features <- function() {
  .cfg_env$default_features
}


#' Get score breaks from configuration
#'
#' Returns the configured score breaks, falling back to the package
#' constants when [apply_config()] has not been called.
#'
#' @return Numeric vector of break points.
#' @export
get_score_breaks <- function() {
  .cfg_env$score_breaks %||% SCORE_BREAKS
}


#' Get score colours for plots from configuration
#'
#' @return Character vector of hex colour strings.
#' @export
get_score_colors_plot <- function() {
  .cfg_env$score_colors_plot %||% SCORE_COLORS_PLOT
}


#' Get score colours for tables from configuration
#'
#' @return Character vector of hex colour strings.
#' @export
get_score_colors_table <- function() {
  .cfg_env$score_colors_table %||% SCORE_COLORS_TABLE
}


#' Get score text colours for tables from configuration
#'
#' @return Character vector of hex colour strings.
#' @export
get_score_colors_table_text <- function() {
  .cfg_env$score_colors_table_text %||% SCORE_COLORS_TABLE_TEXT
}


#' Get parameter type icon mapping from configuration
#'
#' Returns a named list mapping parameter types to Font Awesome icon names.
#'
#' @return A named list (e.g. `list(range_normalized = "flask", ...)`),
#'   or `NULL` if [apply_config()] has not been called.
#' @export
get_param_icons <- function() {
  .cfg_env$icons
}
