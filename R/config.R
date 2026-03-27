# Package-level environment for runtime configuration (mutable state)
.cfg_env <- new.env(parent = emptyenv())
.cfg_env$query_no_change <- "#a380e9"
.cfg_env$query_data_change <- "#2790e0"
.cfg_env$default_features <- NULL
.cfg_env$score_breaks <- NULL
.cfg_env$score_colors_plot <- NULL
.cfg_env$score_colors_table <- NULL
.cfg_env$score_colors_table_text <- NULL
.cfg_env$icons <- NULL


#' Get built-in default configuration
#'
#' Returns the hardcoded defaults that match the shipped `inst/config.yml`.
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
    )
  )
}


#' Load app configuration from a YAML file
#'
#' Reads a YAML config file and merges with built-in defaults so that
#' missing keys fall back gracefully.
#'
#' @param path Path to a YAML file. When `NULL` (default), uses the
#'   config shipped with the package (`inst/config.yml`).
#'
#' @return A nested list with elements `colors` and `features`.
#' @export
load_config <- function(path = NULL) {
  defaults <- default_config()

  if (is.null(path)) {
    path <- system.file("config.yml", package = "ctasapp")
    if (path == "") return(defaults) # nocov
  }

  if (!file.exists(path)) {
    warning("Config file not found: ", path, ". Using defaults.")
    return(defaults)
  }

  user <- yaml::read_yaml(path)

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

  cfg
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

  invisible(cfg)
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
