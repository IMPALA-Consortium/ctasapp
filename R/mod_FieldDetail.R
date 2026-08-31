#' Field Detail Module - UI
#'
#' Sidebar with threshold slider, missingness toggle, feature selector, and
#' clickable parameter list with datatype icons. Main content flows like a
#' webpage: pill-tabbed score tables, timeseries plot, then data table.
#'
#' @param id Module namespace ID.
#' @export
mod_FieldDetail_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      title = NULL,
      width = 250,
      shiny::uiOutput(ns("study_selector")),
      shiny::sliderInput(
        ns("thresh"),
        "Score Threshold",
        min = 0, max = 10, value = 1.3, step = 0.1
      ),
      shiny::checkboxInput(ns("include_miss"), "Include Missingness", value = TRUE),
      bslib::accordion(
        bslib::accordion_panel(
          "ctas Features",
          shiny::checkboxGroupInput(ns("selected_features"), label = NULL,
                                    choices = NULL, selected = NULL)
        ),
        open = FALSE
      ),
      shiny::hr(),
      shiny::textInput(ns("param_filter"), label = NULL,
                       placeholder = "Filter fields..."),
      shiny::selectInput(ns("param_sort"), label = NULL,
                         choices = c("Outliers" = "outliers",
                                     "A-Z" = "alpha"),
                         selected = "outliers"),
      shiny::tags$div(
        style = "max-height:60vh;overflow-y:auto;",
        shiny::uiOutput(ns("param_list"))
      )
    ),
    shiny::tags$div(
      style = "height:calc(100vh - 56px);overflow-y:auto;overflow-x:hidden;padding-right:4px;",
      shiny::fluidRow(
        shiny::column(
          5,
          shiny::h4(shiny::textOutput(ns("plot_title"))),
          shiny::h5(shiny::textOutput(ns("plot_subtitle")))
        ),
        shiny::column(
          3,
          shiny::radioButtons(
            ns("linkage_mode"),
            label = "Link plot & source data to:",
            choices = c("Regular Scores" = "regular",
                        "Missingness Scores" = "missingness",
                        "None" = "none"),
            selected = "regular",
            inline = FALSE
          )
        ),
        shiny::column(
          4,
          shiny::uiOutput(ns("field_notice"))
        )
      ),
      bslib::navset_pill(
        id = ns("score_tab"),
        bslib::nav_panel(
          "Regular Scores",
          shiny::tags$div(
            style = "min-height:380px;",
            DT::dataTableOutput(ns("score_table_regular"))
          )
        ),
        bslib::nav_panel(
          "Missingness Scores",
          shiny::tags$div(
            style = "min-height:380px;",
            DT::dataTableOutput(ns("score_table_miss"))
          )
        )
      ),
      shiny::hr(),
      shiny::fluidRow(
        shiny::column(10, shiny::plotOutput(ns("ts_plot"), height = "700px")),
        shiny::column(
          2,
          style = "max-height:700px;overflow-y:auto;",
          shiny::uiOutput(ns("visit_sorter"))
        )
      ),
      shiny::hr(),
      shiny::h5("Data Tables (Outlier Sites)"),
      bslib::navset_pill(
        id = ns("data_tab"),
        selected = "Queries",
        bslib::nav_panel("Queries", DT::dataTableOutput(ns("query_table"))),
        bslib::nav_panel("Source Data", DT::dataTableOutput(ns("ts_data_table")))
      )
    )
  )
}

#' Build display parameter lookup from measures
#'
#' Groups parameters by `parameter_category_2` (the "field" key). This
#' naturally groups norm + missing-ratio labs under a single entry, and
#' categorical one-hot levels under their shared prefix. The `plot_type`
#' is determined by the dominant `parameter_category_3`; mixed types
#' (e.g. `range_normalized` + `ratio_missing`) are treated as `"numeric"`.
#'
#' When multiple parameters share a `parameter_category_2` but have
#' identical `parameter_category_3` values that aren't meant to be grouped
#' (e.g. two independent numeric params with the same generic category),
#' they are kept separate using `parameter_id` as the display key.
#'
#' Now also includes a `cat3_values` list column with raw `parameter_category_3`
#' values for icon determination.
#'
#' @param df Measures data frame.
#' @return A data frame with `display_id`, `parameter_ids` (list column),
#'   `plot_type`, and `cat3_values` (list column).
#' @keywords internal
build_param_lookup <- function(df) {
  param_meta <- df |>
    dplyr::distinct(.data$parameter_id, .data$parameter_category_2,
                    .data$parameter_category_3)

  by_cat2 <- param_meta |>
    dplyr::summarise(
      parameter_ids = list(.data$parameter_id),
      plot_type = determine_plot_type(.data$parameter_category_3),
      cat3_values = list(unique(.data$parameter_category_3)),
      n_cat3 = dplyr::n_distinct(.data$parameter_category_3),
      .by = "parameter_category_2"
    )

  groupable_types <- c("categorical", "bar", "range_normalized", "ratio_missing")

  needs_split <- by_cat2$n_cat3 == 1 &
    !by_cat2$plot_type %in% groupable_types &
    lengths(by_cat2$parameter_ids) > 1

  keep <- by_cat2[!needs_split, ]
  split_rows <- by_cat2[needs_split, ]

  if (nrow(split_rows) > 0) {
    expanded <- param_meta |>
      dplyr::filter(.data$parameter_category_2 %in% split_rows$parameter_category_2) |>
      dplyr::summarise(
        parameter_ids = list(.data$parameter_id),
        plot_type = dplyr::first(.data$parameter_category_3),
        cat3_values = list(unique(.data$parameter_category_3)),
        n_cat3 = 1L,
        .by = "parameter_id"
      ) |>
      dplyr::rename(parameter_category_2 = "parameter_id")

    keep <- dplyr::bind_rows(keep, expanded)
  }

  keep |>
    dplyr::select("parameter_category_2", "parameter_ids", "plot_type",
                   "cat3_values") |>
    dplyr::rename(display_id = "parameter_category_2")
}


#' Determine plot type from a vector of category_3 values
#'
#' Mixed numeric types (range_normalized + ratio_missing) map to "numeric".
#' Single types pass through.
#'
#' @param cat3 Character vector of parameter_category_3 values.
#' @return Character scalar plot type.
#' @keywords internal
determine_plot_type <- function(cat3) {
  types <- unique(cat3)
  if (length(types) == 1) return(types)
  numeric_types <- c("numeric", "range_normalized", "ratio_missing")
  if (all(types %in% numeric_types)) return("numeric")
  types[1]
}


#' Map plot type to a Font Awesome icon name
#'
#' Uses raw `parameter_category_3` values to distinguish range-normalized
#' labs (flask) from plain numeric timeseries (chart-line). When a config
#' has been applied via [apply_config()], the icon mapping is read from
#' [get_param_icons()].
#'
#' @param plot_type Character scalar from [build_param_lookup()].
#' @param cat3_values Character vector of raw `parameter_category_3` values.
#' @return Character scalar Font Awesome icon name.
#' @keywords internal
plot_type_icon <- function(plot_type, cat3_values) {
  icons <- get_param_icons()
  if (!is.null(icons)) {
    if (any(cat3_values == "range_normalized") &&
        !is.null(icons[["range_normalized"]])) {
      return(icons[["range_normalized"]])
    }
    icon <- icons[[plot_type]]
    if (!is.null(icon)) return(icon)
  }
  if (any(cat3_values == "range_normalized")) return("flask")
  switch(plot_type,
    categorical = "water",
    bar = "chart-bar",
    "chart-line"
  )
}


#' Split parameter_ids into regular vs missingness groups
#'
#' Uses the measures data frame to classify each parameter_id based on
#' its `parameter_category_3` value.
#'
#' @param param_ids Character vector of parameter_ids.
#' @param df_measures Measures data frame.
#' @return A list with elements `regular` and `missingness`.
#' @keywords internal
split_param_ids <- function(param_ids, df_measures) {
  cat3_map <- df_measures |>
    dplyr::distinct(.data$parameter_id, .data$parameter_category_3) |>
    dplyr::filter(.data$parameter_id %in% .env$param_ids)

  miss_ids <- cat3_map$parameter_id[cat3_map$parameter_category_3 == "ratio_missing"]
  reg_ids <- setdiff(param_ids, miss_ids)

  list(regular = reg_ids, missingness = miss_ids)
}


#' Render a score DT table with shared colour constants
#'
#' @param scores_display Data frame from [prepare_score_table_multi()].
#' @param thresh Numeric threshold for outlier column.
#' @param selected_sites Optional character vector of selected site names.
#'   When non-NULL, a "selected" column is added and pre-filtered to "yes".
#' @return A DT datatable object.
#' @keywords internal
render_score_dt <- function(scores_display, thresh, selected_sites = NULL) {
  scores_display$outlier <- ifelse(scores_display$max_score > thresh, "yes", "no")

  has_site_filter <- !is.null(selected_sites) && length(selected_sites) > 0
  if (has_site_filter) {
    scores_display$selected <- ifelse(
      scores_display$site %in% selected_sites, "yes", "no"
    )
  }

  feature_cols <- setdiff(names(scores_display),
                          c("site", "max_score", "outlier", "selected"))

  search_cols <- vector("list", ncol(scores_display))

  if (has_site_filter) {
    # When sites are selected, filter by selected only (not by outlier)
    selected_col_idx <- which(names(scores_display) == "selected")
    search_cols[[selected_col_idx]] <- list(search = "yes")
  } else {
    # When no sites selected, default to showing outliers only
    outlier_col_idx <- which(names(scores_display) == "outlier")
    search_cols[[outlier_col_idx]] <- list(search = "yes")
  }

  dt <- DT::datatable(
    scores_display,
    filter = "top",
    rownames = FALSE,
    extensions = "Buttons",
    options = list(
      pageLength = 10,
      lengthMenu = c(5, 10, 25, 50, 100),
      dom = "Blfrtip",
      buttons = c("copy", "csv", "excel"),
      scrollX = TRUE,
      order = list(list(which(names(scores_display) == "max_score") - 1, "desc")),
      searchCols = search_cols
    )
  )

  brks <- get_score_breaks()
  tbl_cols <- get_score_colors_table()
  tbl_text <- get_score_colors_table_text()

  for (col in feature_cols) {
    dt <- DT::formatStyle(
      dt, col,
      backgroundColor = DT::styleInterval(brks, tbl_cols),
      color = DT::styleInterval(brks, tbl_text)
    )
  }
  dt <- DT::formatStyle(
    dt, "max_score",
    backgroundColor = DT::styleInterval(brks, tbl_cols),
    color = DT::styleInterval(brks, tbl_text)
  )

  dt
}


#' Field Detail Module - Server
#'
#' Renders a clickable list of parameters in the sidebar. On selection,
#' shows pill-tabbed score tables (regular + missingness), the timeseries
#' plot, and a raw data table for outlier sites. Auto-detects plot type
#' from `parameter_category_3`. Supports feature sub-selection via
#' checkbox group.
#'
#' @param id Module namespace ID.
#' @param rctv_measures Reactive expression returning the measures data frame.
#' @param rctv_ctas_results Reactive expression returning the raw ctas results list.
#' @param rctv_untransformed Reactive expression returning the untransformed
#'   timeseries data frame (NULL for ctas sample data).
#' @param rctv_queries Reactive expression returning the query data frame
#'   (NULL when no queries are available).
#' @param rctv_dataset_label Reactive expression returning the dataset label
#'   string (e.g. "ctas sample", "SDTM sample", or a user filename).
#' @param rctv_studies Reactive expression returning a character vector of
#'   available study names, or NULL when data has no study column or only
#'   one study.
#' @param rctv_selected_sites Reactive expression returning a character vector
#'   of selected site names from the global site filter, or NULL when no
#'   sites are selected.
#' @export
mod_FieldDetail_server <- function(id, rctv_measures, rctv_ctas_results,
                                   rctv_untransformed = shiny::reactiveVal(NULL),
                                   rctv_queries = shiny::reactiveVal(NULL),
                                   rctv_dataset_label = shiny::reactiveVal(NULL),
                                   rctv_studies = shiny::reactiveVal(NULL),
                                   rctv_selected_sites = shiny::reactiveVal(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -- Study selector (rendered only when multi-study data) -------------------
    output$study_selector <- shiny::renderUI({
      studies <- rctv_studies()
      if (is.null(studies) || length(studies) <= 1) return(NULL)
      shiny::selectInput(
        ns("study_filter"),
        "Study",
        choices = stats::setNames(studies, studies),
        selected = studies[1]
      )
    })

    # -- Filtered data reactives (study-aware) ---------------------------------
    flt_measures <- shiny::reactive({
      m <- rctv_measures()
      shiny::req(m)
      sel <- input$study_filter
      if (is.null(sel) || sel == "__all__" ||
          !"study" %in% names(m)) return(m)
      study_subj <- unique(m$subject_id[!is.na(m[["study"]]) &
                                         m[["study"]] == sel])
      m[m$subject_id %in% study_subj, ]
    })

    flt_ctas_results <- shiny::reactive({
      res <- rctv_ctas_results()
      shiny::req(res)
      sel <- input$study_filter
      if (is.null(sel) || sel == "__all__") return(res)
      m <- flt_measures()
      study_sites <- unique(m$site)
      filtered_scores <- res$site_scores[res$site_scores$site %in% study_sites, ]
      kept_ts <- unique(filtered_scores$timeseries_id)
      list(
        site_scores = filtered_scores,
        timeseries = res$timeseries[res$timeseries$timeseries_id %in% kept_ts, ]
      )
    })

    flt_untransformed <- shiny::reactive({
      ut <- rctv_untransformed()
      if (is.null(ut)) return(NULL)
      sel <- input$study_filter
      if (is.null(sel) || sel == "__all__") return(ut)
      # Prefer filtering by `study` directly when the column is present, so
      # that subject_ids reused across studies do not pull in rows from
      # other studies.
      if ("study" %in% names(ut)) return(ut[ut$study == sel, ])
      m <- flt_measures()
      study_subj <- unique(m$subject_id)
      ut[ut$subject_id %in% study_subj, ]
    })

    flt_queries <- shiny::reactive({
      qd <- rctv_queries()
      if (is.null(qd)) return(NULL)
      sel <- input$study_filter
      if (is.null(sel) || sel == "__all__") return(qd)
      m <- flt_measures()
      study_subj <- unique(m$subject_id)
      qd[qd$subject_id %in% study_subj, ]
    })

    rctv_study <- shiny::reactive({ # nocov start
      sel <- input$study_filter
      if (is.null(sel) || sel == "__all__") NULL else sel
    }) # nocov end

    # -- Populate feature checkboxes from loaded data --------------------------
    shiny::observeEvent(flt_ctas_results(), {
      res <- flt_ctas_results()
      shiny::req(res, res$site_scores)
      feats <- sort(unique(res$site_scores$feature))
      cfg_defaults <- get_default_features()
      sel <- if (!is.null(cfg_defaults)) {
        intersect(cfg_defaults, feats)
      } else {
        feats
      }
      if (length(sel) == 0) sel <- feats
      shiny::updateCheckboxGroupInput(
        session, "selected_features",
        choices = feats, selected = sel
      )
    })

    # -- Measures with max_score recomputed for selected features --------------
    rctv_measures_feat <- shiny::reactive({
      m <- flt_measures()
      shiny::req(m)
      res <- flt_ctas_results()
      shiny::req(res)
      feats <- input$selected_features
      if (is.null(feats) || length(feats) == 0) return(m)

      all_feats <- sort(unique(res$site_scores$feature))
      if (identical(sort(feats), all_feats)) return(m)

      recompute_max_score(m, res, features = feats)
    })

    rctv_param_lookup <- shiny::reactive({
      df <- rctv_measures_feat()
      shiny::req(df)
      build_param_lookup(df)
    })

    # Helper: get param_ids for the selected display entry
    get_param_ids <- function() {
      lookup <- rctv_param_lookup()
      sel <- input$selected_param
      shiny::req(sel)
      match_row <- lookup$display_id == sel
      shiny::req(any(match_row))
      lookup$parameter_ids[match_row][[1]]
    }

    # Helper: filter out ratio_missing params when checkbox is OFF
    filter_param_ids <- function(param_ids) {
      include_miss <- input$include_miss %||% TRUE
      if (include_miss) return(param_ids)
      df <- rctv_measures_feat()
      splits <- split_param_ids(param_ids, df)
      splits$regular
    }

    # Helper: count outlier sites per field, restricted to either the
    # regular (non-ratio_missing) or the ratio_missing parameter_ids.
    compute_outliers <- function(include = c("regular", "missing")) {
      include <- match.arg(include)
      df <- rctv_measures_feat()
      shiny::req(df)
      lookup <- rctv_param_lookup()
      thresh <- input$thresh %||% 1.3
      sel_sites <- rctv_selected_sites()

      pid_map <- data.frame(
        display_id = rep(lookup$display_id, lengths(lookup$parameter_ids)),
        parameter_id = unlist(lookup$parameter_ids),
        stringsAsFactors = FALSE
      )

      miss_pids <- df |>
        dplyr::filter(.data$parameter_category_3 == "ratio_missing") |>
        dplyr::distinct(.data$parameter_id) |>
        dplyr::pull(.data$parameter_id)

      pid_map <- if (include == "missing") {
        pid_map[pid_map$parameter_id %in% miss_pids, , drop = FALSE]
      } else {
        pid_map[!pid_map$parameter_id %in% miss_pids, , drop = FALSE]
      }

      if (nrow(pid_map) == 0) {
        return(data.frame(display_id = character(),
                          n_outlier_sites = integer(),
                          stringsAsFactors = FALSE))
      }

      site_scores <- df |>
        dplyr::filter(.data$parameter_id %in% pid_map$parameter_id) |>
        dplyr::distinct(.data$site, .data$parameter_id, .data$max_score) |>
        dplyr::left_join(pid_map, by = "parameter_id")

      if (!is.null(sel_sites) && length(sel_sites) > 0) {
        site_scores <- site_scores |>
          dplyr::filter(.data$site %in% .env$sel_sites)
      }

      site_scores |>
        dplyr::summarise(
          max_score = max(.data$max_score, na.rm = TRUE),
          .by = c("site", "display_id")
        ) |>
        dplyr::summarise(
          n_outlier_sites = sum(.data$max_score > .env$thresh, na.rm = TRUE),
          .by = "display_id"
        )
    }

    param_outliers_regular <- shiny::reactive(compute_outliers("regular"))
    param_outliers_missing <- shiny::reactive(compute_outliers("missing"))

    # Fields whose parameter_ids have no rows in the ctas results
    param_no_ctas <- shiny::reactive({
      res <- flt_ctas_results()
      shiny::req(res)
      lookup <- rctv_param_lookup()
      ctas_pids <- unique(scores_with_parameter_id(res)$parameter_id)
      has_ctas <- vapply(lookup$parameter_ids, function(pids) {
        any(pids %in% ctas_pids)
      }, logical(1))
      lookup$display_id[!has_ctas]
    })

    # Fields whose only parameter_ids are ratio_missing (no regular sibling)
    param_only_miss <- shiny::reactive({
      df <- rctv_measures_feat()
      shiny::req(df)
      lookup <- rctv_param_lookup()
      only_miss <- vapply(lookup$parameter_ids, function(pids) {
        splits <- split_param_ids(pids, df)
        length(splits$regular) == 0 && length(splits$missingness) > 0
      }, logical(1))
      lookup$display_id[only_miss]
    })

    output$param_list <- shiny::renderUI({
      lookup <- rctv_param_lookup()
      shiny::req(lookup)
      stats_reg <- param_outliers_regular()
      include_miss <- input$include_miss %||% TRUE
      stats_miss <- if (include_miss) param_outliers_missing() else NULL
      no_ctas_ids <- param_no_ctas()
      only_miss_ids <- param_only_miss()

      display_ids <- lookup$display_id
      n_reg <- stats_reg$n_outlier_sites[match(display_ids, stats_reg$display_id)]
      n_reg[is.na(n_reg)] <- 0L
      n_miss <- if (is.null(stats_miss)) {
        rep(0L, length(display_ids))
      } else {
        m <- stats_miss$n_outlier_sites[match(display_ids, stats_miss$display_id)]
        m[is.na(m)] <- 0L
        m
      }

      # Apply free-text filter
      filter_text <- input$param_filter %||% ""
      if (nzchar(filter_text)) {
        keep <- grepl(filter_text, display_ids, ignore.case = TRUE)
        display_ids <- display_ids[keep]
        n_reg <- n_reg[keep]
        n_miss <- n_miss[keep]
      }
      if (length(display_ids) == 0) {
        return(shiny::tags$p(class = "text-muted", "No matching fields"))
      }

      # Apply sort
      sort_mode <- input$param_sort %||% "outliers"
      ord <- if (sort_mode == "alpha") {
        order(display_ids)
      } else {
        order(-(n_reg + n_miss), display_ids)
      }
      display_ids <- display_ids[ord]
      n_reg <- n_reg[ord]
      n_miss <- n_miss[ord]

      is_no_ctas <- display_ids %in% no_ctas_ids
      is_only_miss <- display_ids %in% only_miss_ids

      labels <- unname(lapply(seq_along(display_ids), function(i) {
        pid <- display_ids[i]
        row_idx <- which(lookup$display_id == pid)
        icon_name <- if (length(row_idx) == 1) {
          plot_type_icon(lookup$plot_type[row_idx], lookup$cat3_values[[row_idx]])
        } else {
          "chart-line" # nocov
        }

        parts <- list(
          shiny::icon(icon_name, class = "text-muted me-1"),
          pid
        )

        if (is_only_miss[i]) {
          parts <- c(parts, list(
            shiny::tags$span(
              title = "Only missingness ratio available for this field",
              shiny::icon("bug", class = "ms-1",
                          style = "color:#b58fe0;")
            )
          ))
          if (n_miss[i] > 0) {
            parts <- c(parts, list(
              shiny::tags$span(
                class = "badge ms-1",
                style = "background-color:#e0d4f7;color:#4b0082;",
                n_miss[i]
              )
            ))
          }
        } else if (is_no_ctas[i]) {
          parts <- c(parts, list(
            shiny::tags$span(
              title = "No CTAS results available for this field",
              shiny::icon("bug", class = "text-muted ms-1")
            )
          ))
        } else {
          if (n_reg[i] > 0) {
            parts <- c(parts, list(
              shiny::tags$span(class = "badge bg-warning text-dark ms-1", n_reg[i])
            ))
          }
          if (n_miss[i] > 0) {
            parts <- c(parts, list(
              shiny::tags$span(
                class = "badge ms-1",
                style = "background-color:#e0d4f7;color:#4b0082;",
                n_miss[i]
              )
            ))
          }
        }

        do.call(htmltools::tagList, parts)
      }))

      shiny::radioButtons(
        ns("selected_param"),
        label = NULL,
        choiceNames = labels,
        choiceValues = display_ids,
        selected = display_ids[1]
      )
    })

    output$plot_title <- shiny::renderText({
      p <- input$selected_param
      if (is.null(p)) return("Select a parameter")
      p
    })

    output$plot_subtitle <- shiny::renderText({
      p <- input$selected_param
      if (is.null(p)) return("")

      df <- rctv_measures_feat()
      shiny::req(df)
      pid <- get_param_ids()
      pnames <- df$parameter_name[df$parameter_id %in% pid &
                                   df$parameter_category_3 != "ratio_missing"]
      pnames <- unique(pnames)

      # Extract distinct parts before and after "="
      before_eq <- unique(sub("=.*", "", pnames))
      after_eq <- unique(sub("^[^=]*=", "", pnames))
      after_eq <- setdiff(after_eq, before_eq)
      parts <- if (length(after_eq) > 0) c(before_eq, sort(after_eq)) else before_eq
      paste(parts, collapse = ", ")
    })

    # -- Helper: get selected features for score table -------------------------
    get_selected_features <- function() {
      feats <- input$selected_features
      if (is.null(feats) || length(feats) == 0) return(NULL)
      res <- flt_ctas_results()
      shiny::req(res)
      all_feats <- sort(unique(res$site_scores$feature))
      if (identical(sort(feats), all_feats)) return(NULL)
      feats
    }

    # -- Scores data (shared between table & plot) -----------------------------
    rctv_scores_regular <- shiny::reactive({
      res <- flt_ctas_results()
      shiny::req(res)
      param_ids <- get_param_ids()
      df <- rctv_measures_feat()
      feats <- get_selected_features()
      splits <- split_param_ids(param_ids, df)
      # nocov start
      if (length(splits$regular) == 0) return(NULL)
      prepare_score_table_multi(res, splits$regular, features = feats)
      # nocov end
    })

    # Helper: get sites from the current page of the regular score table,
    # hard-capped at 24 sites.
    get_plot_sites <- function() { # nocov start
      row_idx <- input$score_table_regular_rows_current
      scores <- rctv_scores_regular()
      if (is.null(row_idx) || length(row_idx) == 0 || is.null(scores)) {
        return(NULL)
      }
      sites <- scores$site[row_idx]
      if (length(sites) > 24) sites <- sites[seq_len(24)]
      sites
    } # nocov end

    # -- Regular score table (pill tab 1) --------------------------------------

    output$score_table_regular <- DT::renderDataTable({
      scores_display <- rctv_scores_regular()
      shiny::req(scores_display, nrow(scores_display) > 0)
      thresh <- input$thresh %||% 1.3
      sel_sites <- rctv_selected_sites()
      render_score_dt(scores_display, thresh, selected_sites = sel_sites)
    })

    # Consolidated notice shown next to the linkage radio.
    output$field_notice <- shiny::renderUI({
      sel <- input$selected_param
      if (is.null(sel)) return(NULL)
      lookup <- rctv_param_lookup()
      match_row <- lookup$display_id == sel
      if (!any(match_row)) return(NULL) # nocov

      if (sel %in% param_only_miss()) {
        return(shiny::tags$div(
          class = "alert py-1 px-2 mb-0",
          style = "font-size:0.9em;background-color:#e0d4f7;color:#4b0082;",
          shiny::icon("circle-info", class = "me-1"),
          "Only missingness ratio available"
        ))
      }

      scores_reg <- rctv_scores_regular()
      if (is.null(scores_reg) || nrow(scores_reg) == 0) {
        return(shiny::tags$div(
          class = "alert alert-warning py-1 px-2 mb-0",
          style = "font-size:0.9em;",
          shiny::icon("triangle-exclamation", class = "me-1"),
          "No outlier scores available (too few timepoints for ctas to compute features)."
        ))
      }

      NULL
    })

    # -- Missingness score table (pill tab 2) ----------------------------------

    rctv_scores_miss <- shiny::reactive({
      res <- flt_ctas_results()
      shiny::req(res)
      param_ids <- get_param_ids()
      df <- rctv_measures_feat()
      feats <- get_selected_features()
      splits <- split_param_ids(param_ids, df)
      if (length(splits$missingness) == 0) return(NULL) # nocov
      scores_display <- prepare_score_table_multi(res, splits$missingness,
                                                  features = feats)
      if (is.null(scores_display) || nrow(scores_display) == 0) return(NULL) # nocov
      scores_display
    })

    output$score_table_miss <- DT::renderDataTable({
      res <- flt_ctas_results()
      shiny::req(res)
      param_ids <- get_param_ids()
      df <- rctv_measures_feat()
      splits <- split_param_ids(param_ids, df)
      shiny::validate(shiny::need(
        length(splits$missingness) > 0,
        "No missingness parameters for this field."
      ))
      scores_display <- rctv_scores_miss()
      shiny::validate(shiny::need(
        !is.null(scores_display) && nrow(scores_display) > 0,
        "No missingness scores available (too few timepoints for ctas to compute features)."
      ))
      thresh <- input$thresh %||% 1.3
      render_score_dt(scores_display, thresh,
                      selected_sites = rctv_selected_sites())
    })

    # -- Visit sorter (arrow buttons for categorical/bar x-axis) ---------------
    # rctv_visit_order: live state edited by arrow buttons (drives the UI list)
    # rctv_visit_order_applied: confirmed state used by the plot (updated on Apply)
    rctv_visit_order <- shiny::reactiveVal(NULL)
    rctv_visit_order_applied <- shiny::reactiveVal(NULL)

    shiny::observe({
      shiny::req(input$selected_param)
      lookup <- rctv_param_lookup()
      sel <- input$selected_param
      match_row <- lookup$display_id == sel
      shiny::req(any(match_row))
      plot_type <- lookup$plot_type[match_row]
      if (!plot_type %in% c("categorical", "bar")) {
        rctv_visit_order(NULL)
        rctv_visit_order_applied(NULL)
        return()
      }
      df <- rctv_measures_feat()
      param_ids <- lookup$parameter_ids[match_row][[1]]
      default <- get_plot_visit_levels(param_ids, df, plot_type = plot_type)
      rctv_visit_order(default)
      rctv_visit_order_applied(default)
    })

    shiny::observeEvent(input$visit_move, {
      msg <- input$visit_move
      lvls <- rctv_visit_order()
      shiny::req(lvls)
      i <- msg$idx
      if (msg$dir == "up" && i > 1) {
        lvls[c(i - 1, i)] <- lvls[c(i, i - 1)]
      }
      if (msg$dir == "down" && i < length(lvls)) {
        lvls[c(i, i + 1)] <- lvls[c(i + 1, i)]
      }
      rctv_visit_order(lvls)
    })

    shiny::observeEvent(input$apply_visit_order, {
      rctv_visit_order_applied(rctv_visit_order())
    })

    output$visit_sorter <- shiny::renderUI({
      lvls <- rctv_visit_order()
      if (is.null(lvls) || length(lvls) == 0) return(NULL)
      n <- length(lvls)
      move_id <- ns("visit_move")

      rows <- lapply(seq_len(n), function(i) {
        up_disabled <- if (i == 1) "disabled" else NULL
        dn_disabled <- if (i == n) "disabled" else NULL
        shiny::tags$div(
          style = "display:flex;align-items:center;gap:4px;margin:1px 0;",
          shiny::tags$button(
            shiny::icon("arrow-up"),
            onclick = sprintf(
              "Shiny.setInputValue('%s',{idx:%d,dir:'up'},{priority:'event'})",
              move_id, i
            ),
            class = "btn btn-sm btn-outline-secondary py-0 px-1",
            disabled = up_disabled
          ),
          shiny::tags$button(
            shiny::icon("arrow-down"),
            onclick = sprintf(
              "Shiny.setInputValue('%s',{idx:%d,dir:'down'},{priority:'event'})",
              move_id, i
            ),
            class = "btn btn-sm btn-outline-secondary py-0 px-1",
            disabled = dn_disabled
          ),
          shiny::tags$span(lvls[i], style = "font-size:.85em;")
        )
      })

      shiny::tagList(
        shiny::tags$p(shiny::tags$small("Reorder visits:")),
        shiny::tags$div(rows),
        shiny::actionButton(
          ns("apply_visit_order"), "Apply order",
          icon = shiny::icon("refresh"),
          class = "btn btn-sm btn-primary mt-2"
        )
      )
    })

    # -- Timeseries / categorical / bar plot -----------------------------------
    # Track which parameter each DT's rows_current belongs to, so we can
    # ignore stale row indices after the user switches fields.
    rv_dt_param <- shiny::reactiveVal(NULL)
    rv_dt_param_miss <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$selected_param, { # nocov start
      rv_dt_param(NULL)
      rv_dt_param_miss(NULL)
    }) # nocov end

    shiny::observeEvent(input$score_table_regular_rows_current, { # nocov start
      rv_dt_param(input$selected_param)
    }) # nocov end

    shiny::observeEvent(input$score_table_miss_rows_current, { # nocov start
      rv_dt_param_miss(input$selected_param)
    }) # nocov end

    # Suppress the tab->linkage sync when we programmatically switch tabs
    # in response to a field selection change.
    skip_tab_sync <- shiny::reactiveVal(FALSE)

    # Sync linkage radio with the score-table pill tab click.
    shiny::observeEvent(input$score_tab, { # nocov start
      if (isTRUE(skip_tab_sync())) {
        skip_tab_sync(FALSE)
        return()
      }
      new_mode <- if (identical(input$score_tab, "Missingness Scores")) {
        "missingness"
      } else {
        "regular"
      }
      if (!identical(input$linkage_mode, new_mode)) {
        shiny::updateRadioButtons(session, "linkage_mode", selected = new_mode)
      }
    }, ignoreInit = TRUE) # nocov end

    # Auto-configure tab + linkage when the user picks a field:
    #   regular outliers  -> Regular tab, linkage = regular
    #   only miss outliers -> Missingness tab, linkage = missingness
    #   otherwise (bug or no outliers) -> Regular tab, linkage = none
    shiny::observeEvent(input$selected_param, { # nocov start
      sel <- input$selected_param
      if (is.null(sel)) return()

      stats_reg <- param_outliers_regular()
      stats_miss <- param_outliers_missing()
      n_reg <- stats_reg$n_outlier_sites[match(sel, stats_reg$display_id)]
      if (length(n_reg) == 0 || is.na(n_reg)) n_reg <- 0L
      n_miss <- stats_miss$n_outlier_sites[match(sel, stats_miss$display_id)]
      if (length(n_miss) == 0 || is.na(n_miss)) n_miss <- 0L

      if (n_reg > 0) {
        new_mode <- "regular"
        new_tab <- "Regular Scores"
      } else if (n_miss > 0) {
        new_mode <- "missingness"
        new_tab <- "Missingness Scores"
      } else {
        new_mode <- "none"
        new_tab <- "Regular Scores"
      }

      if (!identical(input$score_tab, new_tab)) {
        skip_tab_sync(TRUE)
        bslib::nav_select(id = "score_tab", selected = new_tab, session = session)
      }
      if (!identical(input$linkage_mode, new_mode)) {
        shiny::updateRadioButtons(session, "linkage_mode", selected = new_mode)
      }
    }) # nocov end

    rctv_plot_sites <- shiny::reactive({
      mode <- input$linkage_mode %||% "regular"

      if (mode == "none") {
        df <- rctv_measures_feat()
        shiny::req(df, input$selected_param)
        param_ids <- get_param_ids()
        param_ids <- filter_param_ids(param_ids)
        if (length(param_ids) == 0) return(NULL) # nocov
        sites <- unique(df$site[df$parameter_id %in% param_ids])
        if (length(sites) == 0) return(NULL) # nocov
        return(sites)
      }

      if (mode == "missingness") {
        scores <- rctv_scores_miss()
        if (is.null(scores) || nrow(scores) == 0) return(NULL) # nocov
        row_idx <- input$score_table_miss_rows_current
        dt_fresh <- identical(rv_dt_param_miss(), input$selected_param)
        if (!dt_fresh || is.null(row_idx) || length(row_idx) == 0) {
          return(NULL) # nocov
        }
        sites <- scores$site[row_idx]
        if (length(sites) > 24) sites <- sites[seq_len(24)] # nocov
        return(sites)
      }

      # default: regular
      scores <- rctv_scores_regular()
      if (is.null(scores) || nrow(scores) == 0) return(NULL)

      row_idx <- input$score_table_regular_rows_current
      dt_fresh <- identical(rv_dt_param(), input$selected_param)

      if (!dt_fresh || is.null(row_idx) || length(row_idx) == 0) {
        return(NULL) # nocov
      }

      sites <- scores$site[row_idx]
      if (length(sites) > 24) sites <- sites[seq_len(24)]
      sites
    })

    output$ts_plot <- shiny::renderPlot({
      df <- rctv_measures_feat()
      shiny::req(df)
      shiny::req(input$selected_param)
      plot_sites <- rctv_plot_sites()
      shiny::req(plot_sites)

      lookup <- rctv_param_lookup()
      sel <- input$selected_param
      match_row <- lookup$display_id == sel
      shiny::req(any(match_row))
      plot_type <- lookup$plot_type[match_row]
      param_ids <- lookup$parameter_ids[match_row][[1]]
      param_ids <- filter_param_ids(param_ids)
      shiny::req(length(param_ids) > 0)
      thresh <- input$thresh %||% 0
      visit_order <- rctv_visit_order_applied()

      qd <- flt_queries()

      if (plot_type == "categorical") {
        plot_categorical(param_ids, df, thresh = thresh,
                         sites = plot_sites,
                         visit_order = visit_order)
      } else if (plot_type == "bar") {
        plot_bar(param_ids, df, thresh = thresh,
                 sites = plot_sites,
                 visit_order = visit_order)
      } else {
        plot_timeseries(param_ids, df, thresh = thresh,
                        sites = plot_sites, query_data = qd)
      }
    }, res = 96)

    # -- Timeseries data table for outlier sites -------------------------------
    output$ts_data_table <- DT::renderDataTable({
      df <- rctv_measures_feat()
      shiny::req(df)
      shiny::req(input$selected_param)
      plot_sites <- rctv_plot_sites()
      shiny::req(plot_sites)

      lookup <- rctv_param_lookup()
      sel <- input$selected_param
      match_row <- lookup$display_id == sel
      shiny::req(any(match_row))
      plot_type <- lookup$plot_type[match_row]
      param_ids <- lookup$parameter_ids[match_row][[1]]
      param_ids <- filter_param_ids(param_ids)
      shiny::req(length(param_ids) > 0)

      untransformed <- flt_untransformed()
      ts_data <- prepare_ts_data_multi(df, param_ids, thresh = 0,
                                       untransformed = untransformed,
                                       plot_type = plot_type,
                                       sites = plot_sites)
      shiny::req(nrow(ts_data) > 0)

      # Hide parameter_id, parameter_name, and any extra pass-through columns
      # from the untransformed upload by default; users can toggle them on
      # via the colvis button.
      default_visible <- c(
        "site", "subject_id", "parameter_category_2",
        "timepoint_rank", "timepoint_1_name",
        "original_value", "lower", "upper", "original_category",
        "result", "max_score"
      )
      hide_cols <- which(!names(ts_data) %in% default_visible) - 1L

      DT::datatable(
        ts_data,
        filter = "top",
        rownames = FALSE,
        extensions = c("Buttons", "ColReorder"),
        options = list(
          pageLength = 25,
          lengthMenu = c(5, 10, 25, 50, 100),
          dom = "Blfrtip",
          buttons = list("copy", "csv", "excel", "colvis"),
          colReorder = TRUE,
          scrollX = TRUE,
          columnDefs = if (length(hide_cols) > 0) {
            list(list(visible = FALSE, targets = as.list(hide_cols)))
          } else {
            list()
          }
        )
      )
    })

    # -- Query data table -------------------------------------------------------
    output$query_table <- DT::renderDataTable({
      qd <- flt_queries()
      shiny::validate(shiny::need(
        !is.null(qd) && nrow(qd) > 0,
        "No query data available for this dataset."
      ))
      shiny::req(input$selected_param)
      plot_sites <- rctv_plot_sites()
      shiny::req(plot_sites)

      param_ids <- get_param_ids()
      param_ids <- filter_param_ids(param_ids)
      shiny::req(length(param_ids) > 0)

      df <- rctv_measures_feat()

      subj_site <- df |>
        dplyr::distinct(.data$subject_id, .data$site)

      q_filtered <- qd |>
        dplyr::filter(.data$parameter_id %in% param_ids) |>
        dplyr::left_join(subj_site, by = "subject_id") |>
        dplyr::filter(.data$site %in% plot_sites)

      # Move key columns to the front; keep all remaining columns after
      front_cols <- intersect(
        c("site", "subject_id", "visit", "domain", "field",
          "query_status", "query_type", "data_change",
          "query_text", "query_answer",
          "value_first_entry", "value_at_query_open", "value_at_open",
          "value_at_query_close", "value_at_close",
          "value_now", "value_at_current"),
        names(q_filtered)
      )
      rest_cols <- setdiff(names(q_filtered), front_cols)
      q_filtered <- q_filtered[, c(front_cols, rest_cols), drop = FALSE]

      shiny::validate(shiny::need(
        nrow(q_filtered) > 0,
        "No queries for outlier sites on this parameter."
      ))

      # Default-visible columns; hide everything else via colvis
      default_visible <- c(
        "site", "subject_id", "visit", "domain", "field",
        "query_status", "query_type", "data_change",
        "query_text", "query_answer",
        "value_first_entry", "value_at_query_open", "value_at_open",
        "value_at_query_close", "value_at_close",
        "value_now", "value_at_current"
      )
      hidden_idx <- which(!names(q_filtered) %in% default_visible) - 1L

      col_defs <- if (length(hidden_idx) > 0) {
        list(list(visible = FALSE, targets = as.list(hidden_idx)))
      } else {
        NULL # nocov
      }

      DT::datatable(
        q_filtered,
        filter = "top",
        rownames = FALSE,
        extensions = c("Buttons", "ColReorder"),
        options = list(
          pageLength = 25,
          lengthMenu = c(5, 10, 25, 50, 100),
          dom = "Blfrtip",
          buttons = list("copy", "csv", "excel", "colvis"),
          colReorder = TRUE,
          scrollX = TRUE,
          columnDefs = col_defs
        )
      )
    })
  })
}
