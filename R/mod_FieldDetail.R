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
      shiny::uiOutput(ns("param_list"))
    ),
    shiny::h4(shiny::textOutput(ns("plot_title"))),
    shiny::h5(shiny::textOutput(ns("plot_subtitle"))),
    bslib::navset_pill(
      bslib::nav_panel("Regular Scores", DT::dataTableOutput(ns("score_table_regular"))),
      bslib::nav_panel("Missingness Scores", DT::dataTableOutput(ns("score_table_miss")))
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
#' @return A DT datatable object.
#' @keywords internal
render_score_dt <- function(scores_display, thresh) {
  scores_display$outlier <- ifelse(scores_display$max_score > thresh, "yes", "no")

  feature_cols <- setdiff(names(scores_display), c("site", "max_score", "outlier"))

  outlier_col_idx <- which(names(scores_display) == "outlier")
  search_cols <- vector("list", ncol(scores_display))
  search_cols[[outlier_col_idx]] <- list(search = "yes")

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
#' @export
mod_FieldDetail_server <- function(id, rctv_measures, rctv_ctas_results,
                                   rctv_untransformed = shiny::reactiveVal(NULL),
                                   rctv_queries = shiny::reactiveVal(NULL),
                                   rctv_dataset_label = shiny::reactiveVal(NULL),
                                   rctv_studies = shiny::reactiveVal(NULL)) {
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

    rctv_study <- shiny::reactive({
      sel <- input$study_filter
      if (is.null(sel) || sel == "__all__") NULL else sel
    })

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

    param_outliers <- shiny::reactive({
      df <- rctv_measures_feat()
      shiny::req(df)
      lookup <- rctv_param_lookup()
      thresh <- input$thresh %||% 1.3
      include_miss <- input$include_miss %||% TRUE

      pid_map <- data.frame(
        display_id = rep(lookup$display_id, lengths(lookup$parameter_ids)),
        parameter_id = unlist(lookup$parameter_ids),
        stringsAsFactors = FALSE
      )

      if (!include_miss) {
        miss_pids <- df |>
          dplyr::filter(.data$parameter_category_3 == "ratio_missing") |>
          dplyr::distinct(.data$parameter_id) |>
          dplyr::pull(.data$parameter_id)
        pid_map <- pid_map[!pid_map$parameter_id %in% miss_pids, , drop = FALSE]
      }

      site_scores <- df |>
        dplyr::filter(.data$parameter_id %in% pid_map$parameter_id) |>
        dplyr::distinct(.data$site, .data$parameter_id, .data$max_score) |>
        dplyr::left_join(pid_map, by = "parameter_id")

      site_scores |>
        dplyr::summarise(
          max_score = max(.data$max_score, na.rm = TRUE),
          .by = c("site", "display_id")
        ) |>
        dplyr::summarise(
          n_outlier_sites = sum(.data$max_score > .env$thresh, na.rm = TRUE),
          .by = "display_id"
        ) |>
        dplyr::arrange(dplyr::desc(.data$n_outlier_sites))
    })

    output$param_list <- shiny::renderUI({
      stats <- param_outliers()
      shiny::req(stats)
      lookup <- rctv_param_lookup()

      labels <- unname(lapply(stats$display_id, function(pid) {
        n_out <- stats$n_outlier_sites[stats$display_id == pid]
        badge_class <- if (n_out > 0) "badge bg-warning text-dark ms-1" else "badge bg-light text-muted ms-1"

        row_idx <- which(lookup$display_id == pid)
        icon_name <- if (length(row_idx) == 1) {
          plot_type_icon(lookup$plot_type[row_idx], lookup$cat3_values[[row_idx]])
        } else {
          "chart-line" # nocov
        }

        htmltools::tagList(
          shiny::icon(icon_name, class = "text-muted me-1"),
          pid,
          shiny::tags$span(class = badge_class, n_out)
        )
      }))

      shiny::radioButtons(
        ns("selected_param"),
        label = NULL,
        choiceNames = labels,
        choiceValues = stats$display_id,
        selected = stats$display_id[1]
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
      if (length(splits$regular) == 0) return(NULL)
      prepare_score_table_multi(res, splits$regular, features = feats)
    })

    # Helper: get sites from the current page of the regular score table,
    # hard-capped at 24 sites.
    get_plot_sites <- function() {
      row_idx <- input$score_table_regular_rows_current
      scores <- rctv_scores_regular()
      if (is.null(row_idx) || length(row_idx) == 0 || is.null(scores)) {
        return(NULL)
      }
      sites <- scores$site[row_idx]
      if (length(sites) > 24) sites <- sites[seq_len(24)]
      sites
    }

    # -- Regular score table (pill tab 1) --------------------------------------
    output$score_table_regular <- DT::renderDataTable({
      scores_display <- rctv_scores_regular()
      shiny::validate(shiny::need(
        !is.null(scores_display) && nrow(scores_display) > 0,
        "No outlier scores available (too few timepoints for ctas to compute features)."
      ))
      thresh <- input$thresh %||% 1.3
      render_score_dt(scores_display, thresh)
    })

    # -- Missingness score table (pill tab 2) ----------------------------------
    output$score_table_miss <- DT::renderDataTable({
      res <- flt_ctas_results()
      shiny::req(res)
      param_ids <- get_param_ids()
      df <- rctv_measures_feat()
      thresh <- input$thresh %||% 1.3
      feats <- get_selected_features()

      splits <- split_param_ids(param_ids, df)
      shiny::validate(shiny::need(
        length(splits$missingness) > 0,
        "No missingness parameters for this field."
      ))
      scores_display <- prepare_score_table_multi(res, splits$missingness,
                                                  features = feats)
      shiny::validate(shiny::need(
        nrow(scores_display) > 0,
        "No missingness scores available (too few timepoints for ctas to compute features)."
      ))
      render_score_dt(scores_display, thresh)
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
    output$ts_plot <- shiny::renderPlot({
      df <- rctv_measures_feat()
      shiny::req(df)
      shiny::req(input$selected_param)

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

      plot_sites <- get_plot_sites()

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

      lookup <- rctv_param_lookup()
      sel <- input$selected_param
      match_row <- lookup$display_id == sel
      shiny::req(any(match_row))
      param_ids <- lookup$parameter_ids[match_row][[1]]
      param_ids <- filter_param_ids(param_ids)
      shiny::req(length(param_ids) > 0)

      thresh <- input$thresh %||% 1.3
      untransformed <- flt_untransformed()
      ts_data <- prepare_ts_data_multi(df, param_ids, thresh,
                                       untransformed = untransformed)
      shiny::validate(shiny::need(
        nrow(ts_data) > 0,
        "No outlier site data to display (no sites exceed the threshold for this parameter)."
      ))

      DT::datatable(
        ts_data,
        filter = "top",
        rownames = FALSE,
        extensions = "Buttons",
        options = list(
          pageLength = 25,
          lengthMenu = c(5, 10, 25, 50, 100),
          dom = "Blfrtip",
          buttons = c("copy", "csv", "excel"),
          scrollX = TRUE
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

      param_ids <- get_param_ids()
      param_ids <- filter_param_ids(param_ids)
      shiny::req(length(param_ids) > 0)

      df <- rctv_measures_feat()
      thresh <- input$thresh %||% 1.3
      outlier_sites <- df |>
        dplyr::filter(.data$parameter_id %in% param_ids) |>
        dplyr::filter(.data$max_score > thresh) |>
        dplyr::distinct(.data$site) |>
        dplyr::pull(.data$site)

      subj_site <- df |>
        dplyr::distinct(.data$subject_id, .data$site)

      q_filtered <- qd |>
        dplyr::filter(.data$parameter_id %in% param_ids) |>
        dplyr::left_join(subj_site, by = "subject_id") |>
        dplyr::filter(.data$site %in% outlier_sites) |>
        dplyr::select(
          "site", "subject_id", "visit", "domain", "field",
          "query_status", "query_type", "data_change",
          "query_text", "query_answer",
          "value_first_entry", "value_now"
        )

      shiny::validate(shiny::need(
        nrow(q_filtered) > 0,
        "No queries for outlier sites on this parameter."
      ))

      DT::datatable(
        q_filtered,
        filter = "top",
        rownames = FALSE,
        extensions = "Buttons",
        options = list(
          pageLength = 25,
          lengthMenu = c(5, 10, 25, 50, 100),
          dom = "Blfrtip",
          buttons = c("copy", "csv", "excel"),
          scrollX = TRUE
        )
      )
    })
  })
}
