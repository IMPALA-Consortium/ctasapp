#' Field Detail Module - UI
#'
#' Sidebar with threshold slider, missingness toggle, and clickable parameter
#' list. Main content flows like a webpage: pill-tabbed score tables,
#' timeseries plot, then data table.
#'
#' @param id Module namespace ID.
#' @export
mod_FieldDetail_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      title = "Parameters",
      width = 250,
      shiny::sliderInput(
        ns("thresh"),
        "Score Threshold",
        min = 0, max = 10, value = 1.3, step = 0.1
      ),
      shiny::checkboxInput(ns("include_miss"), "Include Missingness", value = TRUE),
      shiny::hr(),
      shiny::uiOutput(ns("param_list"))
    ),
    shiny::h4(shiny::textOutput(ns("plot_title"))),
    shiny::h5("Site Scores by Feature"),
    bslib::navset_pill(
      bslib::nav_panel("Regular Scores", DT::dataTableOutput(ns("score_table_regular"))),
      bslib::nav_panel("Missingness Scores", DT::dataTableOutput(ns("score_table_miss")))
    ),
    shiny::hr(),
    shiny::plotOutput(ns("ts_plot"), height = "700px"),
    shiny::hr(),
    shiny::h5("Timeseries Data (Outlier Sites)"),
    DT::dataTableOutput(ns("ts_data_table"))
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
#' @param df Measures data frame.
#' @return A data frame with `display_id`, `parameter_ids` (list column),
#'   and `plot_type`.
#' @keywords internal
build_param_lookup <- function(df) {
  param_meta <- df |>
    dplyr::distinct(.data$parameter_id, .data$parameter_category_2,
                    .data$parameter_category_3)

  by_cat2 <- param_meta |>
    dplyr::summarise(
      parameter_ids = list(.data$parameter_id),
      plot_type = determine_plot_type(.data$parameter_category_3),
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
        n_cat3 = 1L,
        .by = "parameter_id"
      ) |>
      dplyr::rename(parameter_category_2 = "parameter_id")

    keep <- dplyr::bind_rows(keep, expanded)
  }

  keep |>
    dplyr::select("parameter_category_2", "parameter_ids", "plot_type") |>
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
      buttons = c("csv", "excel"),
      scrollX = TRUE,
      order = list(list(which(names(scores_display) == "max_score") - 1, "desc")),
      searchCols = search_cols
    )
  )

  for (col in feature_cols) {
    dt <- DT::formatStyle(
      dt, col,
      backgroundColor = DT::styleInterval(SCORE_BREAKS, SCORE_COLORS_TABLE)
    )
  }
  dt <- DT::formatStyle(
    dt, "max_score",
    backgroundColor = DT::styleInterval(SCORE_BREAKS, SCORE_COLORS_TABLE)
  )

  dt
}


#' Field Detail Module - Server
#'
#' Renders a clickable list of parameters in the sidebar. On selection,
#' shows pill-tabbed score tables (regular + missingness), the timeseries
#' plot, and a raw data table for outlier sites. Auto-detects plot type
#' from `parameter_category_3`.
#'
#' @param id Module namespace ID.
#' @param rctv_measures Reactive expression returning the measures data frame.
#' @param rctv_ctas_results Reactive expression returning the raw ctas results list.
#' @param rctv_untransformed Reactive expression returning the untransformed
#'   timeseries data frame (NULL for ctas sample data).
#' @export
mod_FieldDetail_server <- function(id, rctv_measures, rctv_ctas_results,
                                   rctv_untransformed = shiny::reactiveVal(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rctv_param_lookup <- shiny::reactive({
      df <- rctv_measures()
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
      df <- rctv_measures()
      splits <- split_param_ids(param_ids, df)
      splits$regular
    }

    param_outliers <- shiny::reactive({
      df <- rctv_measures()
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

      labels <- unname(lapply(stats$display_id, function(pid) {
        n_out <- stats$n_outlier_sites[stats$display_id == pid]
        badge_class <- if (n_out > 0) "badge bg-warning text-dark ms-1" else "badge bg-light text-muted ms-1"
        htmltools::tagList(
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
      if (is.null(p)) "Select a parameter" else paste("Parameter:", p)
    })

    # -- Regular score table (pill tab 1) --------------------------------------
    output$score_table_regular <- DT::renderDataTable({
      res <- rctv_ctas_results()
      shiny::req(res)
      param_ids <- get_param_ids()
      df <- rctv_measures()
      thresh <- input$thresh %||% 1.3

      splits <- split_param_ids(param_ids, df)
      shiny::validate(shiny::need(
        length(splits$regular) > 0,
        "No regular (non-missingness) parameters for this field."
      ))
      scores_display <- prepare_score_table_multi(res, splits$regular)
      shiny::validate(shiny::need(
        nrow(scores_display) > 0,
        "No outlier scores available (too few timepoints for ctas to compute features)."
      ))
      render_score_dt(scores_display, thresh)
    })

    # -- Missingness score table (pill tab 2) ----------------------------------
    output$score_table_miss <- DT::renderDataTable({
      res <- rctv_ctas_results()
      shiny::req(res)
      param_ids <- get_param_ids()
      df <- rctv_measures()
      thresh <- input$thresh %||% 1.3

      splits <- split_param_ids(param_ids, df)
      shiny::validate(shiny::need(
        length(splits$missingness) > 0,
        "No missingness parameters for this field."
      ))
      scores_display <- prepare_score_table_multi(res, splits$missingness)
      shiny::validate(shiny::need(
        nrow(scores_display) > 0,
        "No missingness scores available (too few timepoints for ctas to compute features)."
      ))
      render_score_dt(scores_display, thresh)
    })

    # -- Timeseries / categorical / bar plot -----------------------------------
    output$ts_plot <- shiny::renderPlot({
      df <- rctv_measures()
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

      if (plot_type == "categorical") {
        plot_categorical(param_ids, df, thresh = thresh)
      } else if (plot_type == "bar") {
        plot_bar(param_ids, df, thresh = thresh)
      } else {
        plot_timeseries(param_ids, df, thresh = thresh)
      }
    }, res = 96)

    # -- Timeseries data table for outlier sites -------------------------------
    output$ts_data_table <- DT::renderDataTable({
      df <- rctv_measures()
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
      untransformed <- rctv_untransformed()
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
          buttons = c("csv", "excel"),
          scrollX = TRUE
        )
      )
    })
  })
}
