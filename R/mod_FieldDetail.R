#' Field Detail Module - UI
#'
#' Sidebar with threshold slider and clickable parameter list. Main content
#' flows like a webpage: score table, timeseries plot, then data table.
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
      shiny::hr(),
      shiny::uiOutput(ns("param_list"))
    ),
    shiny::h4(shiny::textOutput(ns("plot_title"))),
    shiny::h5("Site Scores by Feature"),
    DT::dataTableOutput(ns("score_table")),
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

  # Groups with mixed category_3 (e.g. range_normalized + ratio_missing)
  # or with categorical/bar types should stay grouped by category_2.
  # Groups with a single category_3 that are "numeric" and have multiple
  # parameter_ids should be split back to individual entries.
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


#' Field Detail Module - Server
#'
#' Renders a clickable list of parameters in the sidebar. On selection,
#' shows a per-feature score table, the timeseries plot, and a raw data table
#' for outlier sites. Auto-detects plot type from `parameter_category_3`.
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

    param_outliers <- shiny::reactive({
      df <- rctv_measures()
      shiny::req(df)
      lookup <- rctv_param_lookup()
      thresh <- input$thresh %||% 1.3

      pid_map <- data.frame(
        display_id = rep(lookup$display_id, lengths(lookup$parameter_ids)),
        parameter_id = unlist(lookup$parameter_ids),
        stringsAsFactors = FALSE
      )

      site_scores <- df |>
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

    # -- Score table: per-feature scores pivoted wide --------------------------
    output$score_table <- DT::renderDataTable({
      res <- rctv_ctas_results()
      shiny::req(res)
      shiny::req(input$selected_param)

      lookup <- rctv_param_lookup()
      sel <- input$selected_param
      thresh <- input$thresh %||% 1.3

      match_row <- lookup$display_id == sel
      shiny::req(any(match_row))
      param_ids <- lookup$parameter_ids[match_row][[1]]

      scores_display <- prepare_score_table_multi(res, param_ids)
      shiny::validate(shiny::need(
        nrow(scores_display) > 0,
        "No outlier scores available for this parameter (too few timepoints for ctas to compute features)."
      ))
      scores_display$outlier <- ifelse(scores_display$max_score > thresh, "yes", "no")

      feature_cols <- setdiff(names(scores_display), c("site", "max_score", "outlier"))

      outlier_col_idx <- which(names(scores_display) == "outlier")
      search_cols <- vector("list", ncol(scores_display))
      search_cols[[outlier_col_idx]] <- list(search = "yes")

      brks <- c(1.3, 3, 5, 10)
      clrs <- c("white", "#fff3cd", "#ffcc80", "#ff9800", "#e65100")

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
        dt <- DT::formatStyle(dt, col, backgroundColor = DT::styleInterval(brks, clrs))
      }
      dt <- DT::formatStyle(dt, "max_score", backgroundColor = DT::styleInterval(brks, clrs))

      dt
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
