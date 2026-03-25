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

#' Field Detail Module - Server
#'
#' Renders a clickable list of parameter_ids in the sidebar. On selection,
#' shows a per-feature score table, the timeseries plot, and a raw data table
#' for outlier sites.
#'
#' @param id Module namespace ID.
#' @param rctv_measures Reactive expression returning the measures data frame.
#' @param rctv_ctas_results Reactive expression returning the raw ctas results list.
#' @export
mod_FieldDetail_server <- function(id, rctv_measures, rctv_ctas_results) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    param_outliers <- shiny::reactive({
      df <- rctv_measures()
      shiny::req(df)
      thresh <- input$thresh %||% 1.3

      site_scores <- df |>
        dplyr::distinct(.data$site, .data$parameter_id, .data$max_score)

      site_scores |>
        dplyr::summarise(
          n_outlier_sites = sum(.data$max_score > .env$thresh, na.rm = TRUE),
          .by = "parameter_id"
        ) |>
        dplyr::arrange(dplyr::desc(.data$n_outlier_sites))
    })

    output$param_list <- shiny::renderUI({
      stats <- param_outliers()
      shiny::req(stats)

      labels <- unname(lapply(stats$parameter_id, function(pid) {
        n_out <- stats$n_outlier_sites[stats$parameter_id == pid]
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
        choiceValues = stats$parameter_id,
        selected = stats$parameter_id[1]
      )
    })

    output$plot_title <- shiny::renderText({
      p <- input$selected_param
      if (is.null(p)) "Select a parameter" else paste("Timeseries:", p)
    })

    # -- Score table: per-feature scores pivoted wide --------------------------
    output$score_table <- DT::renderDataTable({
      res <- rctv_ctas_results()
      shiny::req(res)
      shiny::req(input$selected_param)

      thresh <- input$thresh %||% 1.3
      scores_display <- prepare_score_table(res, input$selected_param)
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

    # -- Timeseries plot -------------------------------------------------------
    output$ts_plot <- shiny::renderPlot({
      df <- rctv_measures()
      shiny::req(df)
      shiny::req(input$selected_param)

      plot_timeseries(input$selected_param, df, thresh = input$thresh)
    }, res = 96)

    # -- Timeseries data table for outlier sites -------------------------------
    output$ts_data_table <- DT::renderDataTable({
      df <- rctv_measures()
      shiny::req(df)
      shiny::req(input$selected_param)

      thresh <- input$thresh %||% 1.3
      ts_data <- prepare_ts_data(df, input$selected_param, thresh)

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
