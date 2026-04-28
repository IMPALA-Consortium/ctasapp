#' Run the ctas Shiny App
#'
#' Launches the interactive ctas visualization application.
#'
#' @param config Path to a YAML configuration file. When `NULL` (default),
#'   uses the config shipped with the package. See [load_config()] for the
#'   expected structure.
#' @param ... Additional arguments passed to [shiny::shinyApp()].
#' @export
run_ctas_app <- function(config = NULL, ...) {
  cfg <- load_config(config)
  apply_config(cfg)

  options(shiny.maxRequestSize = 100 * 1024^2)

  ui <- bslib::page_navbar(
    title = "ctas",
    fillable = FALSE,
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
    bslib::nav_panel(
      title = "Data",
      icon = shiny::icon("database"),
      mod_DataInput_ui("data_input")
    ),
    bslib::nav_panel(
      title = "Fields",
      icon = shiny::icon("chart-line"),
      mod_FieldDetail_ui("field_detail")
    ),
    bslib::nav_spacer(),
    bslib::nav_item(
      shiny::uiOutput("dataset_badge")
    ),
    bslib::nav_item(
      shiny::div(
        style = "min-width:250px;",
        shiny::selectizeInput(
          "global_site_filter",
          label = NULL,
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "Filter sites...")
        )
      )
    )
  )

  server <- function(input, output, session) {
    data <- mod_DataInput_server("data_input")

    # Populate global site filter when data loads
    shiny::observeEvent(data$measures(), {
      m <- data$measures()
      if (is.null(m)) return()
      sites <- sort(unique(m$site))
      shiny::updateSelectizeInput(
        session, "global_site_filter",
        choices = sites,
        selected = character(0),
        server = FALSE
      )
    })

    # Show dataset label and study in navbar
    output$dataset_badge <- shiny::renderUI({
      label <- data$dataset_label()
      if (is.null(label)) return(NULL)
      study <- data$selected_study()
      text <- if (!is.null(study)) paste(label, study, sep = " \u00B7 ") else label
      shiny::tags$span(
        class = "badge bg-info text-white",
        style = "font-size: 0.85em;",
        text
      )
    })

    rctv_selected_sites <- shiny::reactive({
      sel <- input$global_site_filter
      if (is.null(sel) || length(sel) == 0) return(NULL)
      sel
    })

    mod_FieldDetail_server(
      "field_detail", data$measures, data$ctas_results, data$untransformed,
      data$queries, data$dataset_label, data$studies,
      rctv_selected_sites = rctv_selected_sites
    )
  }

  shiny::shinyApp(ui, server, ...)
}
