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
    )
  )

  server <- function(input, output, session) {
    data <- mod_DataInput_server("data_input")
    mod_FieldDetail_server(
      "field_detail", data$measures, data$ctas_results, data$untransformed,
      data$queries, data$dataset_label, data$studies
    )
  }

  shiny::shinyApp(ui, server, ...)
}
