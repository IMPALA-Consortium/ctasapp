#' Run the ctas Shiny App
#'
#' Launches the interactive ctas visualization application.
#'
#' @param ... Additional arguments passed to [shiny::shinyApp()].
#' @export
run_ctas_app <- function(...) {

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
      "field_detail", data$measures, data$ctas_results, data$untransformed
    )
  }

  shiny::shinyApp(ui, server, ...)
}
