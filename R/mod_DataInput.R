#' Data Input Module - UI
#'
#' Lets the user choose between the bundled ctas sample data and the
#' pharmaversesdtm-derived SDTM sample data. Upload support is reserved
#' for a future version.
#'
#' @param id Module namespace ID.
#' @export
mod_DataInput_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Data Source"),
    bslib::card_body(
      shiny::p("Select a sample dataset to explore, or upload your own data."),
      shiny::radioButtons(
        ns("dataset_choice"),
        "Sample Dataset",
        choices = c(
          "ctas sample" = "ctas",
          "SDTM sample (pharmaversesdtm)" = "sdtm"
        ),
        selected = "ctas"
      ),
      shiny::actionButton(
        ns("load_sample"),
        "Load Selected Data",
        class = "btn-primary",
        icon = shiny::icon("database")
      ),
      shiny::hr(),
      shiny::p(
        shiny::icon("info-circle"),
        "Upload functionality will be added in a future version.",
        style = "color: #888;"
      ),
      shiny::uiOutput(ns("status"))
    )
  )
}

#' Data Input Module - Server
#'
#' Returns a named list of two reactives: `measures` (the prepared data frame)
#' and `ctas_results` (the raw ctas output list for per-feature score access).
#'
#' @param id Module namespace ID.
#' @return Named list with `measures` and `ctas_results` reactive expressions.
#' @export
mod_DataInput_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    rv_measures <- shiny::reactiveVal(NULL)
    rv_ctas_results <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$load_sample, {
      choice <- input$dataset_choice %||% "ctas"

      if (choice == "sdtm") {
        ctas_data <- ctasapp::sample_sdtm_data
        ctas_results <- ctasapp::sample_sdtm_results
        label <- "SDTM sample"
      } else {
        ctas_data <- ctasapp::sample_ctas_data
        ctas_results <- ctasapp::sample_ctas_results
        label <- "ctas sample"
      }

      measures <- prepare_measures(ctas_data, ctas_results)
      rv_measures(measures)
      rv_ctas_results(ctas_results)
      shiny::showNotification(
        paste0("Loaded ", label, " data: ", nrow(measures), " observations"),
        type = "message",
        duration = 3
      )
    })

    output$status <- shiny::renderUI({
      if (!is.null(rv_measures())) {
        df <- rv_measures()
        n_params <- length(unique(df$parameter_id))
        n_sites <- length(unique(df$site))
        n_subj <- length(unique(df$subject_id))
        htmltools::tagList(
          shiny::tags$div(
            class = "alert alert-success",
            shiny::tags$strong("Data loaded: "),
            paste0(
              n_params, " parameters, ",
              n_sites, " sites, ",
              n_subj, " subjects, ",
              nrow(df), " observations"
            )
          )
        )
      }
    })

    list(
      measures = rv_measures,
      ctas_results = rv_ctas_results
    )
  })
}
