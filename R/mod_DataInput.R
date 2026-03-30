#' Read a single uploaded file into a data frame
#'
#' Dispatches on file extension: `.csv` uses [utils::read.csv()],
#' `.parquet` uses [arrow::read_parquet()], `.rda`/`.rdata` loads the
#' first data frame found in the file.
#'
#' @param path Path to the temporary uploaded file.
#' @param name Original filename (used for extension detection).
#' @return A data frame, or stops with an informative error.
#' @export
read_upload_file <- function(path, name) {
  ext <- tolower(tools::file_ext(name))

  if (ext == "csv") {
    return(utils::read.csv(path, stringsAsFactors = FALSE))
  }

  if (ext == "parquet") { # nocov start
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required to read parquet files. ",
           "Install it with: install.packages('arrow')", call. = FALSE)
    }
    return(as.data.frame(arrow::read_parquet(path)))
  } # nocov end

  if (ext %in% c("rda", "rdata")) {
    env <- new.env(parent = emptyenv())
    load(path, envir = env)
    objs <- ls(env)
    for (nm in objs) {
      obj <- get(nm, envir = env)
      if (is.data.frame(obj)) return(obj)
    }
    stop("No data frame found in uploaded .rda file '", name, "'.",
         call. = FALSE)
  }

  stop("Unsupported file format '.", ext,
       "'. Please upload .csv, .parquet, or .rda files.", call. = FALSE)
}


#' Data Input Module - UI
#'
#' Provides a dropdown to select sample datasets or upload custom files.
#' Upload mode is selected by default. When uploading, 2 mandatory file
#' inputs (results, input) and 2 optional file inputs (untransformed,
#' queries) are shown with collapsible format documentation.
#'
#' @param id Module namespace ID.
#' @export
mod_DataInput_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Data Source"),
    bslib::card_body(
      shiny::selectInput(
        ns("data_source"),
        "Data Source",
        choices = c(
          "Upload files" = "upload",
          "ctas sample" = "ctas",
          "SDTM sample (pharmaversesdtm)" = "sdtm"
        ),
        selected = "upload"
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'upload'", ns("data_source")),
        shiny::tags$details(
          shiny::tags$summary(
            shiny::tags$strong("File format documentation"),
            style = "cursor:pointer;"
          ),
          shiny::tags$div(
            class = "text-muted small mt-2",
            shiny::tags$p(
              "Upload 2 mandatory + 2 optional flat files ",
              "(.csv, .parquet, or .rda). Each file is a single ",
              "data frame with one row per observation."
            ),
            shiny::tags$h6(shiny::tags$strong("1. Results"), " (mandatory)"),
            shiny::tags$p(
              "Pre-joined site_scores + timeseries from ctas output. ",
              "Required columns: ",
              shiny::tags$code("site"), ", ",
              shiny::tags$code("timeseries_id"), ", ",
              shiny::tags$code("parameter_id"), ", ",
              shiny::tags$code("feature"), ", ",
              shiny::tags$code("fdr_corrected_pvalue_logp"), " (numeric)."
            ),
            shiny::tags$h6(shiny::tags$strong("2. Input"), " (mandatory)"),
            shiny::tags$p(
              "Pre-joined data + subjects + parameters, one row per ",
              "subject/timepoint/parameter. Required columns: ",
              shiny::tags$code("subject_id"), ", ",
              shiny::tags$code("site"), ", ",
              shiny::tags$code("parameter_id"), ", ",
              shiny::tags$code("parameter_name"), ", ",
              shiny::tags$code("parameter_category_1"), ", ",
              shiny::tags$code("parameter_category_2"), " (groups related ",
              "parameters, e.g. same lab test), ",
              shiny::tags$code("parameter_category_3"), " (plot type), ",
              shiny::tags$code("timepoint_1_name"), ", ",
              shiny::tags$code("timepoint_rank"), " (numeric), ",
              shiny::tags$code("result"), " (numeric)."
            ),
            shiny::tags$p(
              shiny::tags$code("parameter_category_3"),
              " determines visualization: ",
              shiny::tags$code("numeric"), " (line plot), ",
              shiny::tags$code("range_normalized"),
              " (lab values normalized to [0,1] by reference range), ",
              shiny::tags$code("ratio_missing"),
              " (proportion of missing values per subject over time, ",
              "values 0\u20131; paired with a ",
              shiny::tags$code("range_normalized"), " or ",
              shiny::tags$code("numeric"),
              " sibling sharing the same ",
              shiny::tags$code("parameter_category_2"), "), ",
              shiny::tags$code("categorical"),
              " (alluvial plot), ",
              shiny::tags$code("bar"),
              " (single-timepoint bar chart)."
            ),
            shiny::tags$p(
              "Optional column: ",
              shiny::tags$code("study"),
              " \u2014 when present with >1 unique value, ",
              "a study filter appears in the Fields panel."
            ),
            shiny::tags$h6(
              shiny::tags$strong("3. Untransformed"), " (optional)"
            ),
            shiny::tags$p(
              "Original pre-transformation values shown alongside ",
              "transformed data in the Source Data table. Joined on ",
              shiny::tags$code("(subject_id, parameter_category_2, ",
                               "timepoint_1_name)"),
              ". Optional value columns: ",
              shiny::tags$code("original_value"), ", ",
              shiny::tags$code("lower"), ", ",
              shiny::tags$code("upper"), ", ",
              shiny::tags$code("original_category"), "."
            ),
            shiny::tags$h6(
              shiny::tags$strong("4. Queries"), " (optional)"
            ),
            shiny::tags$p(
              "Clinical query records overlaid as dots on plots. ",
              "Required columns: ",
              shiny::tags$code("subject_id"), ", ",
              shiny::tags$code("parameter_id"), ", ",
              shiny::tags$code("visit"), " (matched to timepoint_1_name), ",
              shiny::tags$code("data_change"),
              " (logical TRUE/FALSE). ",
              "Display columns: ",
              shiny::tags$code("domain"), ", ",
              shiny::tags$code("field"), ", ",
              shiny::tags$code("query_status"), ", ",
              shiny::tags$code("query_type"), ", ",
              shiny::tags$code("query_text"), ", ",
              shiny::tags$code("query_answer"), ", ",
              shiny::tags$code("value_first_entry"), ", ",
              shiny::tags$code("value_now"), "."
            )
          )
        ),
        shiny::tags$br(),
        shiny::fileInput(ns("file_results"), "Results file",
                         accept = c(".csv", ".parquet", ".rda", ".rdata")),
        shiny::fileInput(ns("file_input"), "Input data file",
                         accept = c(".csv", ".parquet", ".rda", ".rdata")),
        shiny::fileInput(ns("file_untransformed"),
                         "Untransformed data (optional)",
                         accept = c(".csv", ".parquet", ".rda", ".rdata")),
        shiny::fileInput(ns("file_queries"), "Query data (optional)",
                         accept = c(".csv", ".parquet", ".rda", ".rdata"))
      ),
      shiny::actionButton(
        ns("load_data"),
        "Load Data",
        class = "btn-primary",
        icon = shiny::icon("database")
      ),
      shiny::hr(),
      shiny::uiOutput(ns("status"))
    )
  )
}

#' Data Input Module - Server
#'
#' Returns a named list of reactives: `measures`, `ctas_results`,
#' `untransformed`, `queries`, `dataset_label`, and `studies`.
#' No study filtering is applied here; the Fields module owns filtering.
#'
#' @param id Module namespace ID.
#' @return Named list of reactive expressions.
#' @export
mod_DataInput_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    rv_measures <- shiny::reactiveVal(NULL)
    rv_ctas_results <- shiny::reactiveVal(NULL)
    rv_untransformed <- shiny::reactiveVal(NULL)
    rv_queries <- shiny::reactiveVal(NULL)
    rv_dataset_label <- shiny::reactiveVal(NULL)
    rv_studies <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$load_data, {
      source <- input$data_source %||% "upload"

      if (source == "ctas") {
        ctas_data <- ctasapp::sample_ctas_data
        ctas_results <- ctasapp::sample_ctas_results
        label <- "ctas sample"
      } else if (source == "sdtm") {
        ctas_data <- ctasapp::sample_sdtm_data
        ctas_results <- ctasapp::sample_sdtm_results
        label <- "SDTM sample"
      } else { # nocov start
        res_file <- input$file_results
        inp_file <- input$file_input

        if (is.null(res_file) || is.null(inp_file)) {
          shiny::showNotification(
            "Please upload both a Results file and an Input file.",
            type = "error", duration = 5
          )
          return()
        }

        results_df <- tryCatch(
          read_upload_file(res_file$datapath, res_file$name),
          error = function(e) { NULL }
        )
        input_df <- tryCatch(
          read_upload_file(inp_file$datapath, inp_file$name),
          error = function(e) { NULL }
        )

        if (is.null(results_df)) {
          shiny::showNotification(
            paste0("Could not read results file '", res_file$name,
                   "'. Check the file format."),
            type = "error", duration = 8
          )
          return()
        }
        if (is.null(input_df)) {
          shiny::showNotification(
            paste0("Could not read input file '", inp_file$name,
                   "'. Check the file format."),
            type = "error", duration = 8
          )
          return()
        }

        errs_res <- validate_upload_results(results_df)
        errs_inp <- validate_upload_input(input_df)
        all_errs <- c(errs_res, errs_inp)

        ut_file <- input$file_untransformed
        untransformed_df <- NULL
        if (!is.null(ut_file)) {
          untransformed_df <- tryCatch(
            read_upload_file(ut_file$datapath, ut_file$name),
            error = function(e) { NULL }
          )
          if (!is.null(untransformed_df)) {
            errs_ut <- validate_upload_untransformed(untransformed_df)
            all_errs <- c(all_errs, errs_ut)
          }
        }

        q_file <- input$file_queries
        queries_df <- NULL
        if (!is.null(q_file)) {
          queries_df <- tryCatch(
            read_upload_file(q_file$datapath, q_file$name),
            error = function(e) { NULL }
          )
          if (!is.null(queries_df)) {
            errs_q <- validate_upload_queries(queries_df)
            all_errs <- c(all_errs, errs_q)
          }
        }

        if (length(all_errs) > 0) {
          shiny::showNotification(
            htmltools::HTML(paste(all_errs, collapse = "<br>")),
            type = "error", duration = 12
          )
          return()
        }

        cross_warns <- validate_upload_crossfile(input_df, results_df)
        if (length(cross_warns) > 0) {
          shiny::showNotification(
            htmltools::HTML(paste("Warnings:", paste(cross_warns,
                                                     collapse = "<br>"))),
            type = "warning", duration = 10
          )
        }

        reconstructed <- reconstruct_from_upload(
          input_df, results_df, untransformed_df, queries_df
        )
        ctas_data <- reconstructed$ctas_data
        ctas_results <- reconstructed$ctas_results
        label <- tools::file_path_sans_ext(inp_file$name)
      } # nocov end

      measures <- prepare_measures(ctas_data, ctas_results)
      rv_measures(measures)
      rv_ctas_results(ctas_results)
      rv_untransformed(ctas_data$untransformed)
      rv_queries(ctas_data$queries)
      rv_dataset_label(label)

      studies <- if ("study" %in% names(ctas_data$subjects)) {
        sort(unique(ctas_data$subjects$study)) # nocov
      } else {
        NULL
      }
      rv_studies(if (!is.null(studies) && length(studies) > 1) studies else NULL)

      shiny::showNotification(
        paste0("Loaded '", label, "': ", nrow(measures), " observations"),
        type = "message", duration = 3
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
      ctas_results = rv_ctas_results,
      untransformed = rv_untransformed,
      queries = rv_queries,
      dataset_label = rv_dataset_label,
      studies = rv_studies
    )
  })
}
