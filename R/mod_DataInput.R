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
      message("[DEBUG] === Load Data button clicked ===")
      source <- input$data_source %||% "upload"
      message("[DEBUG] data_source = '", source, "'")

      shiny::withProgress(message = "Loading data...", value = 0, {

      if (source == "ctas") {
        shiny::setProgress(0.1, detail = "Loading ctas sample")
        message("[DEBUG] Loading ctas sample data...")
        ctas_data <- ctasapp::sample_ctas_data
        ctas_results <- ctasapp::sample_ctas_results
        label <- "ctas sample"
        message("[DEBUG] ctas sample loaded OK")
      } else if (source == "sdtm") {
        shiny::setProgress(0.1, detail = "Loading SDTM sample")
        message("[DEBUG] Loading SDTM sample data...")
        ctas_data <- ctasapp::sample_sdtm_data
        ctas_results <- ctasapp::sample_sdtm_results
        label <- "SDTM sample"
        message("[DEBUG] SDTM sample loaded OK")
      } else { # nocov start
        message("[DEBUG] Upload mode: reading uploaded files...")
        res_file <- input$file_results
        inp_file <- input$file_input
        message("[DEBUG] res_file is.null=", is.null(res_file),
                ", inp_file is.null=", is.null(inp_file))

        if (is.null(res_file) || is.null(inp_file)) {
          message("[DEBUG] Missing mandatory file(s), aborting")
          shiny::showNotification(
            "Please upload both a Results file and an Input file.",
            type = "error", duration = 5
          )
          return()
        }

        shiny::setProgress(0.1, detail = "Reading uploaded files")
        message("[DEBUG] Reading results file: ", res_file$name,
                " (", res_file$datapath, ")")
        results_df <- tryCatch(
          read_upload_file(res_file$datapath, res_file$name),
          error = function(e) { e }
        )
        message("[DEBUG] Reading input file: ", inp_file$name,
                " (", inp_file$datapath, ")")
        input_df <- tryCatch(
          read_upload_file(inp_file$datapath, inp_file$name),
          error = function(e) { e }
        )

        if (inherits(results_df, "error")) {
          message("[DEBUG] ERROR reading results: ",
                  conditionMessage(results_df))
          shiny::showNotification(
            paste0("Could not read results file '", res_file$name,
                   "': ", conditionMessage(results_df)),
            type = "error", duration = 8
          )
          return()
        }
        message("[DEBUG] results_df: ", nrow(results_df), " rows, ",
                ncol(results_df), " cols: ",
                paste(names(results_df), collapse = ", "))

        if (inherits(input_df, "error")) {
          message("[DEBUG] ERROR reading input: ",
                  conditionMessage(input_df))
          shiny::showNotification(
            paste0("Could not read input file '", inp_file$name,
                   "': ", conditionMessage(input_df)),
            type = "error", duration = 8
          )
          return()
        }
        message("[DEBUG] input_df: ", nrow(input_df), " rows, ",
                ncol(input_df), " cols: ",
                paste(names(input_df), collapse = ", "))

        shiny::setProgress(0.2, detail = "Validating files")
        message("[DEBUG] Validating uploaded files...")
        errs_res <- validate_upload_results(results_df)
        errs_inp <- validate_upload_input(input_df)
        all_errs <- c(errs_res, errs_inp)
        message("[DEBUG] Validation errors so far: ", length(all_errs))

        ut_file <- input$file_untransformed
        untransformed_df <- NULL
        if (!is.null(ut_file)) {
          message("[DEBUG] Reading untransformed file: ", ut_file$name)
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
          message("[DEBUG] Reading queries file: ", q_file$name)
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
          message("[DEBUG] Validation failed: ",
                  paste(all_errs, collapse = "; "))
          shiny::showNotification(
            htmltools::HTML(paste(all_errs, collapse = "<br>")),
            type = "error", duration = 12
          )
          return()
        }
        message("[DEBUG] Validation passed")

        # Filter out ratio_missing entries paired with categorical/bar params
        # or where ratio_missing is the only type in the category_2 group
        if ("parameter_category_2" %in% names(input_df) &&
            "parameter_category_3" %in% names(input_df)) {
          cat2_with_catbar <- unique(
            input_df$parameter_category_2[
              input_df$parameter_category_3 %in% c("categorical", "bar")
            ]
          )
          cat2_types <- tapply(
            input_df$parameter_category_3,
            input_df$parameter_category_2,
            function(x) unique(x)
          )
          cat2_only_rm <- names(cat2_types)[
            vapply(cat2_types, function(x) {
              length(x) == 1L && x == "ratio_missing"
            }, logical(1))
          ]
          rm_ids <- unique(
            input_df$parameter_id[
              input_df$parameter_category_3 == "ratio_missing" &
              (input_df$parameter_category_2 %in% cat2_with_catbar |
               input_df$parameter_category_2 %in% cat2_only_rm)
            ]
          )
          if (length(rm_ids) > 0) {
            message("[DEBUG] Removing ", length(rm_ids),
                    " ratio_missing parameter_id(s) paired with categorical")
            input_df <- input_df[!input_df$parameter_id %in% rm_ids, ]
            results_df <- results_df[!results_df$parameter_id %in% rm_ids, ]
            shiny::showNotification(
              "Missing Ratios timelines for categorical values not supported",
              type = "warning", duration = 8
            )
          }
        }

        shiny::setProgress(0.3, detail = paste0(
          "Aggregating results (", nrow(results_df), " rows)..."
        ))
        message("[DEBUG] Aggregating results...")
        results_df <- tryCatch(
          aggregate_results(results_df),
          error = function(e) { e }
        )
        if (inherits(results_df, "error")) {
          message("[DEBUG] ERROR in aggregate_results: ",
                  conditionMessage(results_df))
          shiny::showNotification(
            paste0("Error aggregating results: ",
                   conditionMessage(results_df)),
            type = "error", duration = 8
          )
          return()
        }
        message("[DEBUG] Aggregated results: ", nrow(results_df), " rows")

        shiny::setProgress(0.6, detail = "Cross-validating files")
        cross_warns <- validate_upload_crossfile(input_df, results_df)
        if (length(cross_warns) > 0) {
          message("[DEBUG] Cross-file warnings: ",
                  paste(cross_warns, collapse = "; "))
          shiny::showNotification(
            htmltools::HTML(paste("Warnings:", paste(cross_warns,
                                                     collapse = "<br>"))),
            type = "warning", duration = 10
          )
        }

        shiny::setProgress(0.7, detail = "Reconstructing data structures")
        message("[DEBUG] Reconstructing from upload...")
        reconstructed <- tryCatch(
          reconstruct_from_upload(
            input_df, results_df, untransformed_df, queries_df
          ),
          error = function(e) { e }
        )
        if (inherits(reconstructed, "error")) {
          message("[DEBUG] ERROR in reconstruct_from_upload: ",
                  conditionMessage(reconstructed))
          shiny::showNotification(
            paste0("Error processing uploaded data: ",
                   conditionMessage(reconstructed)),
            type = "error", duration = 8
          )
          return()
        }
        ctas_data <- reconstructed$ctas_data
        ctas_results <- reconstructed$ctas_results
        label <- tools::file_path_sans_ext(inp_file$name)
        message("[DEBUG] Reconstruction OK, label='", label, "'")
      } # nocov end

      shiny::setProgress(0.8, detail = "Preparing measures")
      message("[DEBUG] Preparing measures...")
      measures <- tryCatch(
        prepare_measures(ctas_data, ctas_results),
        error = function(e) { e }
      )
      if (inherits(measures, "error")) {
        message("[DEBUG] ERROR in prepare_measures: ",
                conditionMessage(measures))
        shiny::showNotification(
          paste0("Error preparing measures: ",
                 conditionMessage(measures)),
          type = "error", duration = 8
        )
        return()
      }
      message("[DEBUG] measures: ", nrow(measures), " rows, ",
              ncol(measures), " cols")

      shiny::setProgress(0.95, detail = "Finalizing")
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

      shiny::setProgress(1, detail = "Done")
      message("[DEBUG] === Load complete: '", label, "' with ",
              nrow(measures), " observations ===")
      shiny::showNotification(
        paste0("Loaded '", label, "': ", nrow(measures), " observations"),
        type = "message", duration = 3
      )

      }) # end withProgress
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
