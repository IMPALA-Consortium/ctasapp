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
#' When the `embedded:` block is set in `ctasapp.yml`
#' (see [load_config()] / [get_embedded_paths()]), an additional
#' "Embedded data set" entry is added to the dropdown that loads the
#' configured files through the same pipeline as uploads.
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
        condition = sprintf("input['%s'] == 'embedded'", ns("data_source")),
        shiny::uiOutput(ns("embedded_panel"))
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
            ),
            shiny::tags$h6(
              shiny::tags$strong("5. Protocol Deviations"), " (optional)"
            ),
            shiny::tags$p(
              "SDTM DV-style records shown in the Protocol Deviations ",
              "tab of Field Detail. Deviations link at site level only. ",
              "Required column: ",
              shiny::tags$code("site"), ". ",
              "Recommended columns: ",
              shiny::tags$code("subject_id"), ", ",
              shiny::tags$code("dv_seq"), ", ",
              shiny::tags$code("dv_term"), ", ",
              shiny::tags$code("dv_decod"), ", ",
              shiny::tags$code("dv_cat"), ", ",
              shiny::tags$code("dv_scat"), ", ",
              shiny::tags$code("dv_start_date"), ", ",
              shiny::tags$code("dv_end_date"), "."
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
                         accept = c(".csv", ".parquet", ".rda", ".rdata")),
        shiny::fileInput(ns("file_pd"),
                         "Protocol Deviations (optional)",
                         accept = c(".csv", ".parquet", ".rda", ".rdata"))
      ),
      shiny::uiOutput(ns("study_selector")),
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
#' `untransformed`, `queries`, `pd`, `dataset_label`, `studies`, and
#' `selected_study`. When the uploaded results file contains a `study`
#' column with more than one unique value, a study selector is shown on
#' the Data tab and the chosen study is used to filter both the results
#' and input data frames before validation/reconstruction.
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
    rv_pd <- shiny::reactiveVal(NULL)
    rv_dataset_label <- shiny::reactiveVal(NULL)
    rv_studies <- shiny::reactiveVal(NULL)
    rv_available_studies <- shiny::reactiveVal(NULL)
    rv_selected_study <- shiny::reactiveVal(NULL)

    # -- Inject "Embedded data set" option when configured in ctasapp.yml -----
    shiny::observe({
      if (!embedded_files_configured()) return()
      shiny::updateSelectInput(
        session, "data_source",
        choices = c(
          "Upload files" = "upload",
          "Embedded data set" = "embedded",
          "ctas sample" = "ctas",
          "SDTM sample (pharmaversesdtm)" = "sdtm"
        ),
        selected = shiny::isolate(input$data_source) %||% "upload"
      )
    })

    # -- Embedded panel: show resolved paths with existence markers -----------
    output$embedded_panel <- shiny::renderUI({ # nocov start
      paths <- get_embedded_paths()
      if (is.null(paths)) {
        return(shiny::tags$p(
          class = "text-muted small",
          "No embedded dataset configured."
        ))
      }
      labels <- list(
        results = "Results", input = "Input",
        untransformed = "Untransformed (optional)",
        queries = "Queries (optional)",
        pd = "Protocol Deviations (optional)"
      )
      rows <- lapply(names(labels), function(key) {
        p <- paths[[key]]
        if (is.null(p) || !nzchar(p)) {
          return(shiny::tags$div(
            class = "text-muted small",
            shiny::tags$strong(paste0(labels[[key]], ": ")),
            shiny::tags$em("not set")
          ))
        }
        ok <- file.exists(p)
        shiny::tags$div(
          class = "small",
          shiny::tags$span(
            style = paste0("color:", if (ok) "#2e7d32" else "#c62828",
                           ";font-weight:bold;"),
            if (ok) "\u2713" else "\u2717"
          ),
          " ",
          shiny::tags$strong(paste0(labels[[key]], ": ")),
          shiny::tags$code(p)
        )
      })
      shiny::tags$div(
        class = "mb-2",
        shiny::tags$p(
          class = "text-muted small mb-1",
          "Files configured in ", shiny::tags$code("ctasapp.yml"),
          ":"
        ),
        rows
      )
    }) # nocov end

    # -- Detect studies from results file when uploaded -----------------------
    shiny::observeEvent(input$file_results, { # nocov start
      res_file <- input$file_results
      if (is.null(res_file)) return()
      results_df <- tryCatch(
        read_upload_file(res_file$datapath, res_file$name),
        error = function(e) { NULL }
      )
      if (is.null(results_df) || !"study" %in% names(results_df)) {
        rv_available_studies(NULL)
        return()
      }
      studies <- sort(unique(results_df$study))
      if (length(studies) > 1) {
        rv_available_studies(studies)
      } else {
        rv_available_studies(NULL)
      }
    }) # nocov end

    # -- Detect studies from embedded results file when selected --------------
    shiny::observeEvent(input$data_source, { # nocov start
      if (input$data_source != "embedded") return()
      paths <- get_embedded_paths()
      if (is.null(paths) || is.null(paths$results) ||
          !file.exists(paths$results)) {
        rv_available_studies(NULL)
        return()
      }
      results_df <- tryCatch(
        read_upload_file(paths$results, basename(paths$results)),
        error = function(e) { NULL }
      )
      if (is.null(results_df) || !"study" %in% names(results_df)) {
        rv_available_studies(NULL)
        return()
      }
      studies <- sort(unique(results_df$study))
      if (length(studies) > 1) {
        rv_available_studies(studies)
      } else {
        rv_available_studies(NULL)
      }
    }) # nocov end

    output$study_selector <- shiny::renderUI({ # nocov start
      studies <- rv_available_studies()
      src <- input$data_source %||% "upload"
      if (is.null(studies) || !src %in% c("upload", "embedded")) return(NULL)
      shiny::selectInput(
        session$ns("upload_study"),
        "Select Study",
        choices = stats::setNames(studies, studies),
        selected = studies[1]
      )
    }) # nocov end

    # -- Shared pipeline: validate -> aggregate -> reconstruct ----------------
    # Used by both the upload and embedded branches. Returns a list with
    # `ctas_data` and `ctas_results`, or NULL when an error/notification was
    # shown and the caller should abort. Source label ("upload" / "embedded")
    # is only used to tweak user-facing messages.
    process_loaded_frames <- function(results_df, input_df,             # nocov start
                                      untransformed_df = NULL,
                                      queries_df = NULL,
                                      pd_df = NULL,
                                      source_label = "upload") {
      shiny::setProgress(0.2, detail = "Validating files")
      ctas_log("Validating ", source_label, " files...")
      errs_res <- validate_upload_results(results_df)
      errs_inp <- validate_upload_input(input_df)
      all_errs <- c(errs_res, errs_inp)
      if (!is.null(untransformed_df)) {
        all_errs <- c(all_errs, validate_upload_untransformed(untransformed_df))
      }
      if (!is.null(queries_df)) {
        all_errs <- c(all_errs, validate_upload_queries(queries_df))
      }
      if (!is.null(pd_df)) {
        all_errs <- c(all_errs, validate_upload_pd(pd_df))
      }
      if (length(all_errs) > 0) {
        ctas_log("Validation failed: ", paste(all_errs, collapse = "; "))
        shiny::showNotification(
          htmltools::HTML(paste(all_errs, collapse = "<br>")),
          type = "error", duration = 12
        )
        return(NULL)
      }
      ctas_log("Validation passed")

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
          ctas_log("Removing ", length(rm_ids),
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
      ctas_log("Aggregating results...")
      results_df <- tryCatch(
        aggregate_results(results_df),
        error = function(e) { e }
      )
      if (inherits(results_df, "error")) {
        ctas_log("ERROR in aggregate_results: ",
                conditionMessage(results_df))
        shiny::showNotification(
          paste0("Error aggregating results: ",
                 conditionMessage(results_df)),
          type = "error", duration = 8
        )
        return(NULL)
      }
      ctas_log("Aggregated results: ", nrow(results_df), " rows")

      shiny::setProgress(0.6, detail = "Cross-validating files")
      cross_warns <- validate_upload_crossfile(input_df, results_df)
      if (length(cross_warns) > 0) {
        ctas_log("Cross-file warnings: ",
                paste(cross_warns, collapse = "; "))
        shiny::showNotification(
          htmltools::HTML(paste("Warnings:", paste(cross_warns,
                                                   collapse = "<br>"))),
          type = "warning", duration = 10
        )
      }

      shiny::setProgress(0.7, detail = "Reconstructing data structures")
      ctas_log("Reconstructing from ", source_label, "...")
      reconstructed <- tryCatch(
        reconstruct_from_upload(
          input_df, results_df, untransformed_df, queries_df, pd_df
        ),
        error = function(e) { e }
      )
      if (inherits(reconstructed, "error")) {
        ctas_log("ERROR in reconstruct_from_upload: ",
                conditionMessage(reconstructed))
        shiny::showNotification(
          paste0("Error processing ", source_label, " data: ",
                 conditionMessage(reconstructed)),
          type = "error", duration = 8
        )
        return(NULL)
      }
      reconstructed
    } # nocov end

    shiny::observeEvent(input$load_data, {
      ctas_log("=== Load Data button clicked ===")
      source <- input$data_source %||% "upload"
      ctas_log("data_source = '", source, "'")

      shiny::withProgress(message = "Loading data...", value = 0, {

      if (source == "ctas") {
        shiny::setProgress(0.1, detail = "Loading ctas sample")
        ctas_log("Loading ctas sample data...")
        ctas_data <- ctasapp::sample_ctas_data
        ctas_results <- ctasapp::sample_ctas_results
        label <- "ctas sample"
        ctas_log("ctas sample loaded OK")
      } else if (source == "sdtm") {
        shiny::setProgress(0.1, detail = "Loading SDTM sample")
        ctas_log("Loading SDTM sample data...")
        ctas_data <- ctasapp::sample_sdtm_data
        ctas_results <- ctasapp::sample_sdtm_results
        label <- "SDTM sample"
        ctas_log("SDTM sample loaded OK")
      } else if (source == "embedded") { # nocov start
        ctas_log("Embedded mode: reading configured files...")
        paths <- get_embedded_paths()
        if (is.null(paths)) {
          shiny::showNotification(
            "Embedded data set is not configured.",
            type = "error", duration = 5
          )
          return()
        }

        # Check that all configured files exist on disk; surface missing
        # paths in a single error so operators can fix the deployment.
        missing <- character()
        for (key in c("results", "input", "untransformed", "queries", "pd")) {
          p <- paths[[key]]
          if (!is.null(p) && nzchar(p) && !file.exists(p)) {
            missing <- c(missing, paste0(key, ": ", p))
          }
        }
        if (length(missing) > 0) {
          ctas_log("Missing embedded file(s): ",
                  paste(missing, collapse = "; "))
          shiny::showNotification(
            htmltools::HTML(paste0(
              "Configured embedded file(s) not found:<br>",
              paste(missing, collapse = "<br>")
            )),
            type = "error", duration = 12
          )
          return()
        }

        shiny::setProgress(0.1, detail = "Reading embedded files")
        results_df <- tryCatch(
          read_upload_file(paths$results, basename(paths$results)),
          error = function(e) { e }
        )
        if (inherits(results_df, "error")) {
          shiny::showNotification(
            paste0("Could not read results file '", paths$results,
                   "': ", conditionMessage(results_df)),
            type = "error", duration = 8
          )
          return()
        }
        input_df <- tryCatch(
          read_upload_file(paths$input, basename(paths$input)),
          error = function(e) { e }
        )
        if (inherits(input_df, "error")) {
          shiny::showNotification(
            paste0("Could not read input file '", paths$input,
                   "': ", conditionMessage(input_df)),
            type = "error", duration = 8
          )
          return()
        }

        untransformed_df <- NULL
        if (!is.null(paths$untransformed) && nzchar(paths$untransformed)) {
          untransformed_df <- tryCatch(
            read_upload_file(paths$untransformed,
                             basename(paths$untransformed)),
            error = function(e) { NULL }
          )
        }
        queries_df <- NULL
        if (!is.null(paths$queries) && nzchar(paths$queries)) {
          queries_df <- tryCatch(
            read_upload_file(paths$queries, basename(paths$queries)),
            error = function(e) { NULL }
          )
        }
        pd_df <- NULL
        if (!is.null(paths$pd) && nzchar(paths$pd)) {
          pd_df <- tryCatch(
            read_upload_file(paths$pd, basename(paths$pd)),
            error = function(e) { NULL }
          )
        }

        # -- Filter to selected study when multi-study data ------------------
        upload_study <- input$upload_study
        if (!is.null(upload_study) && !is.null(rv_available_studies())) {
          ctas_log("Filtering embedded data to study: ", upload_study)
          if ("study" %in% names(results_df)) {
            results_df <- results_df[results_df$study == upload_study, ]
          }
          if ("study" %in% names(input_df)) {
            input_df <- input_df[input_df$study == upload_study, ]
          }
          rv_selected_study(upload_study)
        } else {
          rv_selected_study(NULL)
        }

        reconstructed <- process_loaded_frames(
          results_df, input_df, untransformed_df, queries_df, pd_df,
          source_label = "embedded"
        )
        if (is.null(reconstructed)) return()
        ctas_data <- reconstructed$ctas_data
        ctas_results <- reconstructed$ctas_results
        label <- tools::file_path_sans_ext(basename(paths$input))
        ctas_log("Embedded load OK, label='", label, "'")
        # nocov end
      } else { # nocov start
        ctas_log("Upload mode: reading uploaded files...")
        res_file <- input$file_results
        inp_file <- input$file_input
        ctas_log("res_file is.null=", is.null(res_file),
                ", inp_file is.null=", is.null(inp_file))

        if (is.null(res_file) || is.null(inp_file)) {
          ctas_log("Missing mandatory file(s), aborting")
          shiny::showNotification(
            "Please upload both a Results file and an Input file.",
            type = "error", duration = 5
          )
          return()
        }

        shiny::setProgress(0.1, detail = "Reading uploaded files")
        ctas_log("Reading results file: ", res_file$name,
                " (", res_file$datapath, ")")
        results_df <- tryCatch(
          read_upload_file(res_file$datapath, res_file$name),
          error = function(e) { e }
        )
        ctas_log("Reading input file: ", inp_file$name,
                " (", inp_file$datapath, ")")
        input_df <- tryCatch(
          read_upload_file(inp_file$datapath, inp_file$name),
          error = function(e) { e }
        )

        if (inherits(results_df, "error")) {
          ctas_log("ERROR reading results: ",
                  conditionMessage(results_df))
          shiny::showNotification(
            paste0("Could not read results file '", res_file$name,
                   "': ", conditionMessage(results_df)),
            type = "error", duration = 8
          )
          return()
        }
        ctas_log("results_df: ", nrow(results_df), " rows, ",
                ncol(results_df), " cols: ",
                paste(names(results_df), collapse = ", "))

        if (inherits(input_df, "error")) {
          ctas_log("ERROR reading input: ",
                  conditionMessage(input_df))
          shiny::showNotification(
            paste0("Could not read input file '", inp_file$name,
                   "': ", conditionMessage(input_df)),
            type = "error", duration = 8
          )
          return()
        }
        ctas_log("input_df: ", nrow(input_df), " rows, ",
                ncol(input_df), " cols: ",
                paste(names(input_df), collapse = ", "))

        # -- Filter to selected study when multi-study data -------------------
        upload_study <- input$upload_study
        if (!is.null(upload_study) && !is.null(rv_available_studies())) {
          ctas_log("Filtering to study: ", upload_study)
          if ("study" %in% names(results_df)) {
            results_df <- results_df[results_df$study == upload_study, ]
          }
          if ("study" %in% names(input_df)) {
            input_df <- input_df[input_df$study == upload_study, ]
          }
          rv_selected_study(upload_study)
          ctas_log("After study filter: results_df ",
                  nrow(results_df), " rows, input_df ",
                  nrow(input_df), " rows")
        } else {
          rv_selected_study(NULL)
        }

        ut_file <- input$file_untransformed
        untransformed_df <- NULL
        if (!is.null(ut_file)) {
          ctas_log("Reading untransformed file: ", ut_file$name)
          untransformed_df <- tryCatch(
            read_upload_file(ut_file$datapath, ut_file$name),
            error = function(e) { NULL }
          )
        }

        q_file <- input$file_queries
        queries_df <- NULL
        if (!is.null(q_file)) {
          ctas_log("Reading queries file: ", q_file$name)
          queries_df <- tryCatch(
            read_upload_file(q_file$datapath, q_file$name),
            error = function(e) { NULL }
          )
        }

        pd_file <- input$file_pd
        pd_df <- NULL
        if (!is.null(pd_file)) {
          ctas_log("Reading PD file: ", pd_file$name)
          pd_df <- tryCatch(
            read_upload_file(pd_file$datapath, pd_file$name),
            error = function(e) { NULL }
          )
        }

        reconstructed <- process_loaded_frames(
          results_df, input_df, untransformed_df, queries_df, pd_df,
          source_label = "upload"
        )
        if (is.null(reconstructed)) return()
        ctas_data <- reconstructed$ctas_data
        ctas_results <- reconstructed$ctas_results
        label <- tools::file_path_sans_ext(inp_file$name)
        ctas_log("Reconstruction OK, label='", label, "'")
      } # nocov end

      shiny::setProgress(0.8, detail = "Preparing measures")
      ctas_log("Preparing measures...")
      measures <- tryCatch( # nocov start
        prepare_measures(ctas_data, ctas_results),
        error = function(e) { e }
      )
      if (inherits(measures, "error")) {
        ctas_log("ERROR in prepare_measures: ",
                conditionMessage(measures))
        shiny::showNotification(
          paste0("Error preparing measures: ",
                 conditionMessage(measures)),
          type = "error", duration = 8
        )
        return()
      } # nocov end
      ctas_log("measures: ", nrow(measures), " rows, ",
              ncol(measures), " cols")

      shiny::setProgress(0.95, detail = "Finalizing")
      rv_measures(measures)
      rv_ctas_results(ctas_results)
      rv_untransformed(ctas_data$untransformed)
      rv_queries(ctas_data$queries)
      rv_pd(ctas_data$pd)
      rv_dataset_label(label)

      studies <- if ("study" %in% names(ctas_data$subjects)) {
        sort(unique(ctas_data$subjects$study)) # nocov
      } else {
        NULL
      }
      rv_studies(if (!is.null(studies) && length(studies) > 1) studies else NULL)

      shiny::setProgress(1, detail = "Done")
      ctas_log("=== Load complete: '", label, "' with ",
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
      pd = rv_pd,
      dataset_label = rv_dataset_label,
      studies = rv_studies,
      selected_study = rv_selected_study
    )
  })
}
