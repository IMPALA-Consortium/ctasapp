pkgload::load_all(".")

lRaw <- list(
  Raw_DM      = gsm.ctas::dm,
  Raw_LB      = gsm.ctas::lb,
  Raw_VS      = gsm.ctas::vs,
  Raw_EG      = gsm.ctas::eg,
  Raw_RS      = gsm.ctas::rs,
  Raw_Queries = gsm.ctas::queries,
  Raw_PD      = gsm.ctas::pd
)

wf_path <- function(layer) system.file("workflow", layer, package = "gsm.ctas")
lMappingWorkflows   <- workr::MakeWorkflowList(strPath = wf_path("1_mappings"))
lMetricWorkflows    <- workr::MakeWorkflowList(strPath = wf_path("2_metrics"))
lReportingWorkflows <- workr::MakeWorkflowList(strPath = wf_path("3_reporting"))

lMapped <- workr::RunWorkflows(
  lWorkflows = lMappingWorkflows,
  lData = gsm.mapping::Ingest(lRaw, gsm.mapping::CombineSpecs(lMappingWorkflows))
)

lAnalysis <- workr::RunWorkflows(
  lWorkflows = lMetricWorkflows,
  lData = lMapped
)

lReporting <- workr::RunWorkflows(
  lWorkflows = lReportingWorkflows,
  lData = c(lMapped, list(
    lAnalyzed = lAnalysis,
    lMetricWorkflows = lMetricWorkflows
  ))
)

unlink(list.files("inst/extdata", pattern = "\\.(csv|parquet)$", full.names = TRUE))
files <- gsm.ctas::Write_ctasapp(lReporting, "inst/extdata", strFormat = "parquet")
files <- files[basename(files) %in%
  paste0(c("results", "input", "untransformed", "queries", "pd"), ".parquet")]
unlink(setdiff(
  list.files("inst/extdata", pattern = "\\.parquet$", full.names = TRUE),
  files
))

read_parquet <- function(name) {
  f <- file.path("inst/extdata", paste0(name, ".parquet"))
  if (file.exists(f)) read_upload_file(f, basename(f))
}

results_df <- read_parquet("results")
input_df <- read_parquet("input")
untransformed_df <- read_parquet("untransformed")
queries_df <- read_parquet("queries")
pd_df <- read_parquet("pd")

errs <- c(
  validate_upload_results(results_df),
  validate_upload_input(input_df),
  if (!is.null(untransformed_df)) validate_upload_untransformed(untransformed_df),
  if (!is.null(queries_df)) validate_upload_queries(queries_df),
  if (!is.null(pd_df)) validate_upload_pd(pd_df)
)
if (length(errs) > 0) stop(paste(errs, collapse = "\n"))

reconstructed <- reconstruct_from_upload(
  input_df, aggregate_results(results_df),
  untransformed_df, queries_df, pd_df
)

sample_data <- reconstructed$ctas_data
sample_results <- reconstructed$ctas_results

lookup <- build_param_lookup(prepare_measures(sample_data, sample_results))
if (!identical(lookup$plot_type[lookup$display_id == "BMICAT"], "bar")) {
  stop("BMICAT is not detected as a bar field")
}

usethis::use_data(sample_data, overwrite = TRUE)
usethis::use_data(sample_results, overwrite = TRUE)
