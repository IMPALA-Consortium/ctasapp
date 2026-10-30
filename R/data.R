#' Sample input data generated with gsm.ctas
#'
#' Output of the gsm.ctas mapping, metrics and reporting workflows on the
#' CDISC pilot SDTM data shipped with gsm.ctas (study `CDISCPILOT01`),
#' reconstructed with [reconstruct_from_upload()]. Contains range-normalized
#' and ratio-missing labs, numeric vital signs and ECG, categorical overall
#' response (`OVRLRESP`) and a single-timepoint BMI category (`BMICAT`,
#' plotted as bar). The same data is shipped as parquet upload files in
#' `system.file("extdata", package = "ctasapp")`.
#'
#' @format A list with six elements:
#' \describe{
#'   \item{data}{Data frame: subject_id, parameter_id, timepoint_1_name,
#'     timepoint_2_name, timepoint_rank, result, study}
#'   \item{subjects}{Data frame: subject_id, site, country, study}
#'   \item{parameters}{Data frame: parameter_id, parameter_name,
#'     parameter_category_1 through _3}
#'   \item{untransformed}{Data frame of original values and reference ranges}
#'   \item{queries}{Data frame of clinical queries}
#'   \item{pd}{Data frame of protocol deviations}
#' }
#' @source Generated with gsm.ctas via `data-raw/sample_data.R`
"sample_data"

#' Sample ctas results generated with gsm.ctas
#'
#' Site scores computed by the gsm.ctas metric workflows for [sample_data].
#'
#' @format A list with two elements:
#' \describe{
#'   \item{site_scores}{Data frame: site, parameter_id, feature,
#'     fdr_corrected_pvalue_logp, study and further site attributes}
#'   \item{timeseries}{`NULL`; parameter_id is included in `site_scores`}
#' }
#' @source Generated with gsm.ctas via `data-raw/sample_data.R`
"sample_results"
