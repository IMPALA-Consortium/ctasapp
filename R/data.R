#' Sample ctas input data
#'
#' A subset of `ctas::ctas_data` containing the input data, subjects, and
#' parameters tables used to generate sample results.
#'
#' @format A list with three elements:
#' \describe{
#'   \item{data}{Tibble with 6890 rows: subject_id, timepoint_rank,
#'     timepoint_1_name, result, parameter_id, timepoint_2_name, baseline}
#'   \item{subjects}{Tibble with 177 rows: subject_id, site, country, region}
#'   \item{parameters}{Tibble with 2 rows: parameter_id, parameter_name,
#'     parameter_category_1 through _3, and analysis config columns}
#' }
#' @source Generated from `ctas::ctas_data` via `data-raw/sample_data.R`
"sample_ctas_data"

#' Sample ctas analysis results
#'
#' Output of `ctas::process_a_study()` run on [sample_ctas_data].
#'
#' @format A list with four elements:
#' \describe{
#'   \item{timeseries}{Tibble with 10 rows: timeseries metadata}
#'   \item{timeseries_features}{Tibble with 7902 rows: per-subject feature values}
#'   \item{PCA_coordinates}{Tibble: first two principal components per subject}
#'   \item{site_scores}{Tibble with 2221 rows: site biasness scores per
#'     timeseries/feature, including fdr_corrected_pvalue_logp}
#' }
#' @source Generated via `data-raw/sample_data.R`
"sample_ctas_results"
