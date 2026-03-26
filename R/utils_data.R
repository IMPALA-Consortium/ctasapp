#' Prepare measures table from ctas input and results
#'
#' Joins the raw timeseries data with subject metadata, parameter metadata,
#' and aggregated site scores into a single data frame suitable for plotting.
#'
#' @param ctas_data List with elements `data`, `subjects`, `parameters`
#'   as used by [ctas::process_a_study()].
#' @param ctas_results List with elements `timeseries`, `timeseries_features`,
#'   `PCA_coordinates`, `site_scores` as returned by [ctas::process_a_study()].
#'
#' @return A data frame with one row per subject/timepoint/parameter observation
#'   and columns: subject_id, timepoint_rank, timepoint_1_name, result,
#'   parameter_id, site, country, region, parameter_name,
#'   parameter_category_1/2/3, max_score, n_subjects.
#'
#' @export
prepare_measures <- function(ctas_data, ctas_results) {

  scores <- ctas_results$site_scores |>
    dplyr::left_join(
      ctas_results$timeseries[, c("timeseries_id", "parameter_id")],
      by = "timeseries_id"
    ) |>
    dplyr::summarise(
      max_score = max(.data$fdr_corrected_pvalue_logp, na.rm = TRUE),
      .by = c("site", "parameter_id")
    )

  param_cols <- c(
    "parameter_id", "parameter_name",
    "parameter_category_1", "parameter_category_2", "parameter_category_3"
  )

  measures <- ctas_data$data |>
    dplyr::left_join(
      ctas_data$subjects,
      by = "subject_id"
    ) |>
    dplyr::left_join(
      ctas_data$parameters[, param_cols],
      by = "parameter_id"
    ) |>
    dplyr::left_join(
      scores,
      by = c("site", "parameter_id")
    ) |>
    dplyr::mutate(
      max_score = tidyr::replace_na(.data$max_score, 0)
    )

  n_subj <- measures |>
    dplyr::summarise(
      n_subjects = dplyr::n_distinct(.data$subject_id),
      .by = c("site", "parameter_id")
    )

  measures |>
    dplyr::left_join(n_subj, by = c("site", "parameter_id"))
}


#' Prepare wide score table for a single parameter
#'
#' Pivots per-feature site scores into a wide data frame with one row per site
#' and one column per feature, plus an aggregated `max_score` column.
#'
#' @param ctas_results List as returned by [ctas::process_a_study()].
#' @param parameter_id Character scalar, the parameter to filter on.
#'
#' @return A data frame with columns: site, one per feature, max_score.
#'   Sorted by descending max_score.
#' @export
prepare_score_table <- function(ctas_results, parameter_id) {
  scores_long <- ctas_results$site_scores |>
    dplyr::left_join(
      ctas_results$timeseries[, c("timeseries_id", "parameter_id")],
      by = "timeseries_id"
    ) |>
    dplyr::filter(.data$parameter_id == .env$parameter_id) |>
    dplyr::summarise(
      score = max(.data$fdr_corrected_pvalue_logp, na.rm = TRUE),
      .by = c("site", "feature")
    )

  scores_wide <- scores_long |>
    tidyr::pivot_wider(names_from = "feature", values_from = "score", values_fill = 0) |>
    dplyr::mutate(
      max_score = do.call(pmax, c(dplyr::across(-"site"), na.rm = TRUE))
    ) |>
    dplyr::arrange(dplyr::desc(.data$max_score))

  feature_cols <- setdiff(names(scores_wide), c("site", "max_score"))

  scores_wide |>
    dplyr::mutate(dplyr::across(dplyr::all_of(feature_cols), \(x) round(x, 2))) |>
    dplyr::mutate(max_score = round(.data$max_score, 2))
}


#' Prepare wide score table for multiple parameter IDs
#'
#' Like [prepare_score_table()] but accepts a vector of parameter IDs, pooling
#' scores across them. Used for grouped categorical/bar parameters.
#'
#' @param ctas_results List as returned by [ctas::process_a_study()].
#' @param parameter_ids Character vector of parameter IDs.
#'
#' @return A data frame with columns: site, one per feature, max_score.
#' @export
prepare_score_table_multi <- function(ctas_results, parameter_ids) {
  scores_long <- ctas_results$site_scores |>
    dplyr::left_join(
      ctas_results$timeseries[, c("timeseries_id", "parameter_id")],
      by = "timeseries_id"
    ) |>
    dplyr::filter(.data$parameter_id %in% .env$parameter_ids) |>
    dplyr::summarise(
      score = max(.data$fdr_corrected_pvalue_logp, na.rm = TRUE),
      .by = c("site", "feature")
    )

  if (nrow(scores_long) == 0) {
    return(data.frame(site = character(0), max_score = numeric(0)))
  }

  scores_wide <- scores_long |>
    tidyr::pivot_wider(names_from = "feature", values_from = "score", values_fill = 0) |>
    dplyr::mutate(
      max_score = do.call(pmax, c(dplyr::across(-"site"), na.rm = TRUE))
    ) |>
    dplyr::arrange(dplyr::desc(.data$max_score))

  feature_cols <- setdiff(names(scores_wide), c("site", "max_score"))

  scores_wide |>
    dplyr::mutate(dplyr::across(dplyr::all_of(feature_cols), \(x) round(x, 2))) |>
    dplyr::mutate(max_score = round(.data$max_score, 2))
}


#' Prepare timeseries data for outlier sites (multiple parameters)
#'
#' Like [prepare_ts_data()] but accepts a vector of parameter IDs. When
#' `untransformed` is supplied (non-NULL), joins in the original pre-transformation
#' values (original_value, lower, upper, original_category) keyed on
#' `subject_id + parameter_category_2 + timepoint_1_name`.
#'
#' @param measures Data frame as returned by [prepare_measures()].
#' @param parameter_ids Character vector of parameter IDs.
#' @param thresh Numeric threshold for outlier flagging.
#' @param untransformed Optional data frame with columns subject_id,
#'   parameter_category_2, timepoint_1_name, original_value, lower, upper,
#'   original_category. Pass NULL (default) to show transformed result only.
#'
#' @return A data frame sorted by site, subject_id, timepoint_rank.
#' @export
prepare_ts_data_multi <- function(measures, parameter_ids, thresh,
                                  untransformed = NULL) {
  filtered <- measures |>
    dplyr::filter(
      .data$parameter_id %in% .env$parameter_ids,
      .data$max_score > .env$thresh
    )

  if (!is.null(untransformed) && nrow(filtered) > 0) {
    display_cats <- unique(filtered$parameter_category_2)
    ut_sub <- untransformed |>
      dplyr::filter(.data$parameter_category_2 %in% .env$display_cats) |>
      dplyr::distinct(
        .data$subject_id, .data$parameter_category_2,
        .data$timepoint_1_name, .keep_all = TRUE
      )

    filtered <- filtered |>
      dplyr::left_join(
        ut_sub,
        by = c("subject_id", "parameter_category_2", "timepoint_1_name")
      )

    ut_cols <- c("original_value", "lower", "upper", "original_category")
    result <- filtered |>
      dplyr::select(
        "site", "subject_id", "parameter_id", "timepoint_rank",
        "timepoint_1_name", dplyr::all_of(ut_cols),
        "result", "parameter_name", "max_score"
      ) |>
      dplyr::mutate(
        original_value = round(.data$original_value, 3),
        lower = round(.data$lower, 3),
        upper = round(.data$upper, 3),
        result = round(.data$result, 3),
        max_score = round(.data$max_score, 2)
      )

    all_na <- vapply(result[ut_cols], function(x) all(is.na(x)), logical(1))
    result <- result[, !names(result) %in% names(all_na[all_na]), drop = FALSE]
  } else {
    result <- filtered |>
      dplyr::select(
        "site", "subject_id", "parameter_id", "timepoint_rank",
        "timepoint_1_name", "result", "parameter_name", "max_score"
      ) |>
      dplyr::mutate(
        result = round(.data$result, 3),
        max_score = round(.data$max_score, 2)
      )
  }

  result |>
    dplyr::arrange(.data$site, .data$subject_id, .data$timepoint_rank)
}


#' Prepare timeseries data for outlier sites
#'
#' Filters the measures data frame to a single parameter and sites whose
#' `max_score` exceeds the threshold, selecting key columns for display.
#'
#' @param measures Data frame as returned by [prepare_measures()].
#' @param parameter_id Character scalar, the parameter to filter on.
#' @param thresh Numeric threshold for outlier flagging.
#'
#' @return A data frame with columns: site, subject_id, timepoint_rank,
#'   timepoint_1_name, result, parameter_name, max_score. Sorted by
#'   site, subject_id, timepoint_rank.
#' @export
prepare_ts_data <- function(measures, parameter_id, thresh) {
  measures |>
    dplyr::filter(
      .data$parameter_id == .env$parameter_id,
      .data$max_score > .env$thresh
    ) |>
    dplyr::select(
      "site", "subject_id", "timepoint_rank", "timepoint_1_name",
      "result", "parameter_name", "max_score"
    ) |>
    dplyr::mutate(
      result = round(.data$result, 3),
      max_score = round(.data$max_score, 2)
    ) |>
    dplyr::arrange(.data$site, .data$subject_id, .data$timepoint_rank)
}
