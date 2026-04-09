#' Aggregate uploaded results by removing timeseries_id
#'
#' Drops the `timeseries_id` column and collapses rows by taking the
#' maximum `fdr_corrected_pvalue_logp` per group of remaining columns.
#' This dramatically reduces the size of the results data frame for large
#' uploads where many timeseries map to the same parameter/site/feature.
#'
#' @param results_df Data frame read from the uploaded results file.
#' @return A data frame without `timeseries_id`, with one row per unique
#'   combination of the remaining columns.
#' @export
aggregate_results <- function(results_df) {
  results_df[["timeseries_id"]] <- NULL
  group_cols <- setdiff(names(results_df), "fdr_corrected_pvalue_logp")
  results_df |>
    dplyr::summarise(
      fdr_corrected_pvalue_logp = if (all(is.na(.data$fdr_corrected_pvalue_logp))) {
        NA_real_
      } else {
        max(.data$fdr_corrected_pvalue_logp, na.rm = TRUE)
      },
      .by = dplyr::all_of(group_cols)
    )
}


#' Ensure parameter_id is present in site_scores
#'
#' For uploaded data, `site_scores` already contains `parameter_id`.
#' For sample/ctas-package data, `parameter_id` lives in the separate
#' `timeseries` table and must be joined in via `timeseries_id`.
#'
#' @param ctas_results List with `site_scores` and optionally `timeseries`.
#' @return A data frame of site scores with a `parameter_id` column.
#' @keywords internal
scores_with_parameter_id <- function(ctas_results) {
  ss <- ctas_results$site_scores
  if ("parameter_id" %in% names(ss)) return(ss)
  ss |>
    dplyr::left_join(
      ctas_results$timeseries[, c("timeseries_id", "parameter_id")],
      by = "timeseries_id"
    )
}


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

  scores <- scores_with_parameter_id(ctas_results) |>
    dplyr::summarise(
      max_score = if (all(is.na(.data$fdr_corrected_pvalue_logp))) {
        NA_real_
      } else {
        max(.data$fdr_corrected_pvalue_logp, na.rm = TRUE)
      },
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
  scores_long <- scores_with_parameter_id(ctas_results) |>
    dplyr::filter(.data$parameter_id == .env$parameter_id) |>
    dplyr::summarise(
      score = if (all(is.na(.data$fdr_corrected_pvalue_logp))) {
        NA_real_
      } else {
        max(.data$fdr_corrected_pvalue_logp, na.rm = TRUE)
      },
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


#' Recompute max_score in measures using a subset of ctas features
#'
#' Recalculates `max_score` per (site, parameter_id) from
#' `ctas_results$site_scores`, optionally filtering to a subset of features.
#' Returns the measures data frame with an updated `max_score` column.
#'
#' @param measures Data frame as returned by [prepare_measures()].
#' @param ctas_results List as returned by [ctas::process_a_study()].
#' @param features Character vector of feature names to include, or NULL for all.
#'
#' @return A data frame identical to `measures` but with recalculated `max_score`.
#' @export
recompute_max_score <- function(measures, ctas_results, features = NULL) {
  scores <- scores_with_parameter_id(ctas_results)

  if (!is.null(features)) {
    scores <- scores |>
      dplyr::filter(.data$feature %in% .env$features)
  }

  new_scores <- scores |>
    dplyr::summarise(
      max_score = if (all(is.na(.data$fdr_corrected_pvalue_logp))) {
        NA_real_
      } else {
        max(.data$fdr_corrected_pvalue_logp, na.rm = TRUE)
      },
      .by = c("site", "parameter_id")
    )

  measures |>
    dplyr::select(-"max_score") |>
    dplyr::left_join(new_scores, by = c("site", "parameter_id")) |>
    dplyr::mutate(max_score = tidyr::replace_na(.data$max_score, 0))
}


#' Prepare wide score table for multiple parameter IDs
#'
#' Like [prepare_score_table()] but accepts a vector of parameter IDs, pooling
#' scores across them. Used for grouped categorical/bar parameters.
#'
#' @param ctas_results List as returned by [ctas::process_a_study()].
#' @param parameter_ids Character vector of parameter IDs.
#' @param features Character vector of feature names to include, or NULL for all.
#'
#' @return A data frame with columns: site, one per feature, max_score.
#' @export
prepare_score_table_multi <- function(ctas_results, parameter_ids,
                                      features = NULL) {
  scores_long <- scores_with_parameter_id(ctas_results) |>
    dplyr::filter(.data$parameter_id %in% .env$parameter_ids)

  if (!is.null(features)) {
    scores_long <- scores_long |>
      dplyr::filter(.data$feature %in% .env$features)
  }

  scores_long <- scores_long |>
    dplyr::summarise(
      score = if (all(is.na(.data$fdr_corrected_pvalue_logp))) {
        NA_real_
      } else {
        max(.data$fdr_corrected_pvalue_logp, na.rm = TRUE)
      },
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
                                  untransformed = NULL,
                                  plot_type = "numeric") {
  # Determine outlier sites from the max score across ALL parameter_ids in the

  # group, so that categorical one-hot encodings with individually low scores
  # are still included when a sibling encoding flags the site.
  outlier_sites <- measures |>
    dplyr::filter(.data$parameter_id %in% .env$parameter_ids) |>
    dplyr::summarise(
      group_max = max(.data$max_score, na.rm = TRUE),
      .by = "site"
    ) |>
    dplyr::filter(.data$group_max > .env$thresh) |>
    dplyr::pull(.data$site)

  filtered <- measures |>
    dplyr::filter(
      .data$parameter_id %in% .env$parameter_ids,
      .data$site %in% .env$outlier_sites,
      .data$parameter_category_3 != "ratio_missing"
    )

  is_cat <- plot_type %in% c("categorical", "bar")

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
    ut_cols <- intersect(ut_cols, names(filtered))

    if (is_cat) {
      result <- filtered |>
        dplyr::select(
          "site", "subject_id", "parameter_category_2",
          "timepoint_rank", "timepoint_1_name",
          dplyr::any_of("original_category")
        ) |>
        dplyr::distinct()
    } else {
      result <- filtered |>
        dplyr::select(
          "site", "subject_id", "parameter_category_2",
          "timepoint_rank", "timepoint_1_name",
          dplyr::any_of(ut_cols),
          "result", "parameter_id", "parameter_name", "max_score"
        ) |>
        dplyr::mutate(
          dplyr::across(
            dplyr::any_of(c("original_value", "lower", "upper")),
            \(x) round(x, 3)
          ),
          result = round(.data$result, 3),
          max_score = round(.data$max_score, 2)
        )
    }

    if (!is_cat) {
      all_na <- vapply(ut_cols, function(col) {
        if (col %in% names(result)) all(is.na(result[[col]])) else TRUE
      }, logical(1))
      drop_cols <- names(all_na[all_na])
      result <- result[, !names(result) %in% drop_cols, drop = FALSE]
    }
  } else {
    if (is_cat) {
      result <- filtered |>
        dplyr::select(
          "site", "subject_id", "parameter_category_2",
          "timepoint_rank", "timepoint_1_name"
        ) |>
        dplyr::distinct()
    } else {
      result <- filtered |>
        dplyr::select(
          "site", "subject_id", "parameter_category_2",
          "timepoint_rank", "timepoint_1_name",
          "result", "parameter_id", "parameter_name", "max_score"
        ) |>
        dplyr::mutate(
          result = round(.data$result, 3),
          max_score = round(.data$max_score, 2)
        )
    }
  }

  result |>
    dplyr::arrange(.data$site, .data$subject_id, .data$timepoint_rank)
}


#' Validate uploaded results file
#'
#' Checks that a data frame has the columns and types expected for the
#' pre-joined `site_scores + timeseries` results file.
#'
#' @param df Data frame to validate.
#' @return Character vector of error messages (length-0 means valid).
#' @export
validate_upload_results <- function(df) {
  errs <- character()

  if (!is.data.frame(df)) {
    return("Results file must be a data frame.")
  }

  required <- c("site", "timeseries_id", "parameter_id", "feature",
                 "fdr_corrected_pvalue_logp")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    errs <- c(errs, paste0("Results file is missing columns: ",
                            paste(missing, collapse = ", ")))
  }

  if ("fdr_corrected_pvalue_logp" %in% names(df) &&
      !is.numeric(df$fdr_corrected_pvalue_logp)) {
    errs <- c(errs, "Column 'fdr_corrected_pvalue_logp' must be numeric.")
  }

  errs
}


#' Validate uploaded input file
#'
#' Checks that a data frame has the columns and types expected for the
#' pre-joined `data + subjects + parameters` input file.
#'
#' @param df Data frame to validate.
#' @return Character vector of error messages (length-0 means valid).
#' @export
validate_upload_input <- function(df) {
  errs <- character()

  if (!is.data.frame(df)) {
    return("Input file must be a data frame.")
  }

  required <- c("subject_id", "site", "parameter_id", "parameter_name",
                 "parameter_category_3", "timepoint_1_name",
                 "timepoint_rank", "result")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    errs <- c(errs, paste0("Input file is missing columns: ",
                            paste(missing, collapse = ", ")))
  }

  if ("result" %in% names(df) && !is.numeric(df$result)) {
    errs <- c(errs, "Column 'result' must be numeric.")
  }
  if ("timepoint_rank" %in% names(df) && !is.numeric(df$timepoint_rank)) {
    errs <- c(errs, "Column 'timepoint_rank' must be numeric.")
  }

  errs
}


#' Validate uploaded untransformed file
#'
#' Checks that a data frame has the join-key columns required to link
#' untransformed data back to the input measures.
#'
#' @param df Data frame to validate.
#' @return Character vector of error messages (length-0 means valid).
#' @export
validate_upload_untransformed <- function(df) {
  errs <- character()

  if (!is.data.frame(df)) {
    return("Untransformed file must be a data frame.")
  }

  required <- c("subject_id", "parameter_category_2", "timepoint_1_name")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    errs <- c(errs, paste0("Untransformed file is missing columns: ",
                            paste(missing, collapse = ", ")))
  }

  errs
}


#' Validate uploaded queries file
#'
#' Checks that a data frame has the columns required for query overlay on
#' plots and display in the queries table.
#'
#' @param df Data frame to validate.
#' @return Character vector of error messages (length-0 means valid).
#' @export
validate_upload_queries <- function(df) {
  errs <- character()

  if (!is.data.frame(df)) {
    return("Queries file must be a data frame.")
  }

  required <- c("subject_id", "parameter_id", "visit", "data_change")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    errs <- c(errs, paste0("Queries file is missing columns: ",
                            paste(missing, collapse = ", ")))
  }

  if ("data_change" %in% names(df) && !is.logical(df$data_change)) {
    errs <- c(errs, "Column 'data_change' must be logical (TRUE/FALSE).")
  }

  errs
}


#' Cross-validate uploaded files
#'
#' Checks referential integrity between the results and input files:
#' parameter_ids in input should appear in results, and sites in results
#' should appear in input.
#'
#' @param input_df Input data frame (File 2).
#' @param results_df Results data frame (File 1).
#' @return Character vector of warning messages (length-0 means clean).
#' @export
validate_upload_crossfile <- function(input_df, results_df) {

  warns <- character()

  result_params <- unique(results_df$parameter_id)
  input_params <- unique(input_df$parameter_id)
  orphan_results <- setdiff(result_params, input_params)
  if (length(orphan_results) > 0) {
    warns <- c(warns, paste0(
      length(orphan_results),
      " parameter_id(s) in results not found in input: ",
      paste(utils::head(orphan_results, 5), collapse = ", "),
      if (length(orphan_results) > 5) "..." else ""
    ))
  }

  result_sites <- unique(results_df$site)
  input_sites <- unique(input_df$site)
  orphan_sites <- setdiff(result_sites, input_sites)
  if (length(orphan_sites) > 0) {
    warns <- c(warns, paste0(
      length(orphan_sites),
      " site(s) in results not found in input: ",
      paste(utils::head(orphan_sites, 5), collapse = ", "),
      if (length(orphan_sites) > 5) "..." else ""
    ))
  }

  warns
}


#' Reconstruct ctas data structures from flat upload files
#'
#' Takes the flat uploaded data frames and splits them back into the list
#' structures expected by [prepare_measures()].
#'
#' @param input_df Data frame from the input file upload.
#' @param results_df Data frame from the results file upload.
#' @param untransformed_df Optional data frame from the untransformed upload.
#' @param queries_df Optional data frame from the queries upload.
#'
#' @return A named list with `ctas_data` and `ctas_results`.
#' @export
reconstruct_from_upload <- function(input_df, results_df,
                                    untransformed_df = NULL,
                                    queries_df = NULL) {

  data_cols <- c("subject_id", "parameter_id", "timepoint_1_name",
                 "timepoint_rank", "result")
  for (opt in c("timepoint_2_name", "baseline")) {
    if (opt %in% names(input_df)) data_cols <- c(data_cols, opt)
  }

  subject_cols <- "subject_id"
  for (opt in c("site", "country", "region", "study")) {
    if (opt %in% names(input_df)) subject_cols <- c(subject_cols, opt)
  }

  param_cols <- intersect(
    c("parameter_id", "parameter_name", "parameter_category_1",
      "parameter_category_2", "parameter_category_3"),
    names(input_df)
  )

  ctas_data <- list(
    data       = unique(input_df[, data_cols, drop = FALSE]),
    subjects   = unique(input_df[, subject_cols, drop = FALSE]),
    parameters = unique(input_df[, param_cols, drop = FALSE]),
    untransformed = untransformed_df,
    queries       = queries_df
  )

  score_cols <- intersect(
    c("timeseries_id", "site", "country", "region", "feature",
      "pvalue_kstest_logp", "kstest_statistic", "fdr_corrected_pvalue_logp",
      "ref_group", "subject_count", "parameter_id"),
    names(results_df)
  )

  has_ts <- "timeseries_id" %in% names(results_df)
  ctas_results <- list(
    site_scores = results_df[, score_cols, drop = FALSE],
    timeseries  = if (has_ts) {
      unique(results_df[, c("timeseries_id", "parameter_id"), drop = FALSE])
    }
  )

  list(ctas_data = ctas_data, ctas_results = ctas_results)
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
