pkgload::load_all(".", export_all = FALSE)

dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)

keep_cat2 <- c("ALT", "AST", "CREAT",
               "VS_DIABP", "VS_HEIGHT", "VS_PULSE", "VS_SYSBP",
               "VS_TEMP", "VS_WEIGHT",
               "RS_OVRLRESP", "VS_WEIGHT_CAT")

# -- ctas sample ---------------------------------------------------------------
ctas_d <- sample_ctas_data
ctas_r <- sample_ctas_results

ctas_input_df <- ctas_d$data |>
  dplyr::left_join(ctas_d$subjects, by = "subject_id") |>
  dplyr::left_join(
    ctas_d$parameters[, c("parameter_id", "parameter_name",
                           "parameter_category_1", "parameter_category_2",
                           "parameter_category_3")],
    by = "parameter_id"
  ) |>
  dplyr::mutate(study = "STUDY-001")

ctas_results_df <- ctas_r$site_scores |>
  dplyr::left_join(
    ctas_r$timeseries[, c("timeseries_id", "parameter_id")],
    by = "timeseries_id"
  )

# -- SDTM sample (filtered to selected parameters) ----------------------------
sdtm_d <- sample_sdtm_data
sdtm_r <- sample_sdtm_results

keep_param_ids <- sdtm_d$parameters$parameter_id[
  sdtm_d$parameters$parameter_category_2 %in% keep_cat2
]

sdtm_input_df <- sdtm_d$data |>
  dplyr::filter(.data$parameter_id %in% keep_param_ids) |>
  dplyr::left_join(sdtm_d$subjects, by = "subject_id") |>
  dplyr::left_join(
    sdtm_d$parameters[, c("parameter_id", "parameter_name",
                           "parameter_category_1", "parameter_category_2",
                           "parameter_category_3")],
    by = "parameter_id"
  ) |>
  dplyr::mutate(study = "STUDY-002")

sdtm_results_df <- sdtm_r$site_scores |>
  dplyr::left_join(
    sdtm_r$timeseries[, c("timeseries_id", "parameter_id")],
    by = "timeseries_id"
  ) |>
  dplyr::filter(.data$parameter_id %in% keep_param_ids)

# -- Combine into multi-study flat files ----------------------------------------
input_csv <- dplyr::bind_rows(ctas_input_df, sdtm_input_df)
results_csv <- dplyr::bind_rows(ctas_results_df, sdtm_results_df)

untransformed_csv <- if (!is.null(sdtm_d$untransformed)) {
  sdtm_d$untransformed |>
    dplyr::filter(.data$parameter_category_2 %in% keep_cat2)
}

sdtm_queries <- if (!is.null(sdtm_d$queries)) {
  sdtm_d$queries |>
    dplyr::filter(.data$parameter_id %in% keep_param_ids)
}
queries_csv <- dplyr::bind_rows(
  ctas_d$queries,
  sdtm_queries
)

# -- Write CSVs -----------------------------------------------------------------
utils::write.csv(results_csv, "inst/extdata/results.csv", row.names = FALSE)
utils::write.csv(input_csv, "inst/extdata/input.csv", row.names = FALSE)

if (!is.null(untransformed_csv) && nrow(untransformed_csv) > 0) {
  utils::write.csv(untransformed_csv, "inst/extdata/untransformed.csv",
                   row.names = FALSE)
}

if (!is.null(queries_csv) && nrow(queries_csv) > 0) {
  utils::write.csv(queries_csv, "inst/extdata/queries.csv", row.names = FALSE)
}

message("Wrote CSV test fixtures to inst/extdata/")
