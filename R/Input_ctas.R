# gsm.core-style Input helpers for converting SDTM domains to ctas input.
# Designed for future extraction to a gsm.ctas package.


#' Inject random missingness into LB data for specific site/parameter combos
#'
#' Useful for creating demo datasets that showcase the missingness ratio
#' visualization. Affected sites are renamed with a `_MISS_<LBTESTCD>` suffix
#' so they are visually identifiable in the app.
#'
#' @param dfDM Data frame with SDTM DM columns (must include USUBJID, SITEID).
#' @param dfLB Data frame with SDTM LB columns (must include USUBJID, LBTESTCD, LBSTRESN).
#' @param injections Data frame with columns `site` (character), `lbtestcd`
#'   (character), and `frac` (numeric 0-1, fraction of values to set NA).
#' @param seed Integer seed for reproducibility.
#'
#' @return A list with elements `dm` and `lb` containing the modified data frames.
#' @export
inject_missingness <- function(dfDM, dfLB, injections, seed = 42) {
  set.seed(seed)

  for (i in seq_len(nrow(injections))) {
    site_id <- injections$site[i]
    testcd <- injections$lbtestcd[i]
    frac <- injections$frac[i]

    subj_at_site <- unique(dfDM$USUBJID[dfDM$SITEID == site_id])
    target_rows <- which(
      dfLB$USUBJID %in% subj_at_site & dfLB$LBTESTCD == testcd
    )
    n_to_na <- round(length(target_rows) * frac)
    if (n_to_na > 0) {
      na_rows <- sample(target_rows, n_to_na)
      dfLB$LBSTRESN[na_rows] <- NA_real_
    }

    new_id <- paste0(site_id, "_MISS_", testcd)
    dfDM$SITEID[dfDM$SITEID == site_id] <- new_id
  }

  list(dm = dfDM, lb = dfLB)
}

#' Build subject table from DM domain
#'
#' @param dfDM Data frame with SDTM DM columns (USUBJID, SITEID, COUNTRY, etc.).
#' @param strSubjectCol Column name for subject ID. Default: `"USUBJID"`.
#' @param strSiteCol Column name for site. Default: `"SITEID"`.
#' @param strCountryCol Column name for country. Default: `"COUNTRY"`.
#'
#' @return Data frame with columns: subject_id, site, country, region.
#' @keywords internal
build_subjects <- function(dfDM,
                           strSubjectCol = "USUBJID",
                           strSiteCol = "SITEID",
                           strCountryCol = "COUNTRY") {
  dfDM |>
    dplyr::transmute(
      subject_id = .data[[strSubjectCol]],
      site = .data[[strSiteCol]],
      country = .data[[strCountryCol]],
      region = NA_character_
    ) |>
    dplyr::distinct()
}


#' Filter out unscheduled visits
#' @param df Data frame with a `timepoint_1_name` column.
#' @return Filtered data frame.
#' @keywords internal
filter_unscheduled <- function(df) {
  df |>
    dplyr::filter(!grepl("unsch", tolower(.data$timepoint_1_name)))
}


#' Create ctas input from SDTM LB domain
#'
#' Produces range-normalized lab timeseries and ratio-missing timeseries for
#' each lab test. Both share the same `parameter_category_2` (the LBTESTCD)
#' so they are grouped together in the app sidebar and plotted in the same
#' faceted panel. Timepoint ranks are recalculated and unscheduled visits
#' are filtered out.
#'
#' @param dfDM Data frame with SDTM DM columns.
#' @param dfLB Data frame with SDTM LB columns.
#'
#' @return A list with elements `data`, `subjects`, `parameters` matching the
#'   ctas input schema, plus `untransformed` with original lab values and ranges.
#' @export
Input_Labs <- function(dfDM, dfLB) {
  subjects <- build_subjects(dfDM)

  df_base <- dfLB |>
    dplyr::transmute(
      subject_id = .data$USUBJID,
      lbtestcd = .data$LBTESTCD,
      lbtest = .data$LBTEST,
      timepoint_1_name = .data$VISIT,
      timepoint_rank = .data$VISITNUM,
      value = .data$LBSTRESN,
      lower = .data$LBSTNRLO,
      upper = .data$LBSTNRHI
    ) |>
    filter_unscheduled()

  # Range-normalized series
  df_norm <- df_base |>
    dplyr::filter(!is.na(.data$value)) |>
    dplyr::mutate(
      result = normalize_by_range(.data$value, .data$lower, .data$upper),
      parameter_id = paste0("LB_NORM_", .data$lbtestcd)
    ) |>
    dplyr::filter(!is.na(.data$result))

  params_norm <- df_norm |>
    dplyr::distinct(.data$parameter_id, .data$lbtestcd, .data$lbtest) |>
    dplyr::transmute(
      parameter_id = .data$parameter_id,
      parameter_name = paste("Lab (norm):", .data$lbtest),
      parameter_category_1 = "Labs",
      parameter_category_2 = .data$lbtestcd,
      parameter_category_3 = "range_normalized"
    )

  data_norm <- df_norm |>
    dplyr::transmute(
      .data$subject_id, .data$parameter_id,
      .data$timepoint_1_name, .data$timepoint_rank, .data$result,
      timepoint_2_name = NA_character_,
      baseline = NA_real_
    ) |>
    recalculate_timepoint_rank()

  # Ratio-missing series (uses all rows, including NA values)
  df_miss_base <- df_base |>
    dplyr::transmute(
      subject_id = .data$subject_id,
      lbtestcd = .data$lbtestcd,
      lbtest = .data$lbtest,
      timepoint_1_name = .data$timepoint_1_name,
      timepoint_rank = .data$timepoint_rank,
      value = .data$value,
      parameter_id = paste0("LB_MISS_", .data$lbtestcd)
    ) |>
    dplyr::arrange(.data$subject_id, .data$parameter_id, .data$timepoint_rank, .data$timepoint_1_name)

  df_miss_base$result <- ratio_missing_over_time(
    df_miss_base$value,
    df_miss_base$subject_id,
    df_miss_base$parameter_id
  )

  params_miss <- df_miss_base |>
    dplyr::distinct(.data$parameter_id, .data$lbtestcd, .data$lbtest) |>
    dplyr::transmute(
      parameter_id = .data$parameter_id,
      parameter_name = paste("Ratio Missing:", .data$lbtest),
      parameter_category_1 = "Labs",
      parameter_category_2 = .data$lbtestcd,
      parameter_category_3 = "ratio_missing"
    )

  data_miss <- df_miss_base |>
    dplyr::transmute(
      .data$subject_id, .data$parameter_id,
      .data$timepoint_1_name, .data$timepoint_rank, .data$result,
      timepoint_2_name = NA_character_,
      baseline = NA_real_
    ) |>
    recalculate_timepoint_rank()

  untransformed <- df_base |>
    dplyr::transmute(
      subject_id = .data$subject_id,
      parameter_category_2 = .data$lbtestcd,
      timepoint_1_name = .data$timepoint_1_name,
      original_value = .data$value,
      lower = .data$lower,
      upper = .data$upper,
      original_category = NA_character_
    )

  list(
    data = dplyr::bind_rows(data_norm, data_miss),
    subjects = subjects,
    parameters = dplyr::bind_rows(params_norm, params_miss),
    untransformed = untransformed
  )
}


#' Create ctas input from SDTM VS domain
#'
#' Produces numeric vital signs timeseries for each test code.
#' Recalculates timepoint ranks and filters unscheduled visits.
#'
#' @param dfDM Data frame with SDTM DM columns.
#' @param dfVS Data frame with SDTM VS columns.
#'
#' @return A list with elements `data`, `subjects`, `parameters`, plus
#'   `untransformed` with original vital sign values.
#' @export
Input_VS <- function(dfDM, dfVS) {
  subjects <- build_subjects(dfDM)

  df <- dfVS |>
    dplyr::filter(!is.na(.data$VSSTRESN)) |>
    dplyr::transmute(
      subject_id = .data$USUBJID,
      parameter_id = paste0("VS_", .data$VSTESTCD),
      vstest = .data$VSTEST,
      timepoint_1_name = .data$VISIT,
      timepoint_rank = .data$VISITNUM,
      result = .data$VSSTRESN,
      timepoint_2_name = NA_character_,
      baseline = NA_real_
    ) |>
    filter_unscheduled() |>
    recalculate_timepoint_rank()

  params <- df |>
    dplyr::distinct(.data$parameter_id, .data$vstest) |>
    dplyr::transmute(
      parameter_id = .data$parameter_id,
      parameter_name = paste("Vital Signs:", .data$vstest),
      parameter_category_1 = "Vital Signs",
      parameter_category_2 = .data$parameter_id,
      parameter_category_3 = "numeric"
    )

  untransformed <- df |>
    dplyr::transmute(
      subject_id = .data$subject_id,
      parameter_category_2 = .data$parameter_id,
      timepoint_1_name = .data$timepoint_1_name,
      original_value = .data$result,
      lower = NA_real_,
      upper = NA_real_,
      original_category = NA_character_
    )

  list(
    data = dplyr::select(df, -"vstest"),
    subjects = subjects,
    parameters = params,
    untransformed = untransformed
  )
}


#' Create ctas input from SDTM RS oncology domain
#'
#' One-hot encodes `RSORRES` for the overall response test, producing one
#' parameter per response level. Filters unscheduled visits and recalculates
#' timepoint ranks.
#'
#' @param dfDM Data frame with SDTM DM columns.
#' @param dfRS Data frame with SDTM RS columns (e.g. `rs_onco`).
#' @param strTestCD Test code to filter on. Default: `"OVRLRESP"`.
#'
#' @return A list with elements `data`, `subjects`, `parameters`, plus
#'   `untransformed` with original categorical response values.
#' @export
Input_RS <- function(dfDM, dfRS, strTestCD = "OVRLRESP") {
  subjects <- build_subjects(dfDM)

  df_filt <- dfRS |>
    dplyr::filter(
      .data$RSTESTCD == .env$strTestCD,
      !is.na(.data$RSORRES),
      .data$RSORRES != ""
    ) |>
    dplyr::transmute(
      subject_id = .data$USUBJID,
      timepoint_1_name = .data$VISIT,
      timepoint_rank = .data$VISITNUM,
      value = .data$RSORRES
    ) |>
    filter_unscheduled()

  prefix <- paste0("RS_", strTestCD)
  encoded <- encode_categorical(df_filt$value, prefix = prefix)

  empty_untransformed <- data.frame(
    subject_id = character(0), parameter_category_2 = character(0),
    timepoint_1_name = character(0), original_value = numeric(0),
    lower = numeric(0), upper = numeric(0),
    original_category = character(0), stringsAsFactors = FALSE
  )

  if (nrow(encoded) == 0) {
    return(list(
      data = data.frame(
        subject_id = character(0), parameter_id = character(0),
        timepoint_1_name = character(0), timepoint_2_name = character(0),
        timepoint_rank = numeric(0), result = numeric(0),
        baseline = numeric(0), stringsAsFactors = FALSE
      ),
      subjects = subjects,
      parameters = data.frame(
        parameter_id = character(0), parameter_name = character(0),
        parameter_category_1 = character(0), parameter_category_2 = character(0),
        parameter_category_3 = character(0), stringsAsFactors = FALSE
      ),
      untransformed = empty_untransformed
    ))
  }

  untransformed <- df_filt |>
    dplyr::transmute(
      subject_id = .data$subject_id,
      parameter_category_2 = .env$prefix,
      timepoint_1_name = .data$timepoint_1_name,
      original_value = NA_real_,
      lower = NA_real_,
      upper = NA_real_,
      original_category = .data$value
    )

  data <- data.frame(
    subject_id = df_filt$subject_id[encoded$orig_row],
    parameter_id = encoded$level,
    timepoint_1_name = df_filt$timepoint_1_name[encoded$orig_row],
    timepoint_rank = df_filt$timepoint_rank[encoded$orig_row],
    result = as.numeric(encoded$encoded),
    timepoint_2_name = NA_character_,
    baseline = NA_real_,
    stringsAsFactors = FALSE
  ) |>
    recalculate_timepoint_rank()

  levels <- sort(unique(encoded$level))
  params <- data.frame(
    parameter_id = levels,
    parameter_name = paste("Response:", levels),
    parameter_category_1 = "Response",
    parameter_category_2 = prefix,
    parameter_category_3 = "categorical",
    stringsAsFactors = FALSE
  )

  list(data = data, subjects = subjects, parameters = params,
       untransformed = untransformed)
}


#' Create ctas input for screening weight categories from VS
#'
#' Derives weight categories from screening-visit weight measurements
#' and encodes as a single-timepoint categorical parameter.
#'
#' @param dfDM Data frame with SDTM DM columns.
#' @param dfVS Data frame with SDTM VS columns.
#'
#' @return A list with elements `data`, `subjects`, `parameters`, plus
#'   `untransformed` with original weight values and categories.
#' @export
Input_BMI <- function(dfDM, dfVS) {
  subjects <- build_subjects(dfDM)

  df_wt <- dfVS |>
    dplyr::filter(
      .data$VSTESTCD == "WEIGHT",
      !is.na(.data$VSSTRESN),
      .data$VSBLFL == "Y" | .data$VISITNUM == 1
    ) |>
    dplyr::transmute(
      subject_id = .data$USUBJID,
      weight = .data$VSSTRESN,
      timepoint_1_name = .data$VISIT,
      timepoint_rank = .data$VISITNUM
    ) |>
    dplyr::arrange(.data$subject_id, .data$timepoint_rank) |>
    dplyr::distinct(.data$subject_id, .keep_all = TRUE)

  df_wt$weight_cat <- dplyr::case_when(
    df_wt$weight < 60 ~ "< 60 kg",
    df_wt$weight < 80 ~ "60-80 kg",
    df_wt$weight < 100 ~ "80-100 kg",
    TRUE ~ ">= 100 kg"
  )

  prefix <- "VS_WEIGHT_CAT"
  encoded <- encode_categorical(df_wt$weight_cat, prefix = prefix)

  empty_untransformed <- data.frame(
    subject_id = character(0), parameter_category_2 = character(0),
    timepoint_1_name = character(0), original_value = numeric(0),
    lower = numeric(0), upper = numeric(0),
    original_category = character(0), stringsAsFactors = FALSE
  )

  if (nrow(encoded) == 0) {
    return(list(
      data = data.frame(
        subject_id = character(0), parameter_id = character(0),
        timepoint_1_name = character(0), timepoint_2_name = character(0),
        timepoint_rank = numeric(0), result = numeric(0),
        baseline = numeric(0), stringsAsFactors = FALSE
      ),
      subjects = subjects,
      parameters = data.frame(
        parameter_id = character(0), parameter_name = character(0),
        parameter_category_1 = character(0), parameter_category_2 = character(0),
        parameter_category_3 = character(0), stringsAsFactors = FALSE
      ),
      untransformed = empty_untransformed
    ))
  }

  untransformed <- df_wt |>
    dplyr::transmute(
      subject_id = .data$subject_id,
      parameter_category_2 = .env$prefix,
      timepoint_1_name = .data$timepoint_1_name,
      original_value = .data$weight,
      lower = NA_real_,
      upper = NA_real_,
      original_category = .data$weight_cat
    )

  data <- data.frame(
    subject_id = df_wt$subject_id[encoded$orig_row],
    parameter_id = encoded$level,
    timepoint_1_name = df_wt$timepoint_1_name[encoded$orig_row],
    timepoint_rank = df_wt$timepoint_rank[encoded$orig_row],
    result = as.numeric(encoded$encoded),
    timepoint_2_name = NA_character_,
    baseline = NA_real_,
    stringsAsFactors = FALSE
  )

  levels <- sort(unique(encoded$level))
  params <- data.frame(
    parameter_id = levels,
    parameter_name = paste("Weight Category:", sub(".*=", "", levels)),
    parameter_category_1 = "Vital Signs",
    parameter_category_2 = prefix,
    parameter_category_3 = "bar",
    stringsAsFactors = FALSE
  )

  list(data = data, subjects = subjects, parameters = params,
       untransformed = untransformed)
}


#' Combine multiple ctas input lists
#'
#' Takes one or more ctas input lists (each with `data`, `subjects`,
#' `parameters`) and merges them into a single ctas-compatible list.
#' Deduplicates subjects and adds empty `custom_timeseries` and
#' `custom_reference_groups` tibbles. If inputs include `untransformed`
#' elements, these are row-bound into a combined `untransformed` data frame.
#'
#' @param ... One or more lists with elements `data`, `subjects`, `parameters`,
#'   and optionally `untransformed`.
#'
#' @return A list with elements: `data`, `subjects`, `parameters`,
#'   `custom_timeseries`, `custom_reference_groups`, and `untransformed`
#'   (NULL when no inputs contain untransformed data).
#' @export
combine_ctas_input <- function(...) {
  inputs <- list(...)

  data <- dplyr::bind_rows(lapply(inputs, `[[`, "data"))
  subjects <- dplyr::bind_rows(lapply(inputs, `[[`, "subjects")) |>
    dplyr::distinct()
  parameters <- dplyr::bind_rows(lapply(inputs, `[[`, "parameters")) |>
    dplyr::mutate(
      time_point_count_min = NA_real_,
      subject_count_min = NA_real_,
      max_share_missing = NA_real_,
      generate_change_from_baseline = NA,
      timeseries_features_to_calculate = NA_character_,
      use_only_custom_timeseries = NA
    )

  untransformed_list <- lapply(inputs, `[[`, "untransformed")
  has_untransformed <- !vapply(untransformed_list, is.null, logical(1))
  untransformed <- if (any(has_untransformed)) {
    dplyr::bind_rows(untransformed_list[has_untransformed])
  } else {
    NULL
  }

  list(
    data = data,
    subjects = subjects,
    parameters = parameters,
    custom_timeseries = data.frame(
      timeseries_id = character(0),
      parameter_id = character(0),
      timepoint_combo = character(0),
      stringsAsFactors = FALSE
    ),
    custom_reference_groups = data.frame(
      parameter_id = character(0),
      feature = character(0),
      ref_group = character(0),
      stringsAsFactors = FALSE
    ),
    untransformed = untransformed
  )
}
