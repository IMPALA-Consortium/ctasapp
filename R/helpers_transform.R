#' Normalize values by reference range
#'
#' Computes `(value - lower) / (upper - lower)`. Values in the normal range
#' fall between 0 and 1; below-range values are negative, above-range are > 1.
#' Returns `NA` where the range is invalid (non-positive width or missing bounds).
#'
#' @param value Numeric vector of observed values.
#' @param lower Numeric vector of lower reference bounds.
#' @param upper Numeric vector of upper reference bounds.
#'
#' @return Numeric vector, same length as `value`.
#' @export
normalize_by_range <- function(value, lower, upper) {
  range_width <- upper - lower
  invalid <- is.na(range_width) | range_width <= 0
  result <- (value - lower) / range_width
  result[invalid] <- NA_real_
  result
}


#' Running ratio of missing values over time
#'
#' For each group defined by `subject_id` and `parameter_id` (in row order),
#' computes a cumulative proportion of `NA` values in `value`.
#' Row \eqn{k} gets `sum(is.na(value[1:k])) / k`.
#'
#' @param value Numeric vector of observed values (may contain `NA`).
#' @param subject_id Character vector identifying subjects.
#' @param parameter_id Character vector identifying parameters.
#'
#' @return Numeric vector, same length as `value`, with the running missing ratio.
#' @export
ratio_missing_over_time <- function(value, subject_id, parameter_id) {
  df <- data.frame(
    value = value,
    subject_id = subject_id,
    parameter_id = parameter_id,
    stringsAsFactors = FALSE
  )
  df$is_missing <- as.numeric(is.na(df$value))
  df$row_idx <- seq_len(nrow(df))

  df <- df |>
    dplyr::mutate(
      timepoint_rank = dplyr::row_number(),
      cum_missing = cumsum(.data$is_missing),
      ratio = .data$cum_missing / .data$timepoint_rank,
      .by = c("subject_id", "parameter_id")
    )

  df$ratio[order(df$row_idx)]
}


#' Recalculate timepoint rank as positional integer
#'
#' Computes `row_number()` within each `(subject_id, parameter_id)` group,
#' ordered by `timepoint_1_name`. This replaces raw `VISITNUM` values with
#' a meaningful sequential rank suitable for x-axis plotting.
#' Matches applytsoa's `tbl_measures.R` rank calculation.
#'
#' @param df Data frame with at least columns `subject_id`, `parameter_id`,
#'   `timepoint_1_name`, and `timepoint_rank`.
#'
#' @return The input data frame with `timepoint_rank` replaced by recalculated
#'   positional integers.
#' @export
recalculate_timepoint_rank <- function(df) {
  df |>
    dplyr::arrange(.data$subject_id, .data$parameter_id, .data$timepoint_1_name) |>
    dplyr::mutate(
      timepoint_rank = dplyr::row_number(),
      .by = c("subject_id", "parameter_id")
    )
}


#' Arrange visit names into clinically meaningful order
#'
#' Orders factor levels so that screening visits come first, discontinuation
#' visits come last, and all others are sorted alphabetically in between.
#' Ported from applytsoa's `report_arrange_timepoints()`.
#'
#' @param x Character vector of visit names.
#'
#' @return A factor with levels ordered: screening, sorted others, discontinuation.
#' @export
arrange_timepoints <- function(x) {
  lvls <- unique(x)
  screening <- lvls[grepl("screen", tolower(lvls))]
  discont <- lvls[grepl("disc", tolower(lvls))]
  others <- lvls[!lvls %in% c(screening, discont)]
  lvls <- c(sort(screening), sort(others), sort(discont))
  factor(x, levels = lvls)
}


#' One-hot encode a categorical variable
#'
#' Expands a categorical vector into a long data frame with one row per
#' original observation per level. The `encoded` column is 1 where the
#' observation matches the level, 0 otherwise. `NA` values in the input
#' are dropped.
#'
#' @param value Character vector of categorical values.
#' @param prefix Character prefix for the level names (e.g. `"RS_OVRLRESP"`).
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{orig_row}{Integer, the row index of the original observation.}
#'     \item{level}{Character, `"{prefix}={value}"` label.}
#'     \item{encoded}{Integer, 1 if the observation matches this level, 0 otherwise.}
#'   }
#' @export
encode_categorical <- function(value, prefix = "var") {
  non_na_idx <- which(!is.na(value))
  if (length(non_na_idx) == 0) {
    return(data.frame(
      orig_row = integer(0),
      level = character(0),
      encoded = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  levels <- sort(unique(value[non_na_idx]))

  rows <- lapply(levels, function(lvl) {
    data.frame(
      orig_row = non_na_idx,
      level = paste0(prefix, "=", lvl),
      encoded = as.integer(value[non_na_idx] == lvl),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}
