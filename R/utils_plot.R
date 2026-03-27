# Shared colour constants --------------------------------------------------
# Used in both DT table styling and ggplot2 point/line colouring.

#' Score colour constants
#'
#' Breakpoints and colour vectors for DT table styling and ggplot2 plot
#' colouring. Below-threshold uses soft green; outlier bands graduate from
#' yellow to red. `SCORE_COLORS_TABLE` uses white for the first band and
#' yellow-to-red for outliers. `SCORE_COLORS_TABLE_TEXT` provides foreground
#' colours for DT cells. Query dots use light purple (no change) and blue
#' (data change). These can be overridden at runtime via [apply_config()].
#'
#' @name score_colors
#' @export
SCORE_BREAKS <- c(1.3, 3, 5, 10)

#' @rdname score_colors
#' @export
SCORE_COLORS_PLOT <- c("#9ED782", "#fed8019c", "#fed801", "#FEAA01", "#FF5858")

#' @rdname score_colors
#' @export
SCORE_COLORS_TABLE <- c("#FFFFFF", "#feed01", "#fed801", "#FEAA01", "#FF5858")

#' @rdname score_colors
#' @export
SCORE_COLORS_TABLE_TEXT <- c("#1A1A1A", "#1A1A1A", "#1A1A1A", "#FFFFFF", "#FFFFFF")


#' Map a numeric score to its graduated colour
#'
#' Uses [findInterval()] and [SCORE_BREAKS] to bucket scores and return
#' the matching colour from [SCORE_COLORS_PLOT].
#'
#' @param score Numeric vector of scores.
#' @return Character vector of hex / named colours.
#' @export
score_to_color <- function(score) {
  brks <- get_score_breaks()
  cols <- get_score_colors_plot()
  idx <- findInterval(score, brks) + 1L
  cols[idx]
}


#' Plot numeric timeseries for one or more parameters
#'
#' Adapted from applytsoa's `report_plot_measure_val_numeric()` / `plot_num()`.
#' When multiple parameter_ids are passed (e.g. a range-normalized lab + its
#' ratio-missing companion), they are faceted by `parameter_name ~ site_label`
#' so they appear as rows within the same panel.
#'
#' @param param_ids Character vector of `parameter_id` values to include.
#' @param df_measures Data frame as returned by [prepare_measures()].
#' @param thresh Numeric, score threshold for flagging. Default: 0.
#' @param sites Character vector of site IDs to highlight. If NULL, auto-selected.
#' @param query_data Optional data frame of query records. When provided, queried
#'   data points are overlaid with medium purple (no change) or indigo
#'   (data change) dots.
#'
#' @return A ggplot2/patchwork object.
#' @export
plot_timeseries <- function(param_ids, df_measures, thresh = 0, sites = NULL,
                            query_data = NULL) {

  df <- df_measures |>
    dplyr::filter(.data$parameter_id %in% .env$param_ids) |>
    dplyr::mutate(
      max_score_site = max(.data$max_score, na.rm = TRUE),
      n_pat_site = dplyr::n_distinct(.data$subject_id),
      .by = "site"
    )

  if (nrow(df) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 1, y = 1, label = "No data for this parameter") +
        ggplot2::theme_void()
    )
  }

  # Factor-relevel parameter_name so the actual variable comes first,
  # "Ratio" measures come second (matching applytsoa's fct_relevel logic)
  var_names <- df |>
    dplyr::filter(!grepl("^Ratio ", .data$parameter_name)) |>
    dplyr::pull(.data$parameter_name) |>
    unique()

  df <- df |>
    dplyr::mutate(
      parameter_name = forcats::fct_relevel(.data$parameter_name, var_names)
    )

  # Detect if this field contains range-normalized data for ref lines
  has_range_norm <- any(df$parameter_category_3 == "range_normalized")

  if (is.null(sites)) {
    sites <- df |>
      dplyr::distinct(.data$site, .data$max_score_site) |>
      dplyr::arrange(dplyr::desc(.data$max_score_site)) |>
      dplyr::filter(.data$max_score_site > .env$thresh | dplyr::row_number() <= 3) |>
      dplyr::pull(.data$site)
  }

  n_groups <- ceiling(length(sites) / 6)
  group_vec <- rep(seq_len(n_groups), each = 6)[seq_along(sites)]

  plot_list <- lapply(split(sites, group_vec), function(site_group) {
    plot_timeseries_panel(df, thresh = thresh, sites = site_group,
                          has_range_norm = has_range_norm,
                          query_data = query_data)
  })

  param_cat2 <- unique(df$parameter_category_2)
  param_cat1 <- unique(df$parameter_category_1)
  title_str <- paste(param_cat2, "-", param_cat1)

  combined <- Reduce(`/`, plot_list) +
    patchwork::plot_annotation(
      title = title_str,
      subtitle = paste0(
        "orange to red: sites with outlier score > ", thresh, " (per parameter)",
        "\ngreen: sites at or below threshold"
      )
    ) +
    patchwork::plot_layout(axes = "collect")

  combined
}


#' Single panel of timeseries plot for a group of sites
#'
#' Facets by `parameter_name ~ site_label` so that norm and ratio-missing
#' companions appear as separate rows within the same panel.
#'
#' @param df Data frame, pre-filtered with site-level score columns.
#' @param thresh Numeric threshold.
#' @param sites Character vector of sites to facet.
#' @param has_range_norm Logical, whether to add 0/1 reference lines.
#' @param query_data Optional query data frame for dot overlay.
#' @return A ggplot object.
#' @keywords internal
plot_timeseries_panel <- function(df, thresh, sites, has_range_norm = FALSE,
                                  query_data = NULL) {

  # Per-parameter max_score determines bland/outlier colouring (applytsoa style)
  df_bland <- df |>
    dplyr::filter(.data$max_score <= .env$thresh) |>
    dplyr::mutate(
      upper_thresh = stats::quantile(.data$result, 0.75, na.rm = TRUE) +
        5 * stats::IQR(.data$result, na.rm = TRUE),
      lower_thresh = stats::quantile(.data$result, 0.25, na.rm = TRUE) -
        5 * stats::IQR(.data$result, na.rm = TRUE),
      result_capped = dplyr::case_when(
        dplyr::between(.data$result, 0, 1) ~ .data$result,
        .data$result > .data$upper_thresh ~ .data$upper_thresh,
        .data$result < .data$lower_thresh ~ .data$lower_thresh,
        TRUE ~ .data$result
      ),
      result_outlier = dplyr::case_when(
        dplyr::between(.data$result, 0, 1) ~ NA_real_,
        .data$result > .data$upper_thresh ~ .data$result_capped,
        .data$result < .data$lower_thresh ~ .data$result_capped,
        TRUE ~ NA_real_
      ),
      .by = "parameter_name"
    )

  df_out <- df |>
    dplyr::filter(.data$site %in% .env$sites) |>
    dplyr::mutate(
      label = paste0(
        .data$site,
        " - score: ", format_score(.data$max_score_site),
        " - n: ", .data$n_pat_site
      ),
      label = forcats::fct_reorder(.data$label, .data$max_score_site),
      label = forcats::fct_rev(.data$label),
      pt_color = score_to_color(.data$max_score),
      smooth_color = score_to_color(.data$max_score)
    )

  has_values <- !all(is.na(df_out$result))

  p <- ggplot2::ggplot(df_bland, ggplot2::aes(.data$timepoint_rank, .data$result_capped)) +
    ggplot2::geom_line(ggplot2::aes(group = .data$subject_id), color = "lightgrey") +
    ggplot2::geom_point(
      ggplot2::aes(y = .data$result_outlier),
      shape = 4, color = "grey", na.rm = TRUE
    )

  if (has_values) {
    p <- p +
      ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = "darkgrey", se = FALSE) +
      ggplot2::geom_smooth(
        data = df_out,
        ggplot2::aes(.data$timepoint_rank, .data$result, color = .data$smooth_color),
        method = "lm", formula = y ~ x, se = FALSE
      )
  }

  if (has_range_norm) {
    p <- p +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.4) +
      ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "grey40", linewidth = 0.4)
  }

  p <- p +
    ggplot2::geom_line(
      data = df_out,
      ggplot2::aes(.data$timepoint_rank, .data$result, group = .data$subject_id, color = .data$pt_color)
    ) +
    ggplot2::geom_point(
      data = df_out,
      ggplot2::aes(.data$timepoint_rank, .data$result, color = .data$pt_color)
    ) +
    ggplot2::facet_wrap(
      .data$parameter_name ~ .data$label,
      scales = "free_y", ncol = length(sites)
    ) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_fill_identity() +
    ggplot2::labs(x = "Timepoint", y = "Value") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")

  if (!is.null(query_data) && nrow(query_data) > 0) {
    df_q <- df_out |>
      dplyr::inner_join(
        query_data[, c("subject_id", "parameter_id",
                        "visit", "data_change")],
        by = c("subject_id", "parameter_id",
               timepoint_1_name = "visit"),
        relationship = "many-to-many"
      )
    if (nrow(df_q) > 0) {
      qcols <- get_query_colors()
      df_q$query_color <- ifelse(df_q$data_change, qcols$data_change,
                                 qcols$no_change)
      p <- p +
        ggplot2::geom_point(
          data = df_q,
          ggplot2::aes(.data$timepoint_rank, .data$result,
                       color = .data$query_color)
        )
    }
  }

  p
}


#' Format outlier score for display
#' @param score Numeric vector.
#' @return Character vector.
#' @keywords internal
format_score <- function(score) {
  dplyr::case_when(
    score > 10000 ~ "> 10000",
    round(score, 1) > 0 ~ as.character(round(score, 1)),
    round(score, 2) > 0 ~ as.character(round(score, 2)),
    round(score, 3) > 0 ~ as.character(round(score, 3)),
    score > 0 ~ "< 0.001",
    TRUE ~ as.character(round(score, 1))
  )
}


#' Compute x-axis visit levels for categorical/bar plots
#'
#' Returns the ordered visit labels as they appear on the plot axis,
#' including duplicate-handling expansions for categorical data (e.g.
#' "WEEK 6" becomes "WEEK 6 01", "WEEK 6 02" when a patient has
#' multiple observations at that visit).
#'
#' @param param_ids Character vector of parameter_id values.
#' @param df_measures Data frame as returned by [prepare_measures()].
#' @param plot_type Either `"categorical"` or `"bar"`.
#' @return Character vector of visit labels in `timepoint_rank` order.
#' @export
get_plot_visit_levels <- function(param_ids, df_measures, plot_type = "categorical") {
  df <- df_measures |>
    dplyr::filter(.data$parameter_id %in% .env$param_ids) |>
    dplyr::filter(.data$result == 1)

  if (nrow(df) == 0) return(character(0))

  if (plot_type == "categorical") {
    df <- df |>
      dplyr::mutate(
        n_per_tp = dplyr::n(),
        .by = c("subject_id", "timepoint_1_name")
      ) |>
      dplyr::mutate(
        timepoint_1_name = ifelse(
          .data$n_per_tp > 1,
          paste(
            .data$timepoint_1_name,
            stringr::str_pad(.data$timepoint_rank, width = 2,
                             side = "left", pad = "0")
          ),
          .data$timepoint_1_name
        )
      )
  }

  df |>
    dplyr::distinct(.data$timepoint_1_name, .data$timepoint_rank) |>
    dplyr::arrange(.data$timepoint_rank) |>
    dplyr::pull(.data$timepoint_1_name) |>
    unique()
}


#' Plot categorical timeseries as alluvial diagram
#'
#' Uses `ggalluvial::geom_flow()` and `geom_stratum()` to show how patients
#' transition between categorical response levels across timepoints. Falls
#' back to bar charts if the alluvial plot fails.
#' Matches applytsoa's `report_plot_measure_val_cat()` + `report_plot_cat_alluvial()`.
#'
#' @param param_ids Character vector of one-hot encoded `parameter_id` values
#'   for this categorical field.
#' @param df_measures Data frame as returned by [prepare_measures()].
#' @param thresh Numeric, score threshold for flagging.
#' @param sites Character vector of site IDs to show. If NULL, auto-selected.
#' @param visit_order Character vector of visit labels in desired x-axis order,
#'   or NULL for default (timepoint_rank) ordering.
#'
#' @return A ggplot object.
#' @export
plot_categorical <- function(param_ids, df_measures, thresh = 0, sites = NULL,
                             visit_order = NULL) {

  df <- df_measures |>
    dplyr::filter(.data$parameter_id %in% .env$param_ids) |>
    dplyr::filter(.data$result == 1) |>
    dplyr::mutate(
      val_cat = sub(".*=", "", .data$parameter_id),
      val_cat = stringr::str_trunc(.data$val_cat, 30),
      max_score_site = max(.data$max_score, na.rm = TRUE),
      .by = "site"
    )

  if (nrow(df) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 1, y = 1, label = "No data for this parameter") +
        ggplot2::theme_void()
    )
  }

  if (is.null(sites)) {
    sites <- df |>
      dplyr::distinct(.data$site, .data$max_score_site) |>
      dplyr::arrange(dplyr::desc(.data$max_score_site)) |>
      dplyr::filter(.data$max_score_site > .env$thresh | dplyr::row_number() <= 6) |>
      dplyr::pull(.data$site)
  }

  # Group sites: above threshold get asterisk, all others collapse to "unflagged"
  df_gr <- df |>
    dplyr::mutate(
      site_label = ifelse(
        .data$max_score_site > .env$thresh,
        paste(.data$site, "*"),
        "unflagged"
      )
    )

  # Handle duplicate timepoints per patient by appending padded rank
  df_plot <- df_gr |>
    dplyr::mutate(
      n_per_tp = dplyr::n(),
      .by = c("subject_id", "timepoint_1_name")
    ) |>
    dplyr::mutate(
      timepoint_1_name = ifelse(
        .data$n_per_tp > 1,
        paste(.data$timepoint_1_name, stringr::str_pad(.data$timepoint_rank, width = 2, side = "left", pad = "0")),
        .data$timepoint_1_name
      )
    ) |>
    dplyr::distinct(.data$subject_id, .data$site_label, .data$timepoint_1_name,
                    .data$timepoint_rank, .data$val_cat) |>
    dplyr::arrange(.data$subject_id, .data$timepoint_rank)

  # Order x-axis: use custom visit_order if provided, else timepoint_rank
  default_levels <- df_plot |>
    dplyr::distinct(.data$timepoint_1_name, .data$timepoint_rank) |>
    dplyr::arrange(.data$timepoint_rank) |>
    dplyr::pull(.data$timepoint_1_name) |>
    unique()

  if (!is.null(visit_order) &&
      length(visit_order) > 0 &&
      all(default_levels %in% visit_order)) {
    visit_levels <- visit_order[visit_order %in% default_levels]
  } else {
    visit_levels <- default_levels
  }
  df_plot$timepoint_1_name <- factor(
    df_plot$timepoint_1_name, levels = visit_levels
  )

  # Try alluvial, fall back to bar on error
  p_alluvial <- try_alluvial(df_plot)
  if (!is.null(p_alluvial)) return(p_alluvial)

  if (dplyr::n_distinct(df_plot$timepoint_1_name) > 3) {
    plot_cat_bar_stacked(df_plot)
  } else {
    plot_cat_bar_ci(df_plot)
  }
}


#' Attempt to render an alluvial plot
#' @param df_plot Prepared data frame with val_cat, subject_id, site_label, timepoint_1_name.
#' @return A ggplot if successful, NULL if it errors.
#' @keywords internal
try_alluvial <- function(df_plot) {
  p <- df_plot |>
    dplyr::mutate(val_cat = as.factor(.data$val_cat)) |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data$timepoint_1_name,
      stratum = .data$val_cat,
      alluvium = .data$subject_id,
      label = .data$val_cat,
      fill = .data$val_cat
    )) +
    ggalluvial::geom_flow() +
    ggalluvial::geom_stratum(color = NA) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
    ) +
    ggplot2::facet_wrap(~ .data$site_label, scales = "free_y") +
    ggplot2::labs(x = "Visit", y = "Count", fill = "Level")

  safe_print <- purrr::safely(print)
  grDevices::pdf(NULL)
  res <- safe_print(p)
  grDevices::dev.off()

  if (is.null(res$error)) p else NULL
}


#' Stacked bar chart for categorical data with many timepoints
#' @param df_plot Prepared data frame.
#' @return A ggplot object.
#' @keywords internal
plot_cat_bar_stacked <- function(df_plot) {
  df_plot |>
    dplyr::mutate(val_cat = as.factor(.data$val_cat)) |>
    dplyr::group_by(.data$site_label, .data$timepoint_1_name, .data$val_cat,
                    .drop = FALSE) |>
    dplyr::summarise(n_pat = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(
      n_total = sum(.data$n_pat),
      .by = c("site_label", "timepoint_1_name")
    ) |>
    dplyr::filter(.data$n_total > 0) |>
    ggplot2::ggplot(ggplot2::aes(.data$timepoint_1_name, .data$n_pat,
                                  fill = .data$val_cat)) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap(~ .data$site_label, scales = "free_y") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
    ) +
    ggplot2::labs(x = "Visit", y = "N Patients", fill = "Level")
}


#' Bar chart with 95% CI for categorical data with few timepoints
#' @param df_plot Prepared data frame.
#' @return A ggplot object.
#' @keywords internal
plot_cat_bar_ci <- function(df_plot) {
  df_summary <- df_plot |>
    dplyr::mutate(val_cat = as.factor(.data$val_cat)) |>
    dplyr::group_by(.data$site_label, .data$timepoint_1_name, .data$val_cat,
                    .drop = FALSE) |>
    dplyr::summarise(n_pat = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(
      n_total = sum(.data$n_pat, na.rm = TRUE),
      .by = c("site_label", "timepoint_1_name")
    ) |>
    dplyr::filter(.data$n_total > 0) |>
    dplyr::mutate(
      ratio = .data$n_pat / .data$n_total,
      ci95_low = purrr::map2_dbl(
        .data$n_pat, .data$n_total,
        ~ stats::prop.test(.x, .y)$conf.int[1]
      ),
      ci95_high = purrr::map2_dbl(
        .data$n_pat, .data$n_total,
        ~ stats::prop.test(.x, .y)$conf.int[2]
      )
    )

  ggplot2::ggplot(
    df_summary,
    ggplot2::aes(x = .data$site_label, y = .data$ratio, fill = .data$val_cat)
  ) +
    ggplot2::geom_col() +
    ggplot2::geom_pointrange(
      ggplot2::aes(ymin = .data$ci95_low, ymax = .data$ci95_high),
      color = "grey50"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(.data$n_pat, "/", .data$n_total)),
      color = "black", size = 3
    ) +
    ggplot2::facet_grid(.data$val_cat ~ .data$timepoint_1_name) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = "Site", y = "Ratio + 95% CI")
}


#' Plot single-timepoint categorical data as a bar chart
#'
#' For parameters with a single timepoint (e.g. screening weight category).
#' Dispatches to stacked bar or CI bar depending on number of timepoints.
#'
#' @param param_ids Character vector of one-hot encoded `parameter_id` values.
#' @param df_measures Data frame as returned by [prepare_measures()].
#' @param thresh Numeric, score threshold for highlighting.
#' @param sites Character vector of site IDs to show. If NULL, auto-selected.
#' @param visit_order Character vector of visit labels in desired x-axis order,
#'   or NULL for default (timepoint_rank) ordering.
#'
#' @return A ggplot object.
#' @export
plot_bar <- function(param_ids, df_measures, thresh = 0, sites = NULL,
                     visit_order = NULL) {

  df <- df_measures |>
    dplyr::filter(.data$parameter_id %in% .env$param_ids) |>
    dplyr::filter(.data$result == 1) |>
    dplyr::mutate(
      val_cat = sub(".*=", "", .data$parameter_id),
      max_score_site = max(.data$max_score, na.rm = TRUE),
      .by = "site"
    )

  if (nrow(df) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 1, y = 1, label = "No data for this parameter") +
        ggplot2::theme_void()
    )
  }

  if (is.null(sites)) {
    sites <- df |>
      dplyr::distinct(.data$site, .data$max_score_site) |>
      dplyr::arrange(dplyr::desc(.data$max_score_site)) |>
      dplyr::filter(.data$max_score_site > .env$thresh | dplyr::row_number() <= 6) |>
      dplyr::pull(.data$site)
  }

  df_gr <- df |>
    dplyr::mutate(
      site_label = ifelse(
        .data$max_score_site > .env$thresh,
        paste(.data$site, "*"),
        "unflagged"
      )
    ) |>
    dplyr::distinct(.data$subject_id, .data$site_label, .data$timepoint_1_name,
                    .data$timepoint_rank, .data$val_cat)

  default_levels <- df_gr |>
    dplyr::distinct(.data$timepoint_1_name, .data$timepoint_rank) |>
    dplyr::arrange(.data$timepoint_rank) |>
    dplyr::pull(.data$timepoint_1_name) |>
    unique()

  if (!is.null(visit_order) &&
      length(visit_order) > 0 &&
      all(default_levels %in% visit_order)) {
    visit_levels <- visit_order[visit_order %in% default_levels]
  } else {
    visit_levels <- default_levels
  }
  df_gr$timepoint_1_name <- factor(
    df_gr$timepoint_1_name, levels = visit_levels
  )

  if (dplyr::n_distinct(df_gr$timepoint_1_name) > 3) {
    plot_cat_bar_stacked(df_gr)
  } else {
    plot_cat_bar_ci(df_gr)
  }
}
