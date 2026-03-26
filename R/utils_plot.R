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
#'
#' @return A ggplot2/patchwork object.
#' @export
plot_timeseries <- function(param_ids, df_measures, thresh = 0, sites = NULL) {

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
                          has_range_norm = has_range_norm)
  })

  param_cat2 <- unique(df$parameter_category_2)
  param_cat1 <- unique(df$parameter_category_1)
  title_str <- paste(param_cat2, "-", param_cat1)

  combined <- Reduce(`/`, plot_list) +
    patchwork::plot_annotation(
      title = title_str,
      subtitle = paste0(
        "orange: sites with outlier score > ", thresh,
        "\nblue: sites at or below threshold"
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
#' @return A ggplot object.
#' @keywords internal
plot_timeseries_panel <- function(df, thresh, sites, has_range_norm = FALSE) {

  df_bland <- df |>
    dplyr::filter(.data$max_score_site <= .env$thresh) |>
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
      pt_color = ifelse(.data$max_score_site > .env$thresh, "orange1", "royalblue"),
      smooth_color = ifelse(.data$max_score_site > .env$thresh, "khaki3", "skyblue2")
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
#'
#' @return A ggplot object.
#' @export
plot_categorical <- function(param_ids, df_measures, thresh = 0, sites = NULL) {

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

  # Group sites: flagged get asterisk, unflagged collapse to "unflagged"
  df_gr <- df |>
    dplyr::filter(.data$max_score_site > .env$thresh | .data$site %in% .env$sites) |>
    dplyr::mutate(
      site_label = dplyr::case_when(
        .data$site %in% .env$sites & .data$max_score_site > .env$thresh ~
          paste(.data$site, "*"),
        .data$site %in% .env$sites ~ .data$site,
        TRUE ~ "unflagged"
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
    dplyr::mutate(
      timepoint_1_name = arrange_timepoints(.data$timepoint_1_name)
    ) |>
    dplyr::distinct(.data$subject_id, .data$site_label, .data$timepoint_1_name,
                    .data$timepoint_rank, .data$val_cat) |>
    dplyr::arrange(.data$subject_id, .data$timepoint_1_name)

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
#'
#' @return A ggplot object.
#' @export
plot_bar <- function(param_ids, df_measures, thresh = 0, sites = NULL) {

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
    dplyr::filter(.data$site %in% .env$sites) |>
    dplyr::mutate(
      site_label = dplyr::case_when(
        .data$max_score_site > .env$thresh ~ paste(.data$site, "*"),
        TRUE ~ .data$site
      ),
      timepoint_1_name = arrange_timepoints(.data$timepoint_1_name)
    ) |>
    dplyr::distinct(.data$subject_id, .data$site_label, .data$timepoint_1_name,
                    .data$timepoint_rank, .data$val_cat)

  if (dplyr::n_distinct(df_gr$timepoint_1_name) > 3) {
    plot_cat_bar_stacked(df_gr)
  } else {
    plot_cat_bar_ci(df_gr)
  }
}
