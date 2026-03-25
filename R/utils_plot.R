#' Plot numeric timeseries for a parameter
#'
#' Adapted from `applytsoa::report_plot_measure_val_numeric()`.
#' Background: grey patient lines for non-flagged sites.
#' Foreground: colored patient lines for selected/flagged sites,
#' orange if above threshold, blue otherwise.
#'
#' @param param Character scalar, the `parameter_id` to plot.
#' @param df_measures Data frame as returned by [prepare_measures()].
#' @param thresh Numeric, score threshold for flagging. Default: 0.
#' @param sites Character vector of site IDs to highlight. If NULL, picks
#'   all sites above threshold plus the top 3 by score.
#'
#' @return A ggplot2/patchwork object.
#' @export
plot_timeseries <- function(param, df_measures, thresh = 0, sites = NULL) {

  df <- df_measures |>
    dplyr::filter(.data$parameter_id == .env$param) |>
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

  param_name <- unique(df$parameter_name)

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
    plot_timeseries_panel(df, thresh = thresh, sites = site_group)
  })

  combined <- Reduce(`/`, plot_list) +
    patchwork::plot_annotation(
      title = paste(param, "-", param_name),
      subtitle = paste0(
        "orange: sites with outlier score > ", thresh,
        "\nblue: sites at or below threshold"
      )
    ) +
    patchwork::plot_layout(axes = "collect")

  combined
}


#' Single panel of timeseries plot for a group of sites
#' @param df Data frame, pre-filtered to one parameter with site-level score columns.
#' @param thresh Numeric threshold.
#' @param sites Character vector of sites to facet.
#' @return A ggplot object.
#' @keywords internal
plot_timeseries_panel <- function(df, thresh, sites) {

  df_bland <- df |>
    dplyr::filter(.data$max_score_site <= .env$thresh) |>
    dplyr::mutate(
      upper_thresh = stats::quantile(.data$result, 0.75, na.rm = TRUE) +
        5 * stats::IQR(.data$result, na.rm = TRUE),
      lower_thresh = stats::quantile(.data$result, 0.25, na.rm = TRUE) -
        5 * stats::IQR(.data$result, na.rm = TRUE),
      result_capped = dplyr::case_when(
        .data$result > .data$upper_thresh ~ .data$upper_thresh,
        .data$result < .data$lower_thresh ~ .data$lower_thresh,
        TRUE ~ .data$result
      ),
      result_outlier = dplyr::case_when(
        .data$result > .data$upper_thresh ~ .data$result_capped,
        .data$result < .data$lower_thresh ~ .data$result_capped,
        TRUE ~ NA_real_
      )
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

  p <- p +
    ggplot2::geom_line(
      data = df_out,
      ggplot2::aes(.data$timepoint_rank, .data$result, group = .data$subject_id, color = .data$pt_color)
    ) +
    ggplot2::geom_point(
      data = df_out,
      ggplot2::aes(.data$timepoint_rank, .data$result, color = .data$pt_color)
    ) +
    ggplot2::facet_wrap(~ .data$label, scales = "free_y", ncol = length(sites)) +
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
