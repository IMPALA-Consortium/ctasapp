# Plot numeric timeseries for one or more parameters

Adapted from applytsoa's `report_plot_measure_val_numeric()` /
`plot_num()`. When multiple parameter_ids are passed (e.g. a
range-normalized lab + its ratio-missing companion), they are faceted by
`parameter_name ~ site_label` so they appear as rows within the same
panel.

## Usage

``` r
plot_timeseries(
  param_ids,
  df_measures,
  thresh = 0,
  sites = NULL,
  query_data = NULL
)
```

## Arguments

- param_ids:

  Character vector of `parameter_id` values to include.

- df_measures:

  Data frame as returned by
  [`prepare_measures()`](https://IMPALA-Consortium.github.io/ctasapp/reference/prepare_measures.md).

- thresh:

  Numeric, score threshold for flagging. Default: 0.

- sites:

  Character vector of site IDs to highlight. If NULL, auto-selected.

- query_data:

  Optional data frame of query records. When provided, queried data
  points are overlaid with medium purple (no change) or indigo (data
  change) dots.

## Value

A ggplot2/patchwork object.
