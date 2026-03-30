# Plot single-timepoint categorical data as a bar chart

For parameters with a single timepoint (e.g. screening weight category).
Dispatches to stacked bar or CI bar depending on number of timepoints.

## Usage

``` r
plot_bar(param_ids, df_measures, thresh = 0, sites = NULL, visit_order = NULL)
```

## Arguments

- param_ids:

  Character vector of one-hot encoded `parameter_id` values.

- df_measures:

  Data frame as returned by
  [`prepare_measures()`](https://IMPALA-Consortium.github.io/ctasapp/reference/prepare_measures.md).

- thresh:

  Numeric, score threshold for highlighting.

- sites:

  Character vector of site IDs to show. If NULL, auto-selected.

- visit_order:

  Character vector of visit labels in desired x-axis order, or NULL for
  default (timepoint_rank) ordering.

## Value

A ggplot object.
