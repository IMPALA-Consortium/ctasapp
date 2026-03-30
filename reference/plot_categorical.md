# Plot categorical timeseries as alluvial diagram

Uses
[`ggalluvial::geom_flow()`](http://corybrunson.github.io/ggalluvial/reference/geom_flow.md)
and `geom_stratum()` to show how patients transition between categorical
response levels across timepoints. Falls back to bar charts if the
alluvial plot fails. Matches applytsoa's
`report_plot_measure_val_cat()` + `report_plot_cat_alluvial()`.

## Usage

``` r
plot_categorical(
  param_ids,
  df_measures,
  thresh = 0,
  sites = NULL,
  visit_order = NULL
)
```

## Arguments

- param_ids:

  Character vector of one-hot encoded `parameter_id` values for this
  categorical field.

- df_measures:

  Data frame as returned by
  [`prepare_measures()`](https://IMPALA-Consortium.github.io/ctasapp/reference/prepare_measures.md).

- thresh:

  Numeric, score threshold for flagging.

- sites:

  Character vector of site IDs to show. If NULL, auto-selected.

- visit_order:

  Character vector of visit labels in desired x-axis order, or NULL for
  default (timepoint_rank) ordering.

## Value

A ggplot object.
