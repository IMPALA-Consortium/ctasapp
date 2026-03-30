# Compute x-axis visit levels for categorical/bar plots

Returns the ordered visit labels as they appear on the plot axis,
including duplicate-handling expansions for categorical data (e.g. "WEEK
6" becomes "WEEK 6 01", "WEEK 6 02" when a patient has multiple
observations at that visit).

## Usage

``` r
get_plot_visit_levels(param_ids, df_measures, plot_type = "categorical")
```

## Arguments

- param_ids:

  Character vector of parameter_id values.

- df_measures:

  Data frame as returned by
  [`prepare_measures()`](https://IMPALA-Consortium.github.io/ctasapp/reference/prepare_measures.md).

- plot_type:

  Either `"categorical"` or `"bar"`.

## Value

Character vector of visit labels in `timepoint_rank` order.
