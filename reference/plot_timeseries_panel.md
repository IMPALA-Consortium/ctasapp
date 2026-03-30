# Single panel of timeseries plot for a group of sites

Facets by `parameter_name ~ site_label` so that norm and ratio-missing
companions appear as separate rows within the same panel.

## Usage

``` r
plot_timeseries_panel(
  df,
  thresh,
  sites,
  has_range_norm = FALSE,
  query_data = NULL
)
```

## Arguments

- df:

  Data frame, pre-filtered with site-level score columns.

- thresh:

  Numeric threshold.

- sites:

  Character vector of sites to facet.

- has_range_norm:

  Logical, whether to add 0/1 reference lines.

- query_data:

  Optional query data frame for dot overlay.

## Value

A ggplot object.
