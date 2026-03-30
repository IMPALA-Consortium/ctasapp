# Map plot type to a Font Awesome icon name

Uses raw `parameter_category_3` values to distinguish range-normalized
labs (flask) from plain numeric timeseries (chart-line). When a config
has been applied via
[`apply_config()`](https://IMPALA-Consortium.github.io/ctasapp/reference/apply_config.md),
the icon mapping is read from
[`get_param_icons()`](https://IMPALA-Consortium.github.io/ctasapp/reference/get_param_icons.md).

## Usage

``` r
plot_type_icon(plot_type, cat3_values)
```

## Arguments

- plot_type:

  Character scalar from
  [`build_param_lookup()`](https://IMPALA-Consortium.github.io/ctasapp/reference/build_param_lookup.md).

- cat3_values:

  Character vector of raw `parameter_category_3` values.

## Value

Character scalar Font Awesome icon name.
