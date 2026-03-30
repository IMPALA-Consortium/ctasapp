# Build display parameter lookup from measures

Groups parameters by `parameter_category_2` (the "field" key). This
naturally groups norm + missing-ratio labs under a single entry, and
categorical one-hot levels under their shared prefix. The `plot_type` is
determined by the dominant `parameter_category_3`; mixed types (e.g.
`range_normalized` + `ratio_missing`) are treated as `"numeric"`.

## Usage

``` r
build_param_lookup(df)
```

## Arguments

- df:

  Measures data frame.

## Value

A data frame with `display_id`, `parameter_ids` (list column),
`plot_type`, and `cat3_values` (list column).

## Details

When multiple parameters share a `parameter_category_2` but have
identical `parameter_category_3` values that aren't meant to be grouped
(e.g. two independent numeric params with the same generic category),
they are kept separate using `parameter_id` as the display key.

Now also includes a `cat3_values` list column with raw
`parameter_category_3` values for icon determination.
