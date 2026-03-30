# Split parameter_ids into regular vs missingness groups

Uses the measures data frame to classify each parameter_id based on its
`parameter_category_3` value.

## Usage

``` r
split_param_ids(param_ids, df_measures)
```

## Arguments

- param_ids:

  Character vector of parameter_ids.

- df_measures:

  Measures data frame.

## Value

A list with elements `regular` and `missingness`.
