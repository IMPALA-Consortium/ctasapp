# Determine plot type from a vector of category_3 values

Mixed numeric types (range_normalized + ratio_missing) map to "numeric".
Single types pass through.

## Usage

``` r
determine_plot_type(cat3)
```

## Arguments

- cat3:

  Character vector of parameter_category_3 values.

## Value

Character scalar plot type.
