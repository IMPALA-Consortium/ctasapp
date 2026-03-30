# Render a score DT table with shared colour constants

Render a score DT table with shared colour constants

## Usage

``` r
render_score_dt(scores_display, thresh)
```

## Arguments

- scores_display:

  Data frame from
  [`prepare_score_table_multi()`](https://IMPALA-Consortium.github.io/ctasapp/reference/prepare_score_table_multi.md).

- thresh:

  Numeric threshold for outlier column.

## Value

A DT datatable object.
