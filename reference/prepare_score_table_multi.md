# Prepare wide score table for multiple parameter IDs

Like
[`prepare_score_table()`](https://IMPALA-Consortium.github.io/ctasapp/reference/prepare_score_table.md)
but accepts a vector of parameter IDs, pooling scores across them. Used
for grouped categorical/bar parameters.

## Usage

``` r
prepare_score_table_multi(ctas_results, parameter_ids, features = NULL)
```

## Arguments

- ctas_results:

  List as returned by
  [`ctas::process_a_study()`](https://rdrr.io/pkg/ctas/man/process_a_study.html).

- parameter_ids:

  Character vector of parameter IDs.

- features:

  Character vector of feature names to include, or NULL for all.

## Value

A data frame with columns: site, one per feature, max_score.
