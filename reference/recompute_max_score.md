# Recompute max_score in measures using a subset of ctas features

Recalculates `max_score` per (site, parameter_id) from
`ctas_results$site_scores`, optionally filtering to a subset of
features. Returns the measures data frame with an updated `max_score`
column.

## Usage

``` r
recompute_max_score(measures, ctas_results, features = NULL)
```

## Arguments

- measures:

  Data frame as returned by
  [`prepare_measures()`](https://IMPALA-Consortium.github.io/ctasapp/reference/prepare_measures.md).

- ctas_results:

  List as returned by
  [`ctas::process_a_study()`](https://rdrr.io/pkg/ctas/man/process_a_study.html).

- features:

  Character vector of feature names to include, or NULL for all.

## Value

A data frame identical to `measures` but with recalculated `max_score`.
