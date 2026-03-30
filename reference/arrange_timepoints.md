# Arrange visit names into clinically meaningful order

Orders factor levels: screening first, then baseline, then remaining
visits in natural numeric order (so "WEEK 2" \< "WEEK 12"), then
discontinuation last. Uses
[`sort_visits_natural()`](https://IMPALA-Consortium.github.io/ctasapp/reference/sort_visits_natural.md)
for the middle group to avoid lexical misordering of numbered visit
names.

## Usage

``` r
arrange_timepoints(x)
```

## Arguments

- x:

  Character vector of visit names.

## Value

A factor with levels ordered: screening, baseline, numbered visits,
discontinuation.
