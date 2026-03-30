# Normalize values by reference range

Computes `(value - lower) / (upper - lower)`. Values in the normal range
fall between 0 and 1; below-range values are negative, above-range are
\> 1. Returns `NA` where the range is invalid (non-positive width or
missing bounds).

## Usage

``` r
normalize_by_range(value, lower, upper)
```

## Arguments

- value:

  Numeric vector of observed values.

- lower:

  Numeric vector of lower reference bounds.

- upper:

  Numeric vector of upper reference bounds.

## Value

Numeric vector, same length as `value`.
