# Running ratio of missing values over time

For each group defined by `subject_id` and `parameter_id` (in row
order), computes a cumulative proportion of `NA` values in `value`. Row
\\k\\ gets `sum(is.na(value[1:k])) / k`.

## Usage

``` r
ratio_missing_over_time(value, subject_id, parameter_id)
```

## Arguments

- value:

  Numeric vector of observed values (may contain `NA`).

- subject_id:

  Character vector identifying subjects.

- parameter_id:

  Character vector identifying parameters.

## Value

Numeric vector, same length as `value`, with the running missing ratio.
