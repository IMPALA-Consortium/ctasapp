# Sort visit names using natural numeric ordering

Extracts the last number from each visit name and sorts by the text
prefix first, then numerically. Visits without numbers sort
alphabetically among themselves ahead of numbered visits with the same
prefix.

## Usage

``` r
sort_visits_natural(x)
```

## Arguments

- x:

  Character vector of visit names.

## Value

Character vector, sorted in natural order.
