# Map a numeric score to its graduated colour

Uses [`findInterval()`](https://rdrr.io/r/base/findInterval.html) and
[SCORE_BREAKS](https://IMPALA-Consortium.github.io/ctasapp/reference/score_colors.md)
to bucket scores and return the matching colour from
[SCORE_COLORS_PLOT](https://IMPALA-Consortium.github.io/ctasapp/reference/score_colors.md).

## Usage

``` r
score_to_color(score)
```

## Arguments

- score:

  Numeric vector of scores.

## Value

Character vector of hex / named colours.
