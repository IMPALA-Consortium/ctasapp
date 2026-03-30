# Score colour constants

Breakpoints and colour vectors for DT table styling and ggplot2 plot
colouring. Below-threshold uses soft green; outlier bands graduate from
yellow to red. `SCORE_COLORS_TABLE` uses white for the first band and
yellow-to-red for outliers. `SCORE_COLORS_TABLE_TEXT` provides
foreground colours for DT cells. Query dots use light purple (no change)
and blue (data change). These can be overridden at runtime via
[`apply_config()`](https://IMPALA-Consortium.github.io/ctasapp/reference/apply_config.md).

## Usage

``` r
SCORE_BREAKS

SCORE_COLORS_PLOT

SCORE_COLORS_TABLE

SCORE_COLORS_TABLE_TEXT
```

## Format

An object of class `numeric` of length 4.

An object of class `character` of length 5.

An object of class `character` of length 5.

An object of class `character` of length 5.
