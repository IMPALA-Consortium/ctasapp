# Inject random missingness into LB data for specific site/parameter combos

Useful for creating demo datasets that showcase the missingness ratio
visualization. Affected sites are renamed with a `_MISS_<LBTESTCD>`
suffix so they are visually identifiable in the app.

## Usage

``` r
inject_missingness(dfDM, dfLB, injections, seed = 42)
```

## Arguments

- dfDM:

  Data frame with SDTM DM columns (must include USUBJID, SITEID).

- dfLB:

  Data frame with SDTM LB columns (must include USUBJID, LBTESTCD,
  LBSTRESN).

- injections:

  Data frame with columns `site` (character), `lbtestcd` (character),
  and `frac` (numeric 0-1, fraction of values to set NA).

- seed:

  Integer seed for reproducibility.

## Value

A list with elements `dm` and `lb` containing the modified data frames.
