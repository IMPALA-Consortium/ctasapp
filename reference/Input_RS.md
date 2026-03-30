# Create ctas input from SDTM RS oncology domain

One-hot encodes `RSORRES` for the overall response test, producing one
parameter per response level. Filters unscheduled visits and
recalculates timepoint ranks.

## Usage

``` r
Input_RS(dfDM, dfRS, strTestCD = "OVRLRESP")
```

## Arguments

- dfDM:

  Data frame with SDTM DM columns.

- dfRS:

  Data frame with SDTM RS columns (e.g. `rs_onco`).

- strTestCD:

  Test code to filter on. Default: `"OVRLRESP"`.

## Value

A list with elements `data`, `subjects`, `parameters`, plus
`untransformed` with original categorical response values.
