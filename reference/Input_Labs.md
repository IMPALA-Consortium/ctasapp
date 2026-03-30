# Create ctas input from SDTM LB domain

Produces range-normalized lab timeseries and ratio-missing timeseries
for each lab test. Both share the same `parameter_category_2` (the
LBTESTCD) so they are grouped together in the app sidebar and plotted in
the same faceted panel. Timepoint ranks are recalculated and unscheduled
visits are filtered out.

## Usage

``` r
Input_Labs(dfDM, dfLB)
```

## Arguments

- dfDM:

  Data frame with SDTM DM columns.

- dfLB:

  Data frame with SDTM LB columns.

## Value

A list with elements `data`, `subjects`, `parameters` matching the ctas
input schema, plus `untransformed` with original lab values and ranges.
