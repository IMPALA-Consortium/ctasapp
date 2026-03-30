# Combine multiple ctas input lists

Takes one or more ctas input lists (each with `data`, `subjects`,
`parameters`) and merges them into a single ctas-compatible list.
Deduplicates subjects and adds empty `custom_timeseries` and
`custom_reference_groups` tibbles. If inputs include `untransformed`
elements, these are row-bound into a combined `untransformed` data
frame.

## Usage

``` r
combine_ctas_input(...)
```

## Arguments

- ...:

  One or more lists with elements `data`, `subjects`, `parameters`, and
  optionally `untransformed`.

## Value

A list with elements: `data`, `subjects`, `parameters`,
`custom_timeseries`, `custom_reference_groups`, and `untransformed`
(NULL when no inputs contain untransformed data).
