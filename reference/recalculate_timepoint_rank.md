# Recalculate timepoint rank as positional integer

Computes `row_number()` within each `(subject_id, parameter_id)` group,
ordered first by the original numeric `timepoint_rank` (typically
VISITNUM) and then by `timepoint_1_name` as a tiebreaker. This produces
a clean 1, 2, 3, ... sequence that respects the clinical visit ordering
rather than relying on lexical sorting of visit names (which miorders
e.g. "WEEK 12" before "WEEK 2").

## Usage

``` r
recalculate_timepoint_rank(df)
```

## Arguments

- df:

  Data frame with at least columns `subject_id`, `parameter_id`,
  `timepoint_1_name`, and `timepoint_rank` (original numeric visit
  number).

## Value

The input data frame with `timepoint_rank` replaced by recalculated
positional integers.
