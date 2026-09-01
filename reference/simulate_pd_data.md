# Simulate protocol deviation (SDTM DV) records for a ctas input dataset

Generates plausible protocol deviation rows for each site in
`ctas_data$subjects`. Deviations link at site level only — they are not
tied to a `parameter_id`. Columns follow lowercase snake_case
equivalents of the SDTM DV domain (see the CDISC SDTM Implementation
Guide, DV domain).

## Usage

``` r
simulate_pd_data(
  ctas_data,
  seed = 321,
  pds_per_site = c(1L, 5L),
  date_range = c("2023-01-01", "2024-12-31")
)
```

## Arguments

- ctas_data:

  A ctas input list containing a `subjects` element with `site` and
  `subject_id` columns.

- seed:

  Integer seed for reproducibility.

- pds_per_site:

  Integer vector of length 2 giving the inclusive min/max number of PD
  rows to generate per site (uniform draw).

- date_range:

  Character vector of length 2 with the inclusive ISO date range
  (`YYYY-MM-DD`) used to sample `dv_start_date`.

## Value

A data frame with columns `site`, `subject_id`, `dv_seq`, `dv_term`,
`dv_decod`, `dv_cat`, `dv_scat`, `dv_start_date`, `dv_end_date`.
