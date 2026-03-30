# Build subject table from DM domain

Build subject table from DM domain

## Usage

``` r
build_subjects(
  dfDM,
  strSubjectCol = "USUBJID",
  strSiteCol = "SITEID",
  strCountryCol = "COUNTRY"
)
```

## Arguments

- dfDM:

  Data frame with SDTM DM columns (USUBJID, SITEID, COUNTRY, etc.).

- strSubjectCol:

  Column name for subject ID. Default: `"USUBJID"`.

- strSiteCol:

  Column name for site. Default: `"SITEID"`.

- strCountryCol:

  Column name for country. Default: `"COUNTRY"`.

## Value

Data frame with columns: subject_id, site, country, region.
