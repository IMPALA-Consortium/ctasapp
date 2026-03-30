# Read a single uploaded file into a data frame

Dispatches on file extension: `.csv` uses
[`utils::read.csv()`](https://rdrr.io/r/utils/read.table.html),
`.parquet` uses
[`arrow::read_parquet()`](https://arrow.apache.org/docs/r/reference/read_parquet.html),
`.rda`/`.rdata` loads the first data frame found in the file.

## Usage

``` r
read_upload_file(path, name)
```

## Arguments

- path:

  Path to the temporary uploaded file.

- name:

  Original filename (used for extension detection).

## Value

A data frame, or stops with an informative error.
