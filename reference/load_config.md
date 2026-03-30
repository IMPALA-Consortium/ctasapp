# Load app configuration from a YAML file

Reads a YAML config file and merges with built-in defaults so that
missing keys fall back gracefully.

## Usage

``` r
load_config(path = NULL)
```

## Arguments

- path:

  Path to a YAML file. When `NULL` (default), uses the config shipped
  with the package (`inst/config.yml`).

## Value

A nested list with elements `colors` and `features`.
