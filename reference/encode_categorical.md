# One-hot encode a categorical variable

Expands a categorical vector into a long data frame with one row per
original observation per level. The `encoded` column is 1 where the
observation matches the level, 0 otherwise. `NA` values in the input are
dropped.

## Usage

``` r
encode_categorical(value, prefix = "var")
```

## Arguments

- value:

  Character vector of categorical values.

- prefix:

  Character prefix for the level names (e.g. `"RS_OVRLRESP"`).

## Value

A data frame with columns:

- orig_row:

  Integer, the row index of the original observation.

- level:

  Character, `"{prefix}={value}"` label.

- encoded:

  Integer, 1 if the observation matches this level, 0 otherwise.
