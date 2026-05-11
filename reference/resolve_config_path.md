# Resolve a path from a config file

Expands `~`, then resolves relative paths against `base_dir` (the
directory containing the active config file). Absolute paths are
returned as-is (after expansion).

## Usage

``` r
resolve_config_path(path, base_dir)
```

## Arguments

- path:

  Character scalar path from the config file.

- base_dir:

  Directory of the active config file.

## Value

Character scalar resolved path.
