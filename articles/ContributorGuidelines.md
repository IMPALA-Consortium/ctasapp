# Contributor Guidelines

## Branches

The core branches used in this repository are:

- `main`: Contains the production version of the package (protected).
- `{issue#}`: Contains the code for a specific issue, named with the
  issue number as a prefix.

## Development Process

1.  Describe the development step in a GitHub issue.
2.  Create a new branch from `main` named with the issue number as a
    prefix (e.g. `42-add-pca-tab`).
3.  Use **test-driven development (TDD)**: write the test first, then
    write the code to pass the test.
4.  All GitHub Actions must pass before merging the branch to `main`.

## Code Style

- Code is formatted with [{styler}](https://styler.r-lib.org/) before
  each release.
- You can trigger [styler](https://github.com/r-lib/styler) on a PR by
  commenting `/style` (available to repository members).

## Documentation

- All exported functions must have
  [roxygen2](https://roxygen2.r-lib.org/) documentation.
- You can regenerate documentation on a PR by commenting `/document`
  (available to repository members).
- Roxygen import tags are centralized in `R/ctasapp-pkg.R`.
