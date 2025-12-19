# Make project 2 variables

make_project_2_vars() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make project 2
variables. The function returns Variables (a character vector).

## Usage

``` r
make_project_2_vars(
  type_1L_chr = c("drop", "clinical", "keep", "modify", "outcome", "utility"),
  disciplines_chr = make_disciplines("semi"),
  exclude_chr = character(0)
)
```

## Arguments

- type_1L_chr:

  Type (a character vector of length one), Default: c("drop",
  "clinical", "keep", "modify", "outcome", "utility")

- disciplines_chr:

  Disciplines (a character vector), Default: make_disciplines("semi")

- exclude_chr:

  Exclude (a character vector), Default: character(0)

## Value

Variables (a character vector)
