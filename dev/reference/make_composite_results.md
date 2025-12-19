# Make composite results

make_composite_results() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make composite
results. The function is called for its side effects and does not return
a value.

## Usage

``` r
make_composite_results(
  X_Ready4useDyad,
  Y_Ready4useDyad,
  Z_Ready4useDyad,
  exclude_chr = character(0),
  exclude_suffixes_chr = character(0),
  keep_chr = character(0),
  modifiable_chr = character(0),
  start_suffix_1L_chr = "_start",
  type_1L_chr = c("AB", "C", "D")
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- Y_Ready4useDyad:

  PARAM_DESCRIPTION

- Z_Ready4useDyad:

  PARAM_DESCRIPTION

- exclude_chr:

  Exclude (a character vector), Default: character(0)

- exclude_suffixes_chr:

  Exclude suffixes (a character vector), Default: character(0)

- keep_chr:

  Keep (a character vector), Default: character(0)

- modifiable_chr:

  Modifiable (a character vector), Default: character(0)

- start_suffix_1L_chr:

  Start suffix (a character vector of length one), Default: '\_start'

- type_1L_chr:

  Type (a character vector of length one), Default: c("AB", "C", "D")

## Value

A (A dataset and data dictionary pair.)
