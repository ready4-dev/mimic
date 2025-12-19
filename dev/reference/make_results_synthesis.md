# Make results synthesis

make_results_synthesis() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make results
synthesis. The function is called for its side effects and does not
return a value.

## Usage

``` r
make_results_synthesis(
  X_Ready4useDyad,
  add_severity_1L_lgl = TRUE,
  exclude_chr = character(0),
  exclude_suffixes_chr = c("_change", "_date", "_previous"),
  keep_chr = character(0),
  modifiable_chr = character(0),
  results_ls = NULL,
  stratification_fn = identity,
  type_1L_chr = c("D", "AB", "C"),
  Y_Ready4useDyad = ready4use::Ready4useDyad(),
  Z_Ready4useDyad = ready4use::Ready4useDyad()
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- add_severity_1L_lgl:

  Add severity (a logical vector of length one), Default: TRUE

- exclude_chr:

  Exclude (a character vector), Default: character(0)

- exclude_suffixes_chr:

  Exclude suffixes (a character vector), Default: c("\_change",
  "\_date", "\_previous")

- keep_chr:

  Keep (a character vector), Default: character(0)

- modifiable_chr:

  Modifiable (a character vector), Default: character(0)

- results_ls:

  Results (a list), Default: NULL

- stratification_fn:

  Stratification (a function), Default: identity

- type_1L_chr:

  Type (a character vector of length one), Default: c("D", "AB", "C")

- Y_Ready4useDyad:

  PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()

- Z_Ready4useDyad:

  PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()

## Value

A (A dataset and data dictionary pair.)
