# Make outcomes variables

make_outcomes_vars() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make outcomes
variables. The function returns Outcomes (a character vector).

## Usage

``` r
make_outcomes_vars(
  X_Ready4useDyad,
  Y_Ready4useDyad,
  Z_Ready4useDyad,
  exclude_chr = character(0),
  exclude_suffixes_chr = character(0),
  modifiable_chr = character(0),
  numeric_only_1L_lgl = FALSE
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

- modifiable_chr:

  Modifiable (a character vector), Default: character(0)

- numeric_only_1L_lgl:

  Numeric only (a logical vector of length one), Default: FALSE

## Value

Outcomes (a character vector)
