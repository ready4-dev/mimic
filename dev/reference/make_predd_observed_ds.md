# Make predicted observed dataset

make_predd_observed_ds() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make predicted
observed dataset. The function is called for its side effects and does
not return a value.

## Usage

``` r
make_predd_observed_ds(
  X_Ready4useDyad,
  Y_Ready4useDyad,
  consolidate_1L_chr = character(0),
  join_with_chr = character(0),
  new_1L_chr = "Simulated",
  old_1L_chr = "Observed",
  select_chr = character(0),
  slim_1L_lgl = FALSE
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- Y_Ready4useDyad:

  PARAM_DESCRIPTION

- consolidate_1L_chr:

  Consolidate (a character vector of length one), Default: character(0)

- join_with_chr:

  Join with (a character vector), Default: character(0)

- new_1L_chr:

  New (a character vector of length one), Default: 'Simulated'

- old_1L_chr:

  Old (a character vector of length one), Default: 'Observed'

- select_chr:

  Select (a character vector), Default: character(0)

- slim_1L_lgl:

  Slim (a logical vector of length one), Default: FALSE

## Value

Y (A dataset and data dictionary pair.)
