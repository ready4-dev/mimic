# Transform to long results

transform_to_long_results() is a Transform function that edits an object
in such a way that core object attributes - e.g. shape, dimensions,
elements, type - are altered. Specifically, this function implements an
algorithm to transform to long results. The function is called for its
side effects and does not return a value.

## Usage

``` r
transform_to_long_results(
  X_Ready4useDyad,
  var_1L_chr,
  add_means_1L_lgl = TRUE,
  tidy_1L_lgl = TRUE
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- var_1L_chr:

  Variable (a character vector of length one)

- add_means_1L_lgl:

  Add means (a logical vector of length one), Default: TRUE

- tidy_1L_lgl:

  Tidy (a logical vector of length one), Default: TRUE

## Value

X (A dataset and data dictionary pair.)
