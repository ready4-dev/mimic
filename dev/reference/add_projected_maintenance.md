# Add projected maintenance

add_projected_maintenance() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add projected maintenance. The function is
called for its side effects and does not return a value.

## Usage

``` r
add_projected_maintenance(
  X_Ready4useDyad,
  outcome_1L_chr,
  suffix_1L_chr,
  tfmn_fn = identity,
  ...
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- outcome_1L_chr:

  Outcome (a character vector of length one)

- suffix_1L_chr:

  Suffix (a character vector of length one)

- tfmn_fn:

  Transformation (a function), Default: identity

- ...:

  Additional arguments

## Value

X (A dataset and data dictionary pair.)
