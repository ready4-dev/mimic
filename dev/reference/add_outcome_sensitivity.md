# Add outcome sensitivity

add_outcome_sensitivity() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add outcome sensitivity. The function is
called for its side effects and does not return a value.

## Usage

``` r
add_outcome_sensitivity(
  X_Ready4useDyad,
  outcome_1L_chr,
  sensitivities_ls = make_sensitivities_ls(),
  tfmn_fn = NULL,
  tfmn_ls = make_class_tfmns(T)
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- outcome_1L_chr:

  Outcome (a character vector of length one)

- sensitivities_ls:

  Sensitivities (a list), Default: make_sensitivities_ls()

- tfmn_fn:

  Transformation (a function), Default: NULL

- tfmn_ls:

  Transformation (a list), Default: make_class_tfmns(T)

## Value

X (A dataset and data dictionary pair.)
