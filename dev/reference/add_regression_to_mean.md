# Add regression to mean

add_regression_to_mean() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add regression to mean. The function is
called for its side effects and does not return a value.

## Usage

``` r
add_regression_to_mean(
  X_Ready4useDyad,
  inputs_ls,
  iterations_int,
  k10_draws_fn,
  add_sensitivity_1L_lgl = FALSE,
  sensitivities_ls = make_sensitivities_ls(),
  tfmn_ls = make_class_tfmns(),
  tx_prefix_1L_chr = "Treatment",
  utilities_chr = c("AQoL8D", "EQ5D", "EQ5DM2", "SF6D", "SF6DM2"),
  utility_fns_ls = make_utility_fns_ls(utilities_chr = utilities_chr)
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- inputs_ls:

  Inputs (a list)

- iterations_int:

  Iterations (an integer vector)

- k10_draws_fn:

  K10 draws (a function)

- add_sensitivity_1L_lgl:

  Add sensitivity (a logical vector of length one), Default: FALSE

- sensitivities_ls:

  Sensitivities (a list), Default: make_sensitivities_ls()

- tfmn_ls:

  Transformation (a list), Default: make_class_tfmns()

- tx_prefix_1L_chr:

  Treatment prefix (a character vector of length one), Default:
  'Treatment'

- utilities_chr:

  Utilities (a character vector), Default: c("AQoL8D", "EQ5D", "EQ5DM2",
  "SF6D", "SF6DM2")

- utility_fns_ls:

  Utility functions (a list), Default: make_utility_fns_ls(utilities_chr
  = utilities_chr)

## Value

X (A dataset and data dictionary pair.)
