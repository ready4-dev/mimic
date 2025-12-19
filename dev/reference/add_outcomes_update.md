# Add outcomes update

add_outcomes_update() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add outcomes update. The function is called
for its side effects and does not return a value.

## Usage

``` r
add_outcomes_update(
  X_Ready4useDyad,
  assert_1L_lgl,
  k10_mdl,
  k10_var_1L_chr,
  iterations_int,
  params_tb,
  sensitivities_ls,
  tfmn_ls,
  tx_prefix_1L_chr,
  update_1L_int,
  utilities_chr,
  utility_fns_ls,
  types_chr = c("Model", "Function")
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- assert_1L_lgl:

  Assert (a logical vector of length one)

- k10_mdl:

  K10 (a model)

- k10_var_1L_chr:

  K10 variable (a character vector of length one)

- iterations_int:

  Iterations (an integer vector)

- params_tb:

  Parameters (a tibble)

- sensitivities_ls:

  Sensitivities (a list)

- tfmn_ls:

  Transformation (a list)

- tx_prefix_1L_chr:

  Treatment prefix (a character vector of length one)

- update_1L_int:

  Update (an integer vector of length one)

- utilities_chr:

  Utilities (a character vector)

- utility_fns_ls:

  Utility functions (a list)

- types_chr:

  Types (a character vector), Default: c("Model", "Function")

## Value

X (A dataset and data dictionary pair.)
