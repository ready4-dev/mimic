# Add project 2 model wrap up

add_project_2_model_wrap_up() is an Add function that updates an object
by adding new values to new or empty fields. Specifically, this function
implements an algorithm to add project 2 model wrap up. The function is
called for its side effects and does not return a value.

## Usage

``` r
add_project_2_model_wrap_up(
  X_Ready4useDyad,
  arms_for_intervention_costs_chr,
  arms_for_offsets_chr = character(0),
  defaults_ls = NULL,
  disciplines_chr,
  inputs_ls,
  iterations_int,
  outcome_1L_chr = "K10",
  sensitivities_ls,
  tfmn_ls,
  tx_prefix_1L_chr,
  update_1L_int = 2,
  utilities_chr,
  utility_fns_ls
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- arms_for_intervention_costs_chr:

  Arms for intervention costs (a character vector)

- arms_for_offsets_chr:

  Arms for offsets (a character vector), Default: character(0)

- defaults_ls:

  Defaults (a list), Default: NULL

- disciplines_chr:

  Disciplines (a character vector)

- inputs_ls:

  Inputs (a list)

- iterations_int:

  Iterations (an integer vector)

- outcome_1L_chr:

  Outcome (a character vector of length one), Default: 'K10'

- sensitivities_ls:

  Sensitivities (a list)

- tfmn_ls:

  Transformation (a list)

- tx_prefix_1L_chr:

  Treatment prefix (a character vector of length one)

- update_1L_int:

  Update (an integer vector of length one), Default: 2

- utilities_chr:

  Utilities (a character vector)

- utility_fns_ls:

  Utility functions (a list)

## Value

X (A dataset and data dictionary pair.)
