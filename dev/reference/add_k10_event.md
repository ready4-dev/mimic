# Add K10 event

add_k10_event() is an Add function that updates an object by adding new
values to new or empty fields. Specifically, this function implements an
algorithm to add k10 event. The function is called for its side effects
and does not return a value.

## Usage

``` r
add_k10_event(
  X_Ready4useDyad,
  adjustment_1L_dbl = 0,
  defaults_ls = list(Minutes = 0),
  k10_draws_fn = add_project_1_k10_draws,
  k10_mdl = NULL,
  k10_var_1L_chr = "k10",
  iterations_int = 1:100L,
  params_tb = make_project_params_tb(),
  sensitivities_ls = make_sensitivities_ls(),
  suffix_1L_chr = character(0),
  tfmn_ls = make_class_tfmns(T),
  type_1L_chr = c("Model", "Project", "Table"),
  tx_prefix_1L_chr = "treatment",
  update_1L_int = integer(0)
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- adjustment_1L_dbl:

  Adjustment (a double vector of length one), Default: 0

- defaults_ls:

  Defaults (a list), Default: list(Minutes = 0)

- k10_draws_fn:

  K10 draws (a function), Default: add_project_1_k10_draws

- k10_mdl:

  K10 (a model), Default: NULL

- k10_var_1L_chr:

  K10 variable (a character vector of length one), Default: 'k10'

- iterations_int:

  Iterations (an integer vector), Default: 1:100L

- params_tb:

  Parameters (a tibble), Default: make_project_params_tb()

- sensitivities_ls:

  Sensitivities (a list), Default: make_sensitivities_ls()

- suffix_1L_chr:

  Suffix (a character vector of length one), Default: character(0)

- tfmn_ls:

  Transformation (a list), Default: make_class_tfmns(T)

- type_1L_chr:

  Type (a character vector of length one), Default: c("Model",
  "Project", "Table")

- tx_prefix_1L_chr:

  Treatment prefix (a character vector of length one), Default:
  'treatment'

- update_1L_int:

  Update (an integer vector of length one), Default: integer(0)

## Value

X (A dataset and data dictionary pair.)
