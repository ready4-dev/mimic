# Add K10 scores

add_k10_scores() is an Add function that updates an object by adding new
values to new or empty fields. Specifically, this function implements an
algorithm to add k10 scores. The function is called for its side effects
and does not return a value.

## Usage

``` r
add_k10_scores(
  X_Ready4useDyad,
  k10_mdl = NULL,
  iterations_int = 1:100L,
  join_with_chr = character(0),
  k10_draws_fn = add_project_1_k10_draws,
  k10_var_1L_chr = "k10",
  params_tb = make_project_params_tb(),
  sensitivities_ls = make_sensitivities_ls(),
  tfmn_ls = make_class_tfmns(T),
  tx_prefix_1L_chr = "treatment",
  type_1L_chr = c("Model", "Project", "Table"),
  var_1L_chr = "k10_12_Weeks",
  what_1L_chr = c("old", "new")
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- k10_mdl:

  K10 (a model), Default: NULL

- iterations_int:

  Iterations (an integer vector), Default: 1:100L

- join_with_chr:

  Join with (a character vector), Default: character(0)

- k10_draws_fn:

  K10 draws (a function), Default: add_project_1_k10_draws

- k10_var_1L_chr:

  K10 variable (a character vector of length one), Default: 'k10'

- params_tb:

  Parameters (a tibble), Default: make_project_params_tb()

- sensitivities_ls:

  Sensitivities (a list), Default: make_sensitivities_ls()

- tfmn_ls:

  Transformation (a list), Default: make_class_tfmns(T)

- tx_prefix_1L_chr:

  Treatment prefix (a character vector of length one), Default:
  'treatment'

- type_1L_chr:

  Type (a character vector of length one), Default: c("Model",
  "Project", "Table")

- var_1L_chr:

  Variable (a character vector of length one), Default: 'k10_12_Weeks'

- what_1L_chr:

  What (a character vector of length one), Default: c("old", "new")

## Value

X (A dataset and data dictionary pair.)
