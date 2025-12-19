# Add episode

add_episode() is an Add function that updates an object by adding new
values to new or empty fields. Specifically, this function implements an
algorithm to add episode. The function is called for its side effects
and does not return a value.

## Usage

``` r
add_episode(
  X_Ready4useDyad,
  assert_1L_lgl,
  episode_1L_int,
  inputs_ls,
  iterations_int,
  sensitivities_ls,
  tfmn_ls,
  tx_prefix_1L_chr,
  utilities_chr,
  utility_fns_ls,
  episode_end_1L_chr = "EpisodeEnd_mdl",
  k10_1L_chr = "K10_mdl",
  k10_relapse_1L_chr = "K10Relapse_mdl",
  k10_var_1L_chr = "K10",
  medical_chr = make_worker_types("medical"),
  treatment_1L_chr = character(0),
  workers_chr = make_worker_types()
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- assert_1L_lgl:

  Assert (a logical vector of length one)

- episode_1L_int:

  Episode (an integer vector of length one)

- inputs_ls:

  Inputs (a list)

- iterations_int:

  Iterations (an integer vector)

- sensitivities_ls:

  Sensitivities (a list)

- tfmn_ls:

  Transformation (a list)

- tx_prefix_1L_chr:

  Treatment prefix (a character vector of length one)

- utilities_chr:

  Utilities (a character vector)

- utility_fns_ls:

  Utility functions (a list)

- episode_end_1L_chr:

  Episode end (a character vector of length one), Default:
  'EpisodeEnd_mdl'

- k10_1L_chr:

  K10 (a character vector of length one), Default: 'K10_mdl'

- k10_relapse_1L_chr:

  K10 relapse (a character vector of length one), Default:
  'K10Relapse_mdl'

- k10_var_1L_chr:

  K10 variable (a character vector of length one), Default: 'K10'

- medical_chr:

  Medical (a character vector), Default: make_worker_types("medical")

- treatment_1L_chr:

  Treatment (a character vector of length one), Default: character(0)

- workers_chr:

  Workers (a character vector), Default: make_worker_types()

## Value

X (A dataset and data dictionary pair.)
