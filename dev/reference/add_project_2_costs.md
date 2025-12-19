# Add project 2 costs

add_project_2_costs() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add project 2 costs. The function is called
for its side effects and does not return a value.

## Usage

``` r
add_project_2_costs(
  X_Ready4useDyad,
  arms_for_intervention_costs_chr,
  arms_for_offsets_chr = character(0),
  disciplines_chr = make_disciplines(),
  intervention_1L_chr = "Intervention",
  sensitivities_ls = make_project_2_sensitivities_ls(),
  total_1L_lgl = T
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- arms_for_intervention_costs_chr:

  Arms for intervention costs (a character vector)

- arms_for_offsets_chr:

  Arms for offsets (a character vector), Default: character(0)

- disciplines_chr:

  Disciplines (a character vector), Default: make_disciplines()

- intervention_1L_chr:

  Intervention (a character vector of length one), Default:
  'Intervention'

- sensitivities_ls:

  Sensitivities (a list), Default: make_project_2_sensitivities_ls()

- total_1L_lgl:

  Total (a logical vector of length one), Default: T

## Value

X (A dataset and data dictionary pair.)
