# Add project 2 cost sensitivity analysis 1

add_project_2_cost_sa_1() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add project 2 cost sensitivity analysis 1.
The function is called for its side effects and does not return a value.

## Usage

``` r
add_project_2_cost_sa_1(
  X_Ready4useDyad,
  arms_for_intervention_costs_chr = "Intervention",
  disciplines_chr = make_disciplines(),
  suffix_1L_chr = "_S1",
  ...
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- arms_for_intervention_costs_chr:

  Arms for intervention costs (a character vector), Default:
  'Intervention'

- disciplines_chr:

  Disciplines (a character vector), Default: make_disciplines()

- suffix_1L_chr:

  Suffix (a character vector of length one), Default: '\_S1'

- ...:

  Additional arguments

## Value

X (A dataset and data dictionary pair.)
