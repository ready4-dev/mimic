# Make project results synthesis

make_project_results_synthesis() is a Make function that creates a new R
object. Specifically, this function implements an algorithm to make
project results synthesis. The function is called for its side effects
and does not return a value.

## Usage

``` r
make_project_results_synthesis(
  inputs_ls,
  results_ls,
  comparator_1L_chr = "Comparator",
  drop_chr = make_project_2_vars("drop"),
  exclude_chr = character(0),
  exclude_suffixes_chr = c("_change", "_date", "_previous"),
  keep_chr = make_project_2_vars("keep"),
  intervention_1L_chr = "Intervention",
  modifiable_chr = make_project_2_vars("modify"),
  stratification_fn = identity,
  type_1L_chr = c("D", "AB", "C"),
  uid_tfmn_fn = as.numeric
)
```

## Arguments

- inputs_ls:

  Inputs (a list)

- results_ls:

  Results (a list)

- comparator_1L_chr:

  Comparator (a character vector of length one), Default: 'Comparator'

- drop_chr:

  Drop (a character vector), Default: make_project_2_vars("drop")

- exclude_chr:

  Exclude (a character vector), Default: character(0)

- exclude_suffixes_chr:

  Exclude suffixes (a character vector), Default: c("\_change",
  "\_date", "\_previous")

- keep_chr:

  Keep (a character vector), Default: make_project_2_vars("keep")

- intervention_1L_chr:

  Intervention (a character vector of length one), Default:
  'Intervention'

- modifiable_chr:

  Modifiable (a character vector), Default:
  make_project_2_vars("modify")

- stratification_fn:

  Stratification (a function), Default: identity

- type_1L_chr:

  Type (a character vector of length one), Default: c("D", "AB", "C")

- uid_tfmn_fn:

  Unique identifier transformation (a function), Default: as.numeric

## Value

X (A dataset and data dictionary pair.)
