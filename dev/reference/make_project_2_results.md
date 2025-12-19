# Make project 2 results

make_project_2_results() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make project 2
results. The function returns Sim results (a list).

## Usage

``` r
make_project_2_results(
  X_Ready4useDyad,
  inputs_ls,
  comparator_1L_chr = "Comparator",
  intervention_1L_chr = "Intervention",
  min_cell_size_1L_int = 30L,
  modifiable_chr = character(0),
  outcomes_chr = character(0),
  threshold_1L_dbl = 96000,
  timestamp_1L_chr = get_timestamp(),
  utilities_chr = c("AQoL8D", "EQ5D", "EQ5DM2", "SF6D", "SF6DM2")
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- inputs_ls:

  Inputs (a list)

- comparator_1L_chr:

  Comparator (a character vector of length one), Default: 'Comparator'

- intervention_1L_chr:

  Intervention (a character vector of length one), Default:
  'Intervention'

- min_cell_size_1L_int:

  Minimum cell size (an integer vector of length one), Default: 30

- modifiable_chr:

  Modifiable (a character vector), Default: character(0)

- outcomes_chr:

  Outcomes (a character vector), Default: character(0)

- threshold_1L_dbl:

  Threshold (a double vector of length one), Default: 96000

- timestamp_1L_chr:

  Timestamp (a character vector of length one), Default: get_timestamp()

- utilities_chr:

  Utilities (a character vector), Default: c("AQoL8D", "EQ5D", "EQ5DM2",
  "SF6D", "SF6DM2")

## Value

Sim results (a list)
