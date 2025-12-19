# Make project results

make_project_results() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make project
results. The function returns Sim results (a list).

## Usage

``` r
make_project_results(
  X_Ready4useDyad,
  inputs_ls,
  min_cell_size_1L_int = 30L,
  modifiable_chr = character(0),
  outcomes_chr = character(0),
  threshold_1L_dbl = 96000,
  timestamp_1L_chr = get_timestamp()
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- inputs_ls:

  Inputs (a list)

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

## Value

Sim results (a list)
