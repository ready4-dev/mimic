# Make results summary

make_results_summary() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make results
summary. The function returns Results summary (a list).

## Usage

``` r
make_results_summary(
  X_Ready4useDyad,
  outcomes_chr,
  group_by_chr = character(0),
  min_cell_size_1L_int = 1L,
  threshold_1L_dbl = 96000,
  timestamp_1L_chr = get_timestamp(),
  utilities_chr = c("AQoL6D", "CHU9D")
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- outcomes_chr:

  Outcomes (a character vector)

- group_by_chr:

  Group by (a character vector), Default: character(0)

- min_cell_size_1L_int:

  Minimum cell size (an integer vector of length one), Default: 1

- threshold_1L_dbl:

  Threshold (a double vector of length one), Default: 96000

- timestamp_1L_chr:

  Timestamp (a character vector of length one), Default: get_timestamp()

- utilities_chr:

  Utilities (a character vector), Default: c("AQoL6D", "CHU9D")

## Value

Results summary (a list)
