# Make project 2 report

make_project_2_report() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make project 2
report. The function returns Data (an output object of multiple
potential types).

## Usage

``` r
make_project_2_report(
  model_data_ls = NULL,
  arms_chr,
  params_tb,
  period_dtm = lubridate::years(1),
  platform_1L_chr = "Intervention",
  processed_ls = NULL,
  regressions_ls = NULL,
  select_1L_chr = character(0),
  sim_results_ls = NULL,
  timepoint_1L_chr = character(0),
  timestamp_1L_chr = get_timestamp(),
  transformations_chr = character(0),
  type_1L_chr = "full_combos",
  ungroup_1L_lgl = FALSE,
  weeks_int = integer(0),
  what_1L_chr = c("paramscost", "paramsk10", "resultsaqol", "resultseq5d",
    "resultseconomic", "resultsoutcomes", "resultsutility", "serviceuse",
    "serviceusecost")
)
```

## Arguments

- model_data_ls:

  Model data (a list), Default: NULL

- arms_chr:

  Arms (a character vector)

- params_tb:

  Parameters (a tibble)

- period_dtm:

  Period (a date vector), Default: lubridate::years(1)

- platform_1L_chr:

  Platform (a character vector of length one), Default: 'Intervention'

- processed_ls:

  Processed (a list), Default: NULL

- regressions_ls:

  Regressions (a list), Default: NULL

- select_1L_chr:

  Select (a character vector of length one), Default: character(0)

- sim_results_ls:

  Sim results (a list), Default: NULL

- timepoint_1L_chr:

  Timepoint (a character vector of length one), Default: character(0)

- timestamp_1L_chr:

  Timestamp (a character vector of length one), Default: get_timestamp()

- transformations_chr:

  Transformations (a character vector), Default: character(0)

- type_1L_chr:

  Type (a character vector of length one), Default: 'full_combos'

- ungroup_1L_lgl:

  Ungroup (a logical vector of length one), Default: FALSE

- weeks_int:

  Weeks (an integer vector), Default: integer(0)

- what_1L_chr:

  What (a character vector of length one), Default: c("paramscost",
  "paramsk10", "resultsaqol", "resultseq5d", "resultseconomic",
  "resultsoutcomes", "resultsutility", "serviceuse", "serviceusecost")

## Value

Data (an output object of multiple potential types)
