# Make report data

make_report_data() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make report data.
The function returns Data (an output object of multiple potential
types).

## Usage

``` r
make_report_data(
  model_data_ls = NULL,
  date_end_dtm = as.POSIXct("2023-07-01"),
  date_start_dtm = as.POSIXct("2023-06-01"),
  period_dtm = lubridate::years(1),
  platform_1L_chr = "Intervention",
  processed_ls = NULL,
  regressions_ls = NULL,
  sim_results_ls = NULL,
  timepoint_1L_chr = character(0),
  timestamp_1L_chr = get_timestamp(),
  transformations_chr = character(0),
  type_1L_chr = "full_combos",
  ungroup_1L_lgl = FALSE,
  weeks_int = integer(0),
  what_1L_chr = c("descriptives", "composite", "costadj", "costitem", "costsum",
    "costunit", "minutes", "mnl-wait", "mnl-tx", "mnl-disc", "outcomes", "outcomeslong",
    "paramscost", "paramsk10", "resultsaqol", "resultschu", "resultsoutcomes",
    "resultseconomic", "serviceuse", "serviceusecost")
)
```

## Arguments

- model_data_ls:

  Model data (a list), Default: NULL

- date_end_dtm:

  Date end (a date vector), Default: as.POSIXct("2023-07-01")

- date_start_dtm:

  Date start (a date vector), Default: as.POSIXct("2023-06-01")

- period_dtm:

  Period (a date vector), Default: lubridate::years(1)

- platform_1L_chr:

  Platform (a character vector of length one), Default: 'Intervention'

- processed_ls:

  Processed (a list), Default: NULL

- regressions_ls:

  Regressions (a list), Default: NULL

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

  What (a character vector of length one), Default: c("descriptives",
  "composite", "costadj", "costitem", "costsum", "costunit", "minutes",
  "mnl-wait", "mnl-tx", "mnl-disc", "outcomes", "outcomeslong",
  "paramscost", "paramsk10", "resultsaqol", "resultschu",
  "resultsoutcomes", "resultseconomic", "serviceuse", "serviceusecost")

## Value

Data (an output object of multiple potential types)
