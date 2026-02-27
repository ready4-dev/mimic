# Make project 2 regression to mean

make_project_2_regression_to_mean() is a Make function that creates a
new R object. Specifically, this function implements an algorithm to
make project 2 regression to mean. The function is called for its side
effects and does not return a value.

## Usage

``` r
make_project_2_regression_to_mean(
  event_nm_1L_chr = "RegressionToMean",
  functions_ls = make_ineligibility_fns_ls(),
  ineligible_1L_chr = character(0),
  outcome_var_1L_chr = "K10",
  use_schedule_1L_chr = "Y",
  use_trigger_1L_chr = "Z"
)
```

## Arguments

- event_nm_1L_chr:

  Event name (a character vector of length one), Default:
  'RegressionToMean'

- functions_ls:

  Functions (a list), Default: make_ineligibility_fns_ls()

- ineligible_1L_chr:

  Ineligible (a character vector of length one), Default: character(0)

- outcome_var_1L_chr:

  Outcome variable (a character vector of length one), Default: 'K10'

- use_schedule_1L_chr:

  Use schedule (a character vector of length one), Default: 'Y'

- use_trigger_1L_chr:

  Use trigger (a character vector of length one), Default: 'Z'

## Value

X (Model event scheduling and event logic data.)
