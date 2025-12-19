# Make project treatment models

make_project_tx_mdls() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make project
treatment models. The function returns Treatment models (a list).

## Usage

``` r
make_project_tx_mdls(
  X_Ready4useDyad,
  append_to_ls = list(),
  predictors_ls = NULL,
  treatment_vars_chr = c("treatment_status", "treatment_status_t2"),
  what_1L_chr = c("All", "Waitlist", "Treatment", "Discharged")
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- append_to_ls:

  Append to (a list), Default: list()

- predictors_ls:

  Predictors (a list), Default: NULL

- treatment_vars_chr:

  Treatment variables (a character vector), Default:
  c("treatment_status", "treatment_status_t2")

- what_1L_chr:

  What (a character vector of length one), Default: c("All", "Waitlist",
  "Treatment", "Discharged")

## Value

Treatment models (a list)
