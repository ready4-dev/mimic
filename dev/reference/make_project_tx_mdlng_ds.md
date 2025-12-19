# Make project treatment modelling dataset

make_project_tx_mdlng_ds() is a Make function that creates a new R
object. Specifically, this function implements an algorithm to make
project treatment modelling dataset. The function is called for its side
effects and does not return a value.

## Usage

``` r
make_project_tx_mdlng_ds(
  X_Ready4useDyad,
  Y_Ready4useDyad,
  Z_Ready4useDyad,
  periods_chr = c(x = "-2 to 0 Weeks", y = "0 to 12 Weeks", z = "12 to 24 Weeks"),
  periods_ls = list(x = c(-2, 0), y = c(0, 12), z = c(12, 24)),
  treatment_vals_chr = c("Waitlist", "Treatment", "Discharged"),
  treatment_vars_chr = c("treatment_status", "treatment_status_t2")
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- Y_Ready4useDyad:

  PARAM_DESCRIPTION

- Z_Ready4useDyad:

  PARAM_DESCRIPTION

- periods_chr:

  Periods (a character vector), Default: c(x = "-2 to 0 Weeks", y = "0
  to 12 Weeks", z = "12 to 24 Weeks")

- periods_ls:

  Periods (a list), Default: list(x = c(-2, 0), y = c(0, 12), z = c(12,
  24))

- treatment_vals_chr:

  Treatment values (a character vector), Default: c("Waitlist",
  "Treatment", "Discharged")

- treatment_vars_chr:

  Treatment variables (a character vector), Default:
  c("treatment_status", "treatment_status_t2")

## Value

A (A dataset and data dictionary pair.)
