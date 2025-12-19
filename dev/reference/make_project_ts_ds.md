# Make project time series dataset

make_project_ts_ds() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make project time
series dataset. The function is called for its side effects and does not
return a value.

## Usage

``` r
make_project_ts_ds(
  X_Ready4useDyad,
  processed_ls,
  index_1L_chr = "Date",
  key_vars_chr = make_project_keys(type_1L_chr = "ts")
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- processed_ls:

  Processed (a list)

- index_1L_chr:

  Index (a character vector of length one), Default: 'Date'

- key_vars_chr:

  Key variables (a character vector), Default:
  make_project_keys(type_1L_chr = "ts")

## Value

Y (A dataset and data dictionary pair.)
