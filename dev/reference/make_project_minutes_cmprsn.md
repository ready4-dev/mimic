# Make project minutes comparison

make_project_minutes_cmprsn() is a Make function that creates a new R
object. Specifically, this function implements an algorithm to make
project minutes comparison. The function returns Comparison (a tibble).

## Usage

``` r
make_project_minutes_cmprsn(
  X_Ready4useDyad,
  Y_Ready4useDyad,
  names_chr = character(0),
  type_1L_chr = c("dataset", "prediction"),
  var_1L_chr = character(0),
  weeks_chr = c("Week14", "Week53")
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- Y_Ready4useDyad:

  PARAM_DESCRIPTION

- names_chr:

  Names (a character vector), Default: character(0)

- type_1L_chr:

  Type (a character vector of length one), Default: c("dataset",
  "prediction")

- var_1L_chr:

  Variable (a character vector of length one), Default: character(0)

- weeks_chr:

  Weeks (a character vector), Default: c("Week14", "Week53")

## Value

Comparison (a tibble)
