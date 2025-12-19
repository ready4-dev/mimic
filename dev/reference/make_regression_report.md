# Make regression report

make_regression_report() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make regression
report. The function returns Report (an output object of multiple
potential types).

## Usage

``` r
make_regression_report(
  regressions_ls,
  what_1L_chr,
  colours_chr = character(0),
  digits_1L_int = integer(0),
  drop_chr = character(0),
  exclude_int = integer(0),
  model_1L_int = integer(0),
  part_1L_int = integer(0),
  report_1L_chr = c("all", "main", "check", "compare", "confusion", "estimates", "test"),
  rank_1L_lgl = TRUE,
  residual_1L_chr = "normal",
  type_1L_chr = c("candidates", "tests", "models"),
  var_1L_chr = character(0),
  X_Ready4useDyad = ready4use::Ready4useDyad()
)
```

## Arguments

- regressions_ls:

  Regressions (a list)

- what_1L_chr:

  What (a character vector of length one)

- colours_chr:

  Colours (a character vector), Default: character(0)

- digits_1L_int:

  Digits (an integer vector of length one), Default: integer(0)

- drop_chr:

  Drop (a character vector), Default: character(0)

- exclude_int:

  Exclude (an integer vector), Default: integer(0)

- model_1L_int:

  Model (an integer vector of length one), Default: integer(0)

- part_1L_int:

  Part (an integer vector of length one), Default: integer(0)

- report_1L_chr:

  Report (a character vector of length one), Default: c("all", "main",
  "check", "compare", "confusion", "estimates", "test")

- rank_1L_lgl:

  Rank (a logical vector of length one), Default: TRUE

- residual_1L_chr:

  Residual (a character vector of length one), Default: 'normal'

- type_1L_chr:

  Type (a character vector of length one), Default: c("candidates",
  "tests", "models")

- var_1L_chr:

  Variable (a character vector of length one), Default: character(0)

- X_Ready4useDyad:

  PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()

## Value

Report (an output object of multiple potential types)
