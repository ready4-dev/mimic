# Make confusion list

make_confusion_ls() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make confusion
list. The function returns Confusion (a list).

## Usage

``` r
make_confusion_ls(
  regressions_ls,
  X_Ready4useDyad,
  var_1L_chr,
  high_1L_chr = "#2E86C1",
  low_1L_chr = "#D6EAF8",
  model_1L_int = integer(0),
  named_1L_lgl = FALSE,
  part_1L_int = integer(0),
  plot_1L_lgl = FALSE,
  tfmn_fn = identity,
  tfmn_args_ls = NULL,
  type_1L_chr = c("candidates", "tests", "models"),
  what_1L_chr = c("AQoL6D", "CHU9D", "K10", "Minutes", "Treatments", "Tx_Waitlist",
    "Tx_Treatment", "Tx_Discharged")
)
```

## Arguments

- regressions_ls:

  Regressions (a list)

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- var_1L_chr:

  Variable (a character vector of length one)

- high_1L_chr:

  High (a character vector of length one), Default: '#2E86C1'

- low_1L_chr:

  Low (a character vector of length one), Default: '#D6EAF8'

- model_1L_int:

  Model (an integer vector of length one), Default: integer(0)

- named_1L_lgl:

  Named (a logical vector of length one), Default: FALSE

- part_1L_int:

  Part (an integer vector of length one), Default: integer(0)

- plot_1L_lgl:

  Plot (a logical vector of length one), Default: FALSE

- tfmn_fn:

  Transformation (a function), Default: identity

- tfmn_args_ls:

  Transformation arguments (a list), Default: NULL

- type_1L_chr:

  Type (a character vector of length one), Default: c("candidates",
  "tests", "models")

- what_1L_chr:

  What (a character vector of length one), Default: c("AQoL6D", "CHU9D",
  "K10", "Minutes", "Treatments", "Tx_Waitlist", "Tx_Treatment",
  "Tx_Discharged")

## Value

Confusion (a list)
