# Get regression

get_regression() is a Get function that extracts data from an object.
Specifically, this function implements an algorithm to get regression.
The function returns Model (an output object of multiple potential
types).

## Usage

``` r
get_regression(
  regressions_ls,
  what_1L_chr,
  constrained_1L_lgl = logical(0),
  model_1L_int = integer(0),
  named_1L_lgl = FALSE,
  part_1L_int = integer(0),
  report_1L_chr = c("all", "check", "compare", "confusion", "density", "estimates",
    "histogram", "scatter", "test"),
  type_1L_chr = c("candidates", "assessments", "models", "tests")
)
```

## Arguments

- regressions_ls:

  Regressions (a list)

- what_1L_chr:

  What (a character vector of length one)

- constrained_1L_lgl:

  Constrained (a logical vector of length one), Default: logical(0)

- model_1L_int:

  Model (an integer vector of length one), Default: integer(0)

- named_1L_lgl:

  Named (a logical vector of length one), Default: FALSE

- part_1L_int:

  Part (an integer vector of length one), Default: integer(0)

- report_1L_chr:

  Report (a character vector of length one), Default: c("all", "check",
  "compare", "confusion", "density", "estimates", "histogram",
  "scatter", "test")

- type_1L_chr:

  Type (a character vector of length one), Default: c("candidates",
  "assessments", "models", "tests")

## Value

Model (an output object of multiple potential types)
