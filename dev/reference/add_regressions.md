# Add regressions

add_regressions() is an Add function that updates an object by adding
new values to new or empty fields. Specifically, this function
implements an algorithm to add regressions. The function returns
Regressions (a list).

## Usage

``` r
add_regressions(
  regressions_ls,
  what_1L_chr,
  model_1L_int = integer(0),
  fn_args_ls = list(),
  model_fn = NULL,
  named_1L_lgl = FALSE,
  X_Ready4useDyad = ready4use::Ready4useDyad(),
  type_1L_chr = c("candidates", "tests", "models")
)
```

## Arguments

- regressions_ls:

  Regressions (a list)

- what_1L_chr:

  What (a character vector of length one)

- model_1L_int:

  Model (an integer vector of length one), Default: integer(0)

- fn_args_ls:

  Function arguments (a list), Default: list()

- model_fn:

  Model (a function), Default: NULL

- named_1L_lgl:

  Named (a logical vector of length one), Default: FALSE

- X_Ready4useDyad:

  PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()

- type_1L_chr:

  Type (a character vector of length one), Default: c("candidates",
  "tests", "models")

## Value

Regressions (a list)
