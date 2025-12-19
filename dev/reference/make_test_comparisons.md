# Make test comparisons

make_test_comparisons() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make test
comparisons. The function is called for its side effects and does not
return a value.

## Usage

``` r
make_test_comparisons(
  X_Ready4useDyad,
  Y_Ready4useDyad = ready4use::Ready4useDyad(),
  type_1L_chr = c("full", "wsummary", "lsummary")
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- Y_Ready4useDyad:

  PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()

- type_1L_chr:

  Type (a character vector of length one), Default: c("full",
  "wsummary", "lsummary")

## Value

Z (A dataset and data dictionary pair.)
