# Add severity cuts

add_severity_cuts() is an Add function that updates an object by adding
new values to new or empty fields. Specifically, this function
implements an algorithm to add severity cuts. The function is called for
its side effects and does not return a value.

## Usage

``` r
add_severity_cuts(
  X_Ready4useDyad,
  add_severity_1L_lgl = TRUE,
  severity_fn = make_k10_severity_cuts,
  severity_var_1L_chr = "k10_start"
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- add_severity_1L_lgl:

  Add severity (a logical vector of length one), Default: TRUE

- severity_fn:

  Severity (a function), Default: make_k10_severity_cuts

- severity_var_1L_chr:

  Severity variable (a character vector of length one), Default:
  'k10_start'

## Value

X (A dataset and data dictionary pair.)
