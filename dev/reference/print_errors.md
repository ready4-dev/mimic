# Print errors

print_errors() is a Print function that prints output to console.
Specifically, this function implements an algorithm to print errors. The
function is called for its side effects and does not return a value.

## Usage

``` r
print_errors(
  X_Ready4useDyad,
  vars_chr,
  assert_1L_lgl = FALSE,
  invalid_fn = function(x) (is.na(x) | is.nan(x) | is.null(x) | x == -Inf | x == Inf)
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- vars_chr:

  Variables (a character vector)

- assert_1L_lgl:

  Assert (a logical vector of length one), Default: FALSE

- invalid_fn:

  Invalid (a function), Default: function(x) (is.na(x) \| is.nan(x) \|
  is.null(x) \| x == -Inf \| x == Inf)

## Value

No return value, called for side effects.
