# Add ineligible

add_ineligible() is an Add function that updates an object by adding new
values to new or empty fields. Specifically, this function implements an
algorithm to add ineligible. The function is called for its side effects
and does not return a value.

## Usage

``` r
add_ineligible(
  X_Ready4useDyad,
  ineligible_1L_chr = character(0),
  post_fn = identity,
  pre_fn = identity,
  type_1L_chr = c("filter", "reset")
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- ineligible_1L_chr:

  Ineligible (a character vector of length one), Default: character(0)

- post_fn:

  Post (a function), Default: identity

- pre_fn:

  Pre (a function), Default: identity

- type_1L_chr:

  Type (a character vector of length one), Default: c("filter", "reset")

## Value

X (A dataset and data dictionary pair.)
